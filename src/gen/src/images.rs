use std::{ffi::OsString, fs, io::Cursor, path::Path};

use anyhow::Context;
use image::{
    DynamicImage, GenericImage, ImageBuffer, ImageReader, Rgba, RgbaImage,
    imageops::FilterType,
};
use rand::seq::SliceRandom;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::data::obj::Obj;

// ============================================================================
// API
// ============================================================================

#[tracing::instrument]
pub fn logo(cache_file_path: &Path) -> anyhow::Result<Obj> {
    let format: ImageFormat = ImageFormat::png();
    let scale = 20;
    let fg = Rgba(RGBA_OPAQUE_ZENBURN_NORMAL_GRAY);
    cached(cache_file_path, || build_logo(format, scale, fg))
}

#[tracing::instrument]
pub fn icon(cache_file_path: &Path) -> anyhow::Result<Obj> {
    let format: ImageFormat = ImageFormat::png();
    let scale = 10;
    let fg = Rgba(RGBA_OPAQUE_BLACK);
    cached(cache_file_path, || build_logo(format, scale, fg))
}

#[tracing::instrument(skip(images))]
pub fn collage(
    cache_file_path: &Path,
    images: Vec<Vec<u8>>,
) -> anyhow::Result<Option<Obj>> {
    if images.is_empty() {
        Ok(None)
    } else {
        let obj = cached(cache_file_path, || build_collage(images))?;
        Ok(Some(obj))
    }
}

// ============================================================================
// Implementation
// ============================================================================

const RGBA_TRANSPARENT: [u8; 4] = [0, 0, 0, 0];
const RGBA_OPAQUE_BLACK: [u8; 4] = [0, 0, 0, 255];
const RGBA_OPAQUE_ZENBURN_NORMAL_GRAY: [u8; 4] = [220, 220, 204, 255];

#[derive(Debug, Clone, Copy)]
struct ImageFormat(image::ImageFormat);

impl ImageFormat {
    fn png() -> Self {
        Self(image::ImageFormat::Png)
    }

    // fn ico() -> Self {
    //     Self(image::ImageFormat::Ico)
    // }

    fn to_ext(&self) -> OsString {
        match self.0 {
            image::ImageFormat::Png => "png".into(),
            // image::ImageFormat::Ico => "ico".into(),
            _ => unreachable!(),
        }
    }
}

#[tracing::instrument(skip_all)]
fn cached<F>(cache_file_path: &Path, build: F) -> anyhow::Result<Obj>
where
    F: FnOnce() -> anyhow::Result<(Vec<u8>, OsString)>,
{
    let (data, ext) = if cache_file_path.try_exists()? {
        tracing::debug!("Reading from cache.");
        let data = fs::read(&cache_file_path)?;
        let ext = cache_file_path
            .extension()
            .unwrap_or_else(|| unreachable!())
            .to_owned();
        (data, ext)
    } else {
        tracing::debug!("Building.");
        if let Some(parent) = cache_file_path.parent() {
            fs::create_dir_all(parent)?;
        }
        let (data, ext) = build()?;
        fs::write(&cache_file_path, &data)?;
        (data, ext)
    };
    let obj = Obj::new(data, ext);
    Ok(obj)
}

#[tracing::instrument(skip_all)]
fn build_logo(
    format: ImageFormat,
    scale: usize,
    fg: Rgba<u8>,
) -> anyhow::Result<(Vec<u8>, OsString)> {
    let bg = Rgba(RGBA_TRANSPARENT);

    #[rustfmt::skip]
    let glider: [[Rgba<u8>; 3]; 3] = {
        let x = fg;
        let o = bg;
        [
            [o, x, o],
            [o, o, x],
            [x, x, x]
        ]
    };

    let cell_size = 2 * scale;
    let grid_thickness = 1 * scale;

    let cells = 3;
    let img_size = cells * cell_size + (cells + 1) * grid_thickness;
    let mut img =
        ImageBuffer::from_pixel(img_size as u32, img_size as u32, bg);

    for (y, row) in glider.iter().enumerate() {
        for (x, &value) in row.iter().enumerate() {
            let start_x = x * (cell_size + grid_thickness) + grid_thickness;
            let start_y = y * (cell_size + grid_thickness) + grid_thickness;

            for dy in 0..cell_size {
                for dx in 0..cell_size {
                    img.put_pixel(
                        (start_x + dx) as u32,
                        (start_y + dy) as u32,
                        value,
                    );
                }
            }
        }
    }

    let img = {
        let mut out = Vec::new();
        img.write_to(&mut Cursor::new(&mut out), format.0)?;
        out
    };

    Ok((img, format.to_ext()))
}

// TODO Refactor.
#[tracing::instrument(skip_all)]
fn build_collage(
    mut images: Vec<Vec<u8>>,
) -> anyhow::Result<(Vec<u8>, OsString)> {
    let out_width: u32 = 800;
    let out_height: u32 = 400;
    let format: ImageFormat = ImageFormat::png();
    let filter: FilterType = FilterType::Lanczos3;

    if let [img] = &images[..] {
        let img = ImageReader::new(Cursor::new(img))
            .with_guessed_format()
            .context("Failed to guess image format.")?;
        let img = img.decode().context("Failed to decode image.")?;
        let img = img.resize(out_width, out_height, filter);
        let mut out = Vec::new();
        img.write_to(&mut Cursor::new(&mut out), format.0)?;
        return Ok((out, format.to_ext()));
    }

    let mut rng = rand::rng();
    images.shuffle(&mut rng);

    let n = images.len();
    let (grid_cols, grid_rows, square_size) =
        collage_best_square_grid_fill_width(n, out_width, out_height);
    let collage_width = grid_cols * square_size;
    let collage_height = grid_rows * square_size;
    let mut collage: RgbaImage =
        ImageBuffer::new(collage_width, collage_height);

    let m = usize::try_from(grid_cols * grid_rows)?;
    let resized: Vec<(DynamicImage, u32, u32)> = (0..m)
        .into_par_iter()
        .filter_map(|i| {
            let img = &images[i % n]; // Wrap around if not enough to fill the row.
            let img = ImageReader::new(Cursor::new(img))
                .with_guessed_format()
                .context("Failed to guess image format.")
                .and_then(|img_reader| {
                    img_reader.decode().context("Failed to decode image.")
                })
                .inspect_err(|error| {
                    tracing::error!(?error, "Failed to init DynamicImage.");
                })
                .ok()?;
            let resized = img.resize_exact(square_size, square_size, filter);
            let col = (i as u32) % grid_cols;
            let row = (i as u32) / grid_cols;
            let x = col * square_size;
            let y = row * square_size;
            Some((resized, x, y))
        })
        .collect();
    for (resized, x, y) in resized {
        collage.copy_from(&resized, x, y)?;
    }

    let mut out = Vec::new();
    collage.write_to(&mut Cursor::new(&mut out), format.0)?;

    Ok((out, format.to_ext()))
}

//  Determine square size and grid layout.
fn collage_best_square_grid_fill_width(
    n: usize,
    out_width: u32,
    out_height: u32,
) -> (u32, u32, u32) {
    if n == 1 {
        return (1, 1, out_height);
    }

    let mut best_cols = 1;
    let mut best_rows = n as u32;
    let mut best_size = 0;

    for cols in 1..=n {
        let cols = cols as u32;
        let square_size = out_width / cols;
        let rows = ((n as f64) / (cols as f64)).ceil() as u32;
        let total_height = rows * square_size;

        if total_height <= out_height && square_size > best_size {
            best_size = square_size;
            best_cols = cols;
            best_rows = rows;
        }
    }
    (best_cols, best_rows, best_size)
}
