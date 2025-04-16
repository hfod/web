use std::{ffi::OsString, fs, io::Cursor, path::Path};

use anyhow::Context;
use image::{
    DynamicImage, GenericImage, ImageBuffer, ImageReader, RgbaImage,
    imageops::FilterType,
};
use rand::seq::SliceRandom;
use rayon::iter::{IntoParallelIterator, ParallelIterator};

use crate::data::obj::Obj;

// TODO We need an image abstraction module.

#[derive(Debug, Clone, Copy)]
struct ImageFormat(image::ImageFormat);

impl ImageFormat {
    fn png() -> Self {
        Self(image::ImageFormat::Png)
    }

    fn to_ext(&self) -> OsString {
        match self.0 {
            image::ImageFormat::Png => "png".into(),
            _ => unreachable!(),
        }
    }
}

pub fn object(
    cache_file_path: &Path,
    images: Vec<Vec<u8>>,
) -> anyhow::Result<Option<Obj>> {
    if images.is_empty() {
        Ok(None)
    } else {
        let (data, ext) = if cache_file_path.try_exists()? {
            tracing::debug!("Collage reading.");
            let data = fs::read(&cache_file_path)?;
            let ext = cache_file_path
                .extension()
                .unwrap_or_else(|| unreachable!())
                .to_owned();
            (data, ext)
        } else {
            tracing::debug!("Collage building.");
            if let Some(parent) = cache_file_path.parent() {
                fs::create_dir_all(parent)?;
            }
            let (data, ext) = build(images)?;
            fs::write(&cache_file_path, &data)?;
            (data, ext)
        };
        let obj = Obj::new(data, ext);
        Ok(Some(obj))
    }
}

// TODO Refactor.
#[tracing::instrument(name = "collage::build", skip_all)]
fn build(mut images: Vec<Vec<u8>>) -> anyhow::Result<(Vec<u8>, OsString)> {
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
        best_square_grid_fill_width(n, out_width, out_height);
    let collage_width = grid_cols * square_size;
    let collage_height = grid_rows * square_size;
    let mut collage: RgbaImage =
        ImageBuffer::new(collage_width, collage_height);

    let m = usize::try_from(grid_cols * grid_rows)?;
    let resized: Vec<(DynamicImage, u32, u32)> = (0..m)
        .into_par_iter()
        .map(|i| {
            let img = &images[i % n]; // Wrap around if not enough to fill the row.
            let img = ImageReader::new(Cursor::new(img))
                .with_guessed_format()
                .context("Failed to guess image format.")
                .and_then(|img_reader| {
                    img_reader.decode().context("Failed to decode image.")
                })
                .unwrap(); // FIXME Handle the error.
            let resized = img.resize_exact(square_size, square_size, filter);
            let col = (i as u32) % grid_cols;
            let row = (i as u32) / grid_cols;
            let x = col * square_size;
            let y = row * square_size;
            (resized, x, y)
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
fn best_square_grid_fill_width(
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
