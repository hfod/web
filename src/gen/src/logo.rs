use std::{ffi::OsString, fs, io::Cursor, path::Path};

use image::{ImageBuffer, Rgba};

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

pub fn object(cache_file_path: &Path) -> anyhow::Result<Obj> {
    let (data, ext) = if cache_file_path.try_exists()? {
        tracing::debug!("Logo reading.");
        let data = fs::read(&cache_file_path)?;
        let ext = cache_file_path
            .extension()
            .unwrap_or_else(|| unreachable!())
            .to_owned();
        (data, ext)
    } else {
        tracing::debug!("Logo building.");
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

#[tracing::instrument(name = "logo::build", skip_all)]
pub fn build() -> anyhow::Result<(Vec<u8>, OsString)> {
    let format: ImageFormat = ImageFormat::png();

    let rgba_transparent = Rgba([0, 0, 0, 0]);
    // let rgba_opaque_black = Rgba([0, 0, 0, 255]);
    let rgba_opaque_zenburn_normal_gray = Rgba([220, 220, 204, 255]);
    let bg = rgba_transparent;
    let fg = rgba_opaque_zenburn_normal_gray;

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

    let scale = 20;
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
