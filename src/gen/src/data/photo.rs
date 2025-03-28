use std::{fs, path::Path};

use anyhow::anyhow;

use crate::data::obj::Obj;

#[derive(Clone, Debug)]
pub struct Photo {
    pub obj_hash: String,
    pub caption: Option<String>,
}

impl Photo {
    pub fn from_file(
        photo_file_path: &Path,
    ) -> anyhow::Result<Option<(Self, Obj)>> {
        let data = fs::read(&photo_file_path)?;
        if !infer::is_image(&data[..]) {
            return Ok(None);
        }
        let ext = photo_file_path
            .extension()
            .ok_or(anyhow!(
                "Missing file extension in photo file: {photo_file_path:?}"
            ))?
            .to_owned();
        let obj = Obj::new(data, ext);
        let caption_file_path = photo_file_path.with_extension("txt");
        let caption = if caption_file_path.try_exists()? {
            let caption = fs::read_to_string(&caption_file_path)?;
            let caption = caption.trim().to_string();
            Some(caption)
        } else {
            None
        };
        let selph = Self {
            obj_hash: obj.hash.clone(),
            caption,
        };
        Ok(Some((selph, obj)))
    }
}
