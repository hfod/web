use std::{ffi::OsStr, fs, path::Path};

use anyhow::anyhow;

use crate::data::obj::Obj;

#[derive(Clone, Debug)]
pub struct Photo {
    pub obj_hash: String,
    pub caption: Option<String>,
}

impl Photo {
    pub fn from_bytes<S: AsRef<OsStr>>(
        bytes: Vec<u8>,
        ext: S,
        caption: Option<String>,
    ) -> anyhow::Result<Option<(Self, Obj)>> {
        let ext = ext.as_ref().to_owned();
        if !infer::is_image(&bytes[..]) {
            return Ok(None);
        }
        let obj = Obj::new(bytes, ext);
        let selph = Self {
            obj_hash: obj.hash.clone(),
            caption,
        };
        Ok(Some((selph, obj)))
    }

    pub fn from_file(
        photo_file_path: &Path,
    ) -> anyhow::Result<Option<(Self, Obj)>> {
        let ext = photo_file_path
            .extension()
            .ok_or(anyhow!(
                "Missing file extension in photo file: {photo_file_path:?}"
            ))?
            .to_os_string();
        let caption_file_path = photo_file_path.with_extension("txt");
        let caption = if caption_file_path.try_exists()? {
            let caption = fs::read_to_string(&caption_file_path)?;
            let caption = caption.trim().to_string();
            Some(caption)
        } else {
            None
        };
        let bytes = fs::read(&photo_file_path)?;
        Self::from_bytes(bytes, ext, caption)
    }
}
