use std::path::Path;

use crate::{data::obj::Obj, md, web_path};

#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
pub struct Doc {
    pub text_html: String,
}

impl Doc {
    pub fn from_dir(doc_dir: &Path) -> anyhow::Result<(Self, Vec<Obj>)> {
        let file_md = doc_dir.join("text.md");
        let (text_html, objects) =
            md::to_html_with_local_obj_refs(&file_md, &web_path::objects())?;
        let selph = Self { text_html };
        Ok((selph, objects))
    }
}
