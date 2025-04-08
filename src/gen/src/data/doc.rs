use std::path::Path;

use crate::{data::obj::Obj, md};

#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
pub struct Doc {
    pub text_html: String,
}

impl Doc {
    pub fn from_dir(
        doc_dir: &Path,
        web_path_objects: &Path,
    ) -> anyhow::Result<(Self, Vec<Obj>)> {
        let file_md = doc_dir.join("text.md");
        let (text_html, objects) =
            md::to_html_with_local_obj_refs(&file_md, web_path_objects)?;
        let selph = Self { text_html };
        Ok((selph, objects))
    }
}
