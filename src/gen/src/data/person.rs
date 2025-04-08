use std::{fs, path::Path};

use anyhow::{Context, anyhow};

use crate::data::{
    link::{self, Link},
    obj::Obj,
    photo::Photo,
};

#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
pub struct Person {
    #[serde(default)]
    pub id: String,

    pub name: String,

    pub signal: Option<Link>,

    pub email: Option<String>,

    #[serde(default)]
    pub email_show: bool,

    pub website: Option<link::Url>,

    #[serde(default)]
    pub affiliated_links: Vec<Link>,

    #[serde(skip)]
    pub avatar: Option<Photo>,
}

impl Person {
    pub fn from_dir(dir_path: &Path) -> anyhow::Result<(Self, Vec<Obj>)> {
        let info_file_path = dir_path.join("info.json5");
        let photo_file_path = dir_path.join("photo.jpeg");
        let id = dir_path
            .file_name()
            .ok_or(anyhow!("Invalid person dir: {dir_path:?}"))?
            .to_str()
            .ok_or(anyhow!("Invalid person dir: {dir_path:?}"))?
            .to_string();
        let mut selph = {
            let ctx = info_file_path.display().to_string();
            let data = fs::read(&info_file_path).context(ctx.clone())?;
            let selph: Self =
                serde_json5::from_slice(&data[..]).context(ctx.clone())?;
            selph
        };
        selph.id = id;
        let mut objects = Vec::new();
        {
            let ctx = photo_file_path.display().to_string();
            if photo_file_path.try_exists().context(ctx.clone())? {
                if let Some((photo, obj)) =
                    Photo::from_file(&photo_file_path).context(ctx.clone())?
                {
                    selph.avatar = Some(photo);
                    objects.push(obj);
                }
            }
        }
        Ok((selph, objects))
    }
}
