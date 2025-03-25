use std::{fs, path::Path};

use anyhow::{anyhow, Context};

#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
pub struct Address {
    pub building: String,
    pub street: String,
    pub room: Option<String>,
    pub town: String,
    pub state: String,
    pub zipcode: String,
    pub country: String,

    // XXX Generating these URLs requires an API key
    //     and potentially costs, but making them manually is free.
    pub google_maps_embed_url: String,
}

#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
pub struct Venue {
    #[serde(default)]
    pub id: String,

    pub name: String,
    pub addr: Address,
    pub contact_id: String, // Person.id
}

impl Venue {
    pub fn from_dir(dir_path: &Path) -> anyhow::Result<Self> {
        let info_file_path = dir_path.join("info.json5");
        let id = dir_path
            .file_name()
            .ok_or(anyhow!("Invalid venue dir: {dir_path:?}"))?
            .to_str()
            .ok_or(anyhow!("Invalid venue dir: {dir_path:?}"))?
            .to_string();
        let mut selph = {
            let ctx = info_file_path.display().to_string();
            let data = fs::read(&info_file_path).context(ctx.clone())?;
            let selph: Self = serde_json5::from_slice(&data[..]).context(ctx.clone())?;
            selph
        };
        selph.id = id;
        Ok(selph)
    }
}
