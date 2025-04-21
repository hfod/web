use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::Context;

#[derive(serde::Deserialize, Debug)]
pub struct Remote {
    pub host: String,
    pub port: u16,
    pub user: String,
    pub group: String,
    pub dir: PathBuf,
}

#[derive(serde::Deserialize, Debug)]
pub struct Conf {
    pub remote: Remote,
}

impl Conf {
    pub fn from_file(path: &Path) -> anyhow::Result<Self> {
        tracing::info!(?path, "Reading config file.");
        let str = fs::read_to_string(path)
            .context(format!("Failed to read config file from: {path:?}"))?;
        let selph = serde_json5::from_str(&str)
            .context(format!("Failed to parse config"))?;
        Ok(selph)
    }
}
