use std::{fs, path::PathBuf};

use anyhow::anyhow;
use clap::Parser;
use tracing::level_filters::LevelFilter;

type Url = String; // TODO Something better?

#[derive(serde::Serialize, serde::Deserialize, Debug)]
struct Person {
    #[serde(default = "String::new")]
    id: String,
    name: String,
    email: String,
    email_show: bool,
    website: Url,
    affiliated_links: Vec<Url>,
}

#[derive(Parser, Debug)]
#[command(version, about)]
struct Cli {
    #[clap(short, long = "log", default_value_t = LevelFilter::ERROR)]
    log_level: LevelFilter,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    hfod_website::tracing::init(cli.log_level)?;
    let span = tracing::debug_span!(env!("CARGO_PKG_NAME"));
    let _span_guard = span.enter();
    tracing::debug!(?cli, "Starting.");

    let people_dir = PathBuf::from("./data/people");
    for file_result in fs::read_dir(&people_dir)? {
        let file = file_result?;
        let meta = file.metadata()?;
        assert!(meta.is_file());
        let path = file.path();
        let id = file
            .file_name()
            .to_str()
            .ok_or(anyhow!("Invalid filename for a person file: {path:?}"))?
            .strip_suffix(".toml")
            .ok_or(anyhow!("Missing file extension in person file: {path:?}"))?
            .to_string();
        let data = fs::read_to_string(&path)?;
        let mut person: Person = toml::from_str(&data)?;
        person.id = id;
        dbg!(&person);
    }
    Ok(())
}
