use std::path::PathBuf;

use clap::Parser;
use tracing::level_filters::LevelFilter;

use hfod_website::data;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Cli {
    #[clap(short, long = "log", default_value_t = LevelFilter::ERROR)]
    log_level: LevelFilter,

    #[clap(short, long = "data", default_value = "./data")]
    data_dir: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    hfod_website::tracing::init(cli.log_level)?;
    let span = tracing::debug_span!(env!("CARGO_PKG_NAME"));
    let _span_guard = span.enter();
    tracing::debug!(?cli, "Starting.");

    let data_dir = cli.data_dir.canonicalize()?;
    let store = data::Store::connect(&data_dir)?;
    for person in store.people()? {
        dbg!(person);
    }
    for venue in store.venues()? {
        dbg!(venue);
    }
    for meeting in store.meetings()? {
        dbg!(meeting);
    }
    for obj in store.objects()? {
        dbg!((&obj.hash, &obj.ext));
    }
    Ok(())
}
