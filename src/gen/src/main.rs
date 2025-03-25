use std::path::PathBuf;

use clap::Parser;
use tracing::level_filters::LevelFilter;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Cli {
    #[clap(short, long = "log", default_value_t = LevelFilter::ERROR)]
    log_level: LevelFilter,

    /// Input data directory.
    #[clap(short, long = "in", default_value = "data")]
    input_dir: PathBuf,

    /// Output directory.
    #[clap(short, long = "out", default_value = "www")]
    output_dir: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    hfod_web_gen::tracing::init(cli.log_level)?;
    let span = tracing::debug_span!(env!("CARGO_PKG_NAME"));
    let _span_guard = span.enter();
    tracing::debug!(?cli, "Starting.");
    hfod_web_gen::pages::generate(&cli.input_dir, &cli.output_dir)?;
    Ok(())
}
