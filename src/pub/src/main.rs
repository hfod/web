use std::path::PathBuf;

use clap::Parser;
use tracing::{Level, level_filters::LevelFilter};

use hfod_web_lib as hfod;

use hfod_web_pub::conf::Conf;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Cli {
    /// Logging level.
    #[clap(short, long = "log", default_value_t = LevelFilter::INFO)]
    log_level: LevelFilter,

    /// Config file path.
    #[clap(short, long, default_value = "~/.hfod-web-pub.json5")]
    conf: PathBuf,

    local_dir: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    tracing_init(cli.log_level)?;
    let span = tracing::info_span!(env!("CARGO_PKG_NAME"));
    let _span_guard = span.enter();
    tracing::info!(?cli, "Starting.");
    let conf_path = hfod::path::expand_tilde(&cli.conf)?;
    let conf = Conf::from_file(&conf_path)?;
    tracing::info!(?conf, "Executing.");
    hfod_web_pub::run(&conf.remote, &cli.local_dir)?;
    Ok(())
}

fn tracing_init(level: LevelFilter) -> anyhow::Result<()> {
    use tracing_subscriber::{
        EnvFilter, Layer,
        fmt::{self, format::FmtSpan},
        layer::SubscriberExt,
    };

    let span_events = if let Some(Level::TRACE) = level.into_level() {
        FmtSpan::NEW | FmtSpan::CLOSE
    } else {
        FmtSpan::CLOSE
    };

    let layer_stderr = fmt::Layer::new()
        .with_writer(std::io::stderr)
        .with_ansi(true)
        .with_file(false)
        .with_line_number(true)
        .with_thread_ids(true)
        .with_span_events(span_events)
        .with_filter(
            EnvFilter::from_default_env().add_directive(level.into()),
        );
    tracing::subscriber::set_global_default(
        tracing_subscriber::registry().with(layer_stderr),
    )?;
    Ok(())
}
