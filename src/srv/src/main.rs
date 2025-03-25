use std::{net::SocketAddr, path::PathBuf};

use anyhow::Context;
use clap::Parser;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Cli {
    /// Logging level.
    #[clap(short, long, default_value_t = tracing::Level::INFO)]
    log_level: tracing::Level,

    #[clap(short, long, default_value = "127.0.0.1:8080")]
    addr: SocketAddr,

    /// Directory of files to serve.
    #[clap(short, long)]
    web_dir: PathBuf,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    tracing_init(cli.log_level)?;
    let web_dir = &cli.web_dir;
    let web_dir = web_dir
        .canonicalize()
        .context(format!("Failed to canonicalize path: {:?}", web_dir))?;
    hfod_web_srv::run(cli.addr, &web_dir).await?;
    todo!()
}

fn tracing_init(level: tracing::Level) -> anyhow::Result<()> {
    use tracing_subscriber::{fmt, layer::SubscriberExt, EnvFilter, Layer};
    let layer_stderr = fmt::Layer::new()
        .with_writer(std::io::stderr)
        .with_ansi(true)
        .with_file(false)
        .with_line_number(true)
        .with_thread_ids(true)
        .with_filter(EnvFilter::from_default_env().add_directive(level.into()));
    tracing::subscriber::set_global_default(tracing_subscriber::registry().with(layer_stderr))?;
    Ok(())
}
