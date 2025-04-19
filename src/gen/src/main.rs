use std::path::PathBuf;

use clap::Parser;
use tracing::level_filters::LevelFilter;

#[derive(Parser, Debug)]
#[command(version, about)]
struct Cli {
    #[clap(short, long = "log", default_value_t = LevelFilter::DEBUG)]
    log_level: LevelFilter,

    /// Cache directory.
    #[clap(short, long = "cache", default_value = ".cache")]
    cache_dir: PathBuf,

    /// Input data directory.
    #[clap(short, long = "in", default_value = "data")]
    input_dir: PathBuf,

    /// Output directory.
    #[clap(short, long = "out", default_value = "www")]
    output_dir: PathBuf,

    #[clap(subcommand)]
    command: Cmd,
}

#[derive(clap::Subcommand, Debug)]
enum Cmd {
    /// Generate the web site.
    Gen {
        #[clap(short, long)]
        minify: bool,
    },

    /// Create a new data record.
    New {
        #[clap(subcommand)]
        entry: Entry,
    },
}

#[derive(clap::Subcommand, Debug)]
enum Entry {
    Talk {
        #[clap(short, long = "meeting")]
        meeting_seq: i32,

        /// Must already exist.
        #[clap(short, long = "speaker")]
        speaker_id: String,

        #[clap(short, long)]
        title: String,

        #[clap(short, long)]
        description: String,

        #[clap(short, long)]
        website: Option<hfod_web_gen::data::link::Url>,
    },
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    hfod_web_gen::tracing::init(cli.log_level)?;
    let span = tracing::debug_span!(env!("CARGO_PKG_NAME"));
    let _span_guard = span.enter();
    tracing::debug!(?cli, "Starting.");
    match &cli.command {
        Cmd::Gen { minify } => {
            hfod_web_gen::pages::generate(
                &cli.cache_dir,
                &cli.input_dir,
                &cli.output_dir,
                *minify,
            )?;
        }
        Cmd::New {
            entry:
                Entry::Talk {
                    meeting_seq,
                    speaker_id,
                    title,
                    description,
                    website,
                },
        } => {
            use hfod_web_gen::data::talk::Talk;

            let talk = Talk {
                speaker_id: speaker_id.to_string(),
                title: title.to_string(),
                description: description.to_string(),
                website: website.to_owned(),
                artifacts: Vec::new(),
                references: Vec::new(),
            };
            hfod_web_gen::data::write_talk(
                &cli.input_dir,
                *meeting_seq,
                &talk,
            )?;
        }
    }
    Ok(())
}
