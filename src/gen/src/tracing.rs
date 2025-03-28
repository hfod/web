use tracing::{Level, level_filters::LevelFilter};

pub fn init(level: LevelFilter) -> anyhow::Result<()> {
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
