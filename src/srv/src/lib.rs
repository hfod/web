use std::{net::SocketAddr, path::Path};

use tokio::net::TcpListener;
use tower_http::trace::TraceLayer;

#[tracing::instrument(name = "server")]
pub async fn run(addr: SocketAddr, dir: &Path) -> anyhow::Result<()> {
    tracing::info!(?addr, ?dir, "Serving");
    let router = axum::Router::new()
        .nest_service("/", tower_http::services::ServeDir::new(dir));
    let listener = TcpListener::bind(addr).await?;
    axum::serve(listener, router.layer(TraceLayer::new_for_http())).await?;
    Ok(())
}
