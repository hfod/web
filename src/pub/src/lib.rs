pub mod conf;

use std::{ffi::OsStr, path::Path, process};

use anyhow::bail;

pub fn run(remote: &conf::Remote, local_dir: &Path) -> anyhow::Result<()> {
    upload(remote, &local_dir)?;
    set_ownership(remote)?;
    set_permissions(remote)?;
    Ok(())
}

#[tracing::instrument(skip_all)]
fn upload(remote: &conf::Remote, local_dir: &Path) -> anyhow::Result<()> {
    tracing::info!("Uploading.");
    let mut cmd = process::Command::new("rsync");
    cmd.arg("-avz")
        .arg("--delete")
        .arg("--omit-dir-times")
        .arg("--copy-links")
        .args(["--rsh", &format!("ssh -p {}", remote.port)])
        .arg(format!(
            "{}@{}:{}",
            remote.user,
            remote.host,
            remote.dir.display()
        ))
        .arg(local_dir);
    let status = cmd.status()?;
    if !status.success() {
        bail!("rsync failed: {status:?}");
    }
    Ok(())
}

#[tracing::instrument(skip_all)]
fn set_ownership(remote: &conf::Remote) -> anyhow::Result<()> {
    let ownership = format!("{}:{}", remote.user, remote.group);
    tracing::info!(?ownership, "Setting ownership.");
    ssh(
        remote,
        ["chown", "-R", &ownership, &remote.dir.display().to_string()],
    )?;
    Ok(())
}

#[tracing::instrument(skip_all)]
fn set_permissions(remote: &conf::Remote) -> anyhow::Result<()> {
    let permissions = "a+rX";
    tracing::info!(?permissions, "Setting permissions.");
    ssh(
        remote,
        [
            "chmod",
            "-R",
            &permissions,
            &remote.dir.display().to_string(),
        ],
    )?;
    Ok(())
}

fn ssh<I, S>(remote: &conf::Remote, remote_cmd: I) -> anyhow::Result<()>
where
    I: IntoIterator<Item = S>,
    S: AsRef<OsStr>,
{
    let mut cmd = process::Command::new("ssh");
    cmd.args(["-p", &remote.port.to_string()])
        .arg(format!("{}@{}", &remote.user, &remote.host))
        .args(remote_cmd);
    let status = cmd.status()?;
    if !status.success() {
        bail!("ssh failed: {status:?}");
    }
    Ok(())
}
