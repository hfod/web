use std::path::{Component, Path, PathBuf};

use anyhow::anyhow;

pub fn expand_tilde<P: AsRef<Path>>(path: P) -> anyhow::Result<PathBuf> {
    let base_dirs = directories::BaseDirs::new().ok_or_else(|| {
        anyhow!("Could not determine user's base directories.")
    })?;
    let expanded = path
        .as_ref()
        .components()
        .enumerate()
        .flat_map(|i_component| -> Box<dyn Iterator<Item = Component>> {
            match i_component {
                (0, Component::Normal(name)) if name == "~" => {
                    Box::new(base_dirs.home_dir().components())
                }
                (_, c) => Box::new([c].into_iter()),
            }
        })
        .collect();
    Ok(expanded)
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    #[test]
    fn expand_tilde() {
        let base_dirs = directories::BaseDirs::new().unwrap();
        let home = base_dirs.home_dir();

        assert_eq!(home.join("foo"), super::expand_tilde("~/foo").unwrap());
        assert_eq!(
            PathBuf::from("/foo/~"),
            super::expand_tilde("/foo/~").unwrap()
        );
        assert_eq!(
            PathBuf::from("/foo/~/bar"),
            super::expand_tilde("/foo/~/bar").unwrap()
        );
    }
}
