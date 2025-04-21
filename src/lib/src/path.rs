use std::path::{Component, Path, PathBuf};

use anyhow::anyhow;

pub fn reroot(parent: &Path, child: &Path) -> anyhow::Result<PathBuf> {
    let child = if child.is_absolute() {
        child.strip_prefix("/")?
    } else {
        assert!(child.is_relative());
        child
    };
    Ok(parent.join(child))
}

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

    use rstest::rstest;

    #[rstest]
    #[case("foo", "foo", "/")]
    #[case("foo/bar", "foo", "bar")]
    #[case("foo/bar", "foo", "/bar")]
    #[case("/foo/bar", "/foo", "/bar")]
    #[case("/foo/bar", "/foo", "bar")]
    #[case("/a/b/c/d/e", "/a/b", "/c/d/e")]
    fn reroot(
        #[case] expect: &str,
        #[case] parent: &str,
        #[case] child: &str,
    ) {
        macro_rules! p {
            ($path:expr) => {
                PathBuf::from($path)
            };
        }
        assert_eq!(
            p!(expect),
            super::reroot(&p!(parent), &p!(child)).unwrap()
        );
    }

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
