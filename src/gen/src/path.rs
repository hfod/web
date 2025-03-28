use std::path::{Path, PathBuf};

pub fn reroot(parent: &Path, child: &Path) -> anyhow::Result<PathBuf> {
    let child = if child.is_absolute() {
        child.strip_prefix("/")?
    } else {
        assert!(child.is_relative());
        child
    };
    Ok(parent.join(child))
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use rstest::rstest;

    #[rstest]
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
}
