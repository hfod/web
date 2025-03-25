use std::path::PathBuf;

#[derive(Clone, Debug)]
pub struct Link {
    pub name: String,
    pub path: PathBuf,
}
