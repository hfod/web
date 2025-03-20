use std::ffi::OsString;

#[derive(Clone, Debug)]
pub struct Obj {
    pub hash: String,
    pub data: Vec<u8>,
    pub ext: OsString,
}

impl Obj {
    pub fn new(data: Vec<u8>, ext: OsString) -> Self {
        let hash = sha2_256_hex(&data[..]);
        Self { hash, data, ext }
    }
}

fn sha2_256_hex(data: &[u8]) -> String {
    use sha2::Digest;

    sha2::Sha256::digest(data)
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect()
}
