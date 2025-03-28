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

    let bytes: Vec<u8> = sha2::Sha256::digest(data).to_vec();
    hex_encode(bytes.as_slice())
}

fn hex_encode(bytes: &[u8]) -> String {
    use std::fmt::Write;

    // Ref: https://rust-lang.github.io/rust-clippy/master/index.html#format_collect
    bytes
        .iter()
        .fold(String::new(), |mut hex, byte| {
            write!(hex, "{byte:02X}").unwrap_or_else(|e| {
                unreachable!("Writes to a string failed: {e:?}")
            });
            hex
        })
        .to_lowercase()
}
