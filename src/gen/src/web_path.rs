use std::path::PathBuf;

use crate::data::{meeting::Meeting, obj::Obj, person::Person, venue::Venue};

pub fn home() -> PathBuf {
    root()
}

pub fn objects() -> PathBuf {
    root().join("_obj")
}

pub fn object(obj: &Obj) -> PathBuf {
    objects().join(obj.to_file_name())
}

pub fn meetings() -> PathBuf {
    root().join("meetings")
}

pub fn meeting(Meeting { seq, .. }: &Meeting) -> PathBuf {
    meetings().join(seq.to_string())
}

pub fn people() -> PathBuf {
    root().join("people")
}

pub fn person(Person { id, .. }: &Person) -> PathBuf {
    people().join(id)
}

pub fn venues() -> PathBuf {
    root().join("venues")
}

pub fn venue(Venue { id, .. }: &Venue) -> PathBuf {
    venues().join(id)
}

// ============================================================================

fn root() -> PathBuf {
    PathBuf::from("/")
}
