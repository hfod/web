use crate::data::link::{self, Link};

#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
pub struct Talk {
    pub speaker_id: String, // Person.
    pub title: String,
    pub description: String,
    pub website: Option<link::Url>,

    #[serde(default)]
    pub artifacts: Vec<Link>,

    #[serde(default)]
    pub references: Vec<Link>,
}
