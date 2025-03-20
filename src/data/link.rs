pub type Url = String; // TODO Something better?

#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
pub struct Link {
    #[serde(default)]
    pub name: Option<String>,
    pub url: Url,
}
