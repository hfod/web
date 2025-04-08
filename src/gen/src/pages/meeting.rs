use crate::data::{self, Data};

#[derive(askama::Template)]
// XXX escape="none" to avoid butchering rendered content, in recap field.
#[template(path = "meeting.html", escape = "none")]
pub struct Meeting<'a> {
    pub data: &'a Data,
    pub meeting: &'a data::meeting::Meeting,
}
