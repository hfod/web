use askama::Template;

use crate::data::{Data, meeting::Meeting};

#[derive(askama::Template)]
// XXX escape="none" to avoid butchering rendered content, in recap field.
#[template(path = "meeting.html", escape = "none")]
pub struct MeetingPage<'a> {
    pub data: &'a Data,
    pub meeting: &'a Meeting,
}

pub fn build(data: &Data, meeting: &Meeting) -> anyhow::Result<String> {
    let html = MeetingPage { meeting, data }.render()?;
    Ok(html)
}
