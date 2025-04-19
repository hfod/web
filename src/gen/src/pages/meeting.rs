use std::path::PathBuf;

use askama::Template;

use crate::{
    data::{Data, meeting::Meeting},
    web_path,
};

#[derive(askama::Template)]
// XXX escape="none" to avoid butchering rendered content, in recap field.
#[template(path = "meeting.html", escape = "none")]
struct MeetingPage<'a> {
    data: &'a Data,
    meeting: &'a Meeting,
    collage_web_path: Option<PathBuf>,
}

pub fn build(data: &Data, meeting: &Meeting) -> anyhow::Result<String> {
    let collage_web_path = meeting
        .collage_obj_hash
        .as_ref()
        .map(|h| web_path::object(data.get_obj(h)));
    let html = MeetingPage {
        meeting,
        data,
        collage_web_path,
    }
    .render()?;
    Ok(html)
}
