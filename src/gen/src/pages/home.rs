use std::path::PathBuf;

use askama::Template;

use crate::{
    data::{Data, meeting::Meeting},
    time::Date,
    web_path,
};

#[derive(askama::Template)]
#[template(path = "home.html", escape = "none")]
struct Home<'a> {
    data: &'a Data,
    description_html: String,
    home_collage_web_path: Option<PathBuf>,
    upcoming_meetings: Vec<&'a Meeting>,
}

pub fn build(data: &Data) -> anyhow::Result<String> {
    let today = Date::today();
    let mut upcoming_meetings: Vec<&Meeting> =
        data.meetings()?.filter(|m| m.date >= today).collect();
    upcoming_meetings.sort_by_key(|m| m.date); // Oledest first.
    let html = Home {
        data,
        description_html: data.home()?.text_html.clone(),
        home_collage_web_path: data
            .get_obj_home_collage()
            .map(web_path::object),
        upcoming_meetings,
    }
    .render()?;
    Ok(html)
}
