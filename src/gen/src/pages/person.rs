use askama::Template;

use crate::{
    data::{self, Data, meeting::Meeting, talk::Talk},
    time::Date,
};

#[derive(askama::Template)]
#[template(path = "person.html")]
pub struct Person<'a> {
    pub person: data::person::Person,
    pub talks: &'a Vec<(i32, Date, Talk)>,
    pub organized: Option<&'a Vec<Meeting>>,
}

impl<'a> Person<'a> {
    pub fn build(
        person: data::person::Person,
        data: &'a Data,
    ) -> anyhow::Result<String> {
        let talks = data.get_talks_by(&person.id)?;
        let organized = data.get_meetings_organized_by(&person.id)?;
        let html_str = Self {
            person,
            talks,
            organized,
        }
        .render()?;
        Ok(html_str)
    }
}
