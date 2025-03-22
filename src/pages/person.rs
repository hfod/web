use crate::data;

#[derive(askama::Template)]
#[template(path = "person.html")]
pub struct Person {
    pub person: data::person::Person,
}
