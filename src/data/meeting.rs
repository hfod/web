use std::{fs, path::Path};

use anyhow::{anyhow, bail, Context};

use crate::{
    data::{link, obj::Obj, photo::Photo, talk::Talk},
    time,
};

#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
pub struct Meeting {
    pub seq: i32, // Not usize because we need -1 :)
    pub format: Format,
    pub codename: String,
    pub date: time::Date,
    pub time: time::Time,
    pub venue_id: String, // Venue. Formerly: [host Host?]

    // HBIC of arrangements, promotion and logging.
    pub organizer_id: String, // Person. Formerly: [organizer Speaker?].

    pub registration_url: Option<link::Url>,
    pub recap: Option<String>, // Markdown.

    #[serde(default)]
    pub talks: Vec<Talk>,

    #[serde(skip)]
    pub photos: Vec<Photo>,
}

impl Meeting {
    pub fn from_dir(dir_path: &Path) -> anyhow::Result<(Self, Vec<Obj>)> {
        let talks_dir_path = dir_path.join("talks");
        let photos_dir_path = dir_path.join("photos");
        let info_file_path = dir_path.join("info.json5");
        let dir_name = dir_path
            .file_name()
            .ok_or(anyhow!("Invalid meeting dir: {dir_path:?}"))?
            .to_str()
            .ok_or(anyhow!("Invalid meeting dir: {dir_path:?}"))?
            .to_string();
        let mut selph = {
            let ctx = info_file_path.display().to_string();
            let data = fs::read(&info_file_path).context(ctx.clone())?;
            let selph: Self = serde_json5::from_slice(&data[..]).context(ctx.clone())?;
            selph
        };
        assert_eq!(dir_name, selph.date.to_string());
        if talks_dir_path.try_exists()? {
            for entry_result in
                fs::read_dir(&talks_dir_path).context(talks_dir_path.display().to_string())?
            {
                let entry = entry_result?;
                let talk_file_path = entry.path();
                let ctx = talk_file_path.display().to_string();
                let typ = entry.file_type().context(ctx.clone())?;
                if !typ.is_file() {
                    bail!("Invalid talk entry: {talk_file_path:?}");
                }
                let data: Vec<u8> = fs::read(&talk_file_path).context(ctx.clone())?;
                let talk: Talk = serde_json5::from_slice(&data[..]).context(ctx.clone())?;
                selph.talks.push(talk);
            }
        }
        let mut objects = Vec::new();
        if photos_dir_path.try_exists()? {
            for entry_result in
                fs::read_dir(&photos_dir_path).context(photos_dir_path.display().to_string())?
            {
                let entry = entry_result?;
                let photo_file_path = entry.path();
                let ctx = photo_file_path.display().to_string();
                let typ = entry.file_type().context(ctx.clone())?;
                if typ.is_file() {
                    if let Some((photo, obj)) = Photo::from_file(&photo_file_path)? {
                        selph.photos.push(photo);
                        objects.push(obj);
                    }
                }
            }
        }
        Ok((selph, objects))
    }
}

#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
pub enum Format {
    MeetAndGreet,
    ShowAndTell,
    Talk, // TODO: Better name?
}
