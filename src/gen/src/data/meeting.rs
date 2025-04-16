use std::{
    ffi::OsStr,
    fmt::Display,
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, anyhow, bail};

use crate::{
    collage,
    data::{doc::Doc, link, obj::Obj, photo::Photo, talk::Talk},
    time,
};

// TODO Perhaps have info.json5 map to a dedicated struct,
//      then copy from it into the Meeting struct.
#[derive(serde::Serialize, serde::Deserialize, Clone, Debug)]
pub struct Meeting {
    pub seq: i32, // Not usize because we need -1 :)
    pub format: Format,
    pub codename: String, // TODO Rename "codename" to simply "title"?
    pub date: time::Date,
    pub time: time::Time,
    pub venue_id: String, // Venue. Formerly: [host Host?]

    // HBIC of arrangements, promotion and logging.
    pub organizer_id: String, // Person. Formerly: [organizer Speaker?].

    pub registration_url: Option<link::Url>,
    pub recap: Option<Doc>,

    #[serde(default)]
    pub talks: Vec<Talk>,

    #[serde(skip)]
    pub photos: Vec<Photo>,

    #[serde(skip)]
    pub collage_obj_file_name: Option<PathBuf>,
}

impl Meeting {
    #[tracing::instrument(name = "meeting", skip_all, fields(dir = ?dir_path))]
    pub fn from_dir(
        cache_dir: &Path,
        dir_path: &Path,
        objects_web_path: &Path,
    ) -> anyhow::Result<(Self, Vec<Obj>)> {
        let talks_dir_path = dir_path.join("talks");
        let photos_dir_path = dir_path.join("photos");
        let info_file_path = dir_path.join("info.json5");
        let recap_dir_path = dir_path.join("recap");
        let dir_name = dir_path
            .file_name()
            .ok_or(anyhow!("Invalid meeting dir: {dir_path:?}"))?
            .to_str()
            .ok_or(anyhow!("Invalid meeting dir: {dir_path:?}"))?
            .to_string();
        let mut selph = {
            let ctx = info_file_path.display().to_string();
            let data = fs::read(&info_file_path).context(ctx.clone())?;
            let selph: Self =
                serde_json5::from_slice(&data[..]).context(ctx.clone())?;
            selph
        };
        // "YYYY-mm-dd--seq"
        match dir_name.split("--").collect::<Vec<&str>>()[..] {
            [date, seq] => {
                if date != selph.date.to_string() {
                    bail!(
                        "Meeting dates mismatch \
                        between dir name and info file. dir_path={dir_path:?}."
                    )
                }
                if seq != selph.seq.to_string() {
                    bail!(
                        "Meeting sequence numbers mismatch \
                        between dir name and info file. dir_path={dir_path:?}."
                    )
                }
            }
            _ => {
                bail!(
                    "Invalid meeting dir name format. dir_name={dir_name:?}, meeting={selph:?}."
                )
            }
        }
        if talks_dir_path.try_exists()? {
            for entry_result in fs::read_dir(&talks_dir_path)
                .context(talks_dir_path.display().to_string())?
            {
                let entry = entry_result?;
                let talk_file_path = entry.path();
                let ctx = talk_file_path.display().to_string();
                let typ = entry.file_type().context(ctx.clone())?;
                if typ.is_file()
                    && matches!(talk_file_path.extension(), Some(ext) if ext.eq(OsStr::new("json5")))
                {
                    let data: Vec<u8> =
                        fs::read(&talk_file_path).context(ctx.clone())?;
                    let talk: Talk = serde_json5::from_slice(&data[..])
                        .context(ctx.clone())?;
                    selph.talks.push(talk);
                }
            }
        }
        let mut objects = Vec::new();
        let mut photos_for_collage = Vec::new();
        if photos_dir_path.try_exists()? {
            for entry_result in fs::read_dir(&photos_dir_path).context(
                format!("Failed to list photos dir: {photos_dir_path:?}"),
            )? {
                let entry = entry_result?;
                let photo_file_path = entry.path();
                let typ = entry.file_type().context(format!(
                    "Failed to lookup file type for: {photo_file_path:?}"
                ))?;
                if typ.is_file() {
                    if let Some((photo, obj)) = Photo::from_file(
                        &photo_file_path,
                    )
                    .context(format!(
                        "Failed to read photo from file: {photo_file_path:?}"
                    ))? {
                        photos_for_collage.push(obj.data.clone());
                        selph.photos.push(photo);
                        objects.push(obj);
                    }
                }
            }
        }
        if recap_dir_path.try_exists()? {
            let (recap_doc, mut recap_objects) = Doc::from_dir(
                &recap_dir_path,
                objects_web_path,
            )
            .context(format!(
                "Failed to read recap doc from dir: {recap_dir_path:?}"
            ))?;
            selph.recap = Some(recap_doc);
            objects.append(&mut recap_objects);
        }
        let collage_file_path = cache_dir
            .join("meeting")
            .join(selph.seq.to_string())
            .join("collage.png");
        if let Some(obj) =
            collage::object(&collage_file_path, photos_for_collage).context(
                format!("Failed to build collage as {collage_file_path:?}"),
            )?
        {
            selph.collage_obj_file_name = Some(obj.to_file_name());
            objects.push(obj);
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

impl Display for Format {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::MeetAndGreet => "Meet & Greet",
            Self::ShowAndTell => "Show & Tell",
            Self::Talk => "Talk",
        };
        write!(f, "{s}")
    }
}
