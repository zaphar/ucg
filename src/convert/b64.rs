use std::error::Error;
use std::rc::Rc;
use std::result::Result;

use base64::Engine;
use base64::{engine::general_purpose::STANDARD, engine::general_purpose::URL_SAFE};

use crate::build::Val;
use crate::convert::traits::Importer;

pub struct Base64Importer {
    pub url_safe: bool,
}

impl Importer for Base64Importer {
    fn import(&self, bytes: &[u8]) -> Result<Rc<Val>, Box<dyn Error>> {
        return if self.url_safe {
            Ok(Rc::new(Val::Str(STANDARD.encode(bytes).into())))
        } else {
            Ok(Rc::new(Val::Str(URL_SAFE.encode(bytes).into())))
        };
    }
}
