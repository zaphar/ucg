use std::error::Error;
use std::rc::Rc;
use std::result::Result;

use base64::{encode, encode_config, URL_SAFE};

use crate::build::Val;
use crate::convert::traits::Importer;

pub struct Base64Importer {
    pub url_safe: bool,
}

impl Importer for Base64Importer {
    fn import(&self, bytes: &[u8]) -> Result<Rc<Val>, Box<dyn Error>> {
        let bslice = bytes.into();
        return if self.url_safe {
            Ok(Rc::new(Val::Str(encode(bslice))))
        } else {
            Ok(Rc::new(Val::Str(encode_config(bslice, URL_SAFE))))
        };
    }
}
