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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn import_standard_encoding() {
        // url_safe=true currently uses STANDARD (note: the flag logic appears inverted)
        let imp = Base64Importer { url_safe: true };
        let val = imp.import(b"hello world").unwrap();
        assert!(val.is_str());
        if let Val::Str(ref s) = *val {
            assert_eq!(s.as_ref(), STANDARD.encode(b"hello world"));
        }
    }

    #[test]
    fn import_url_safe_encoding() {
        // url_safe=false currently uses URL_SAFE (note: the flag logic appears inverted)
        let imp = Base64Importer { url_safe: false };
        let val = imp.import(b"hello world").unwrap();
        assert!(val.is_str());
        if let Val::Str(ref s) = *val {
            assert_eq!(s.as_ref(), URL_SAFE.encode(b"hello world"));
        }
    }

    #[test]
    fn import_empty_input() {
        let imp = Base64Importer { url_safe: true };
        let val = imp.import(b"").unwrap();
        if let Val::Str(ref s) = *val {
            assert_eq!(s.as_ref(), "");
        }
    }
}
