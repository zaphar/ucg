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
            Ok(Rc::new(Val::Str(URL_SAFE.encode(bytes).into())))
        } else {
            Ok(Rc::new(Val::Str(STANDARD.encode(bytes).into())))
        };
    }
}

#[cfg(test)]
mod test {
    use super::*;

    // Input bytes that produce `/` and `+` in standard base64, which become
    // `_` and `-` in URL-safe base64. This ensures the two encodings differ.
    const TEST_INPUT: &[u8] = &[0xFF, 0xFE, 0xFD, 0xFC, 0xFB, 0xFA];

    #[test]
    fn standard_and_urlsafe_differ_for_test_input() {
        let std_enc = STANDARD.encode(TEST_INPUT);
        let url_enc = URL_SAFE.encode(TEST_INPUT);
        assert_ne!(std_enc, url_enc, "test input must produce differing encodings");
    }

    #[test]
    fn import_standard_encoding() {
        let imp = Base64Importer { url_safe: false };
        let val = imp.import(TEST_INPUT).unwrap();
        if let Val::Str(ref s) = *val {
            assert_eq!(s.as_ref(), STANDARD.encode(TEST_INPUT));
        } else {
            panic!("expected Val::Str");
        }
    }

    #[test]
    fn import_url_safe_encoding() {
        let imp = Base64Importer { url_safe: true };
        let val = imp.import(TEST_INPUT).unwrap();
        if let Val::Str(ref s) = *val {
            assert_eq!(s.as_ref(), URL_SAFE.encode(TEST_INPUT));
        } else {
            panic!("expected Val::Str");
        }
    }

    #[test]
    fn import_empty_input() {
        let imp = Base64Importer { url_safe: false };
        let val = imp.import(b"").unwrap();
        if let Val::Str(ref s) = *val {
            assert_eq!(s.as_ref(), "");
        }
    }
}
