// Copyright 2018 Jeremy Wall <jeremy@marzhillstudios.com>
//
//  Licensed under the Apache License, Version 2.0 (the "License");
//  you may not use this file except in compliance with the License.
//  You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
//  Unless required by applicable law or agreed to in writing, software
//  distributed under the License is distributed on an "AS IS" BASIS,
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//  See the License for the specific language governing permissions and
//  limitations under the License.

use std;
use std::error::Error;
use std::io::Write;
use std::rc::Rc;

use super::traits::{ConvertResult, Converter};
use crate::ast::Position;
use crate::build::Val;
use crate::error::BuildError;
use crate::error::ErrorType;

use xml::common::XmlVersion;
use xml::writer::events::XmlEvent;
use xml::writer::EventWriter;
use xml::EmitterConfig;

pub struct XmlConverter {}

impl XmlConverter {
    fn get_str_val(v: &Val) -> std::result::Result<&str, Box<dyn Error>> {
        if let Val::Str(ref s) = v {
            Ok(s)
        } else {
            Err(Box::new(BuildError::new(
                "Not a String value",
                ErrorType::TypeFail,
                Position::new(0, 0, 0),
            )))
        }
    }

    fn get_tuple_val(v: &Val) -> std::result::Result<&Vec<(String, Rc<Val>)>, Box<dyn Error>> {
        if let Val::Tuple(ref fs) = v {
            Ok(fs)
        } else {
            Err(Box::new(BuildError::new(
                "Not a tuple value",
                ErrorType::TypeFail,
                Position::new(0, 0, 0),
            )))
        }
    }

    fn get_list_val(v: &Val) -> std::result::Result<&Vec<Rc<Val>>, Box<dyn Error>> {
        if let Val::List(ref fs) = v {
            Ok(fs)
        } else {
            Err(Box::new(BuildError::new(
                "Not a List value",
                ErrorType::TypeFail,
                Position::new(0, 0, 0),
            )))
        }
    }

    fn write_node<W: std::io::Write>(&self, v: &Val, w: &mut EventWriter<W>) -> ConvertResult {
        // First we determine if this is a tag or text node
        if let Val::Tuple(ref fs) = v {
            let mut name: Option<&str> = None;
            let mut attrs: Option<&Vec<(String, Rc<Val>)>> = None;
            let mut children: Option<&Vec<Rc<Val>>> = None;
            let mut text: Option<&str> = None;
            let mut ns: Option<(&str, &str)> = None;
            for (ref field, ref val) in fs.iter() {
                if field == "name" {
                    name = Some(Self::get_str_val(val.as_ref())?);
                }
                if field == "ns" {
                    if let Val::Tuple(ref fs) = val.as_ref() {
                        let mut prefix = "";
                        let mut uri = "";
                        for (ref name, ref val) in fs.iter() {
                            if val.is_empty() {
                                continue;
                            }
                            if name == "uri" {
                                uri = Self::get_str_val(val.as_ref())?;
                            }
                            if name == "prefix" {
                                prefix = Self::get_str_val(val.as_ref())?;
                            }
                        }
                        if uri != "" && prefix != "" {
                            ns = Some((prefix, uri));
                        }
                    } else if let Val::Str(ref s) = val.as_ref() {
                        ns = Some(("", s));
                    }
                }
                if field == "attrs" {
                    // This should be a tuple.
                    if !val.is_empty() {
                        attrs = Some(Self::get_tuple_val(val.as_ref())?);
                    }
                }
                if field == "children" {
                    // This should be a list of tuples.
                    if !val.is_empty() {
                        children = Some(Self::get_list_val(val.as_ref())?);
                    }
                }
                if field == "text" {
                    if !val.is_empty() {
                        text = Some(Self::get_str_val(val.as_ref())?);
                    }
                }
            }
            if name.is_some() && text.is_some() {
                return Err(Box::new(BuildError::new(
                    "XML nodes can not have both text and name fields",
                    ErrorType::TypeFail,
                    Position::new(0, 0, 0),
                )));
            }
            if name.is_some() {
                let mut start = XmlEvent::start_element(name.unwrap());
                if attrs.is_some() {
                    for (ref name, ref val) in attrs.unwrap().iter() {
                        if val.is_empty() {
                            continue;
                        }
                        start = start.attr(name.as_ref(), Self::get_str_val(val.as_ref())?);
                    }
                }
                if let Some((prefix, uri)) = ns {
                    if prefix == "" {
                        start = start.default_ns(uri);
                    } else {
                        start = start.ns(prefix, uri);
                    }
                }
                w.write(start)?;
                if children.is_some() {
                    for child in children.unwrap().iter() {
                        self.write_node(child.as_ref(), w)?;
                    }
                }
                w.write(XmlEvent::end_element())?;
            }
            if text.is_some() {
                w.write(XmlEvent::characters(text.unwrap()))?;
            }
        } else if let Val::Str(ref s) = v {
            w.write(XmlEvent::characters(s.as_ref()))?;
        } else {
            return Err(Box::new(BuildError::new(
                "XML nodes must be a Tuple or a string",
                ErrorType::TypeFail,
                Position::new(0, 0, 0),
            )));
        }
        Ok(())
    }

    fn write(&self, v: &Val, w: &mut Write) -> ConvertResult {
        if let Val::Tuple(ref fs) = v {
            let mut version: Option<&str> = None;
            let mut encoding: Option<&str> = None;
            let mut standalone: Option<bool> = None;
            let mut root: Option<Rc<Val>> = None;
            for &(ref name, ref val) in fs.iter() {
                if name == "version" {
                    version = Some(Self::get_str_val(val)?);
                }
                if name == "encoding" {
                    encoding = Some(Self::get_str_val(val)?);
                }
                if name == "standalone" {
                    standalone = match val.as_ref() {
                        Val::Boolean(b) => Some(*b),
                        _ => None,
                    };
                }
                if name == "root" {
                    root = Some(val.clone());
                }
            }
            match root {
                Some(n) => {
                    let mut writer = EmitterConfig::new()
                        .perform_indent(true)
                        .normalize_empty_elements(false)
                        .create_writer(w);
                    // first we see if we need to emit a document
                    // declaration event.
                    let version = match version {
                        Some(s) => {
                            if s == "1.0" {
                                Some(XmlVersion::Version10)
                            } else if s == "1.1" {
                                Some(XmlVersion::Version11)
                            } else {
                                // If they specified the wrong version then
                                // error out.
                                return Err(Box::new(BuildError::new(
                                    "XML version must be either 1.0 or 1.1",
                                    ErrorType::TypeFail,
                                    Position::new(0, 0, 0),
                                )));
                            }
                        }
                        None => None,
                    };
                    writer.write(XmlEvent::StartDocument {
                        // We default to version 1.1 documents if not specified.
                        version: version.unwrap_or(XmlVersion::Version10),
                        encoding: encoding,
                        standalone: standalone,
                    })?;
                    self.write_node(n.as_ref(), &mut writer)
                }
                None => Err(Box::new(BuildError::new(
                    "XML doc tuples must have a root field",
                    ErrorType::TypeFail,
                    Position::new(0, 0, 0),
                ))),
            }
        } else {
            Err(Box::new(BuildError::new(
                "XML outputs must be a Tuple",
                ErrorType::TypeFail,
                Position::new(0, 0, 0),
            )))
        }
    }
}

impl Converter for XmlConverter {
    fn convert(&self, v: Rc<Val>, mut w: &mut Write) -> ConvertResult {
        self.write(&v, &mut w)
    }

    fn file_ext(&self) -> String {
        String::from("xml")
    }

    fn description(&self) -> String {
        String::from("Convert a ucg DSL into xml.")
    }
}
