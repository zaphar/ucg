// Copyright 2019 Jeremy Wall
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
use std::fmt;

use super::Composite;
use super::Primitive;
use super::Value;

use Composite::{List, Tuple};
use Primitive::{Bool, Empty, Float, Int, Str};
use Value::{C, F, M, P, S, T};

impl fmt::Display for Value {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        match self {
            P(p) => write!(w, "{}", p),
            C(List(ref els, _)) => {
                write!(w, "[")?;
                for e in els {
                    write!(w, "{},", e)?;
                }
                write!(w, "]")
            }
            C(Tuple(ref flds, _)) => {
                write!(w, "{{")?;
                for (k, v) in flds {
                    write!(w, "\"{}\"={},\n", k, v)?;
                }
                write!(w, "}}")
            }
            F(_) => write!(w, "<Func>"),
            M(_) => write!(w, "<Module>"),
            T(_) => write!(w, "<Expression>"),
            S(v) => write!(w, "{}", v),
        }
    }
}

impl fmt::Display for Primitive {
    fn fmt(&self, w: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Bool(v) => write!(w, "{}", v),
            Int(v) => write!(w, "{}", v),
            Float(v) => write!(w, "{}", v),
            Str(v) => write!(w, "\"{}\"", v.replace("\"", "\\\"")),
            Empty => write!(w, "NULL"),
        }
    }
}
