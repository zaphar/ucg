use std::clone::Clone;
use std::collections::HashMap;
use std::convert::AsRef;
use std::convert::Into;
use std::error::Error;
use std::rc::Rc;

use crate::ast::Position;
use crate::ast::PositionedItem;
use crate::build::ir::Val;
use crate::error;

pub fn find_in_fieldlist(target: &str, fs: &Vec<(String, Rc<Val>)>) -> Option<Rc<Val>> {
    for (key, val) in fs.iter().cloned() {
        if target == &key {
            return Some(val.clone());
        }
    }
    return None;
}

/// Defines a set of values in a parsed file.
pub type ValueMap = HashMap<PositionedItem<String>, Rc<Val>>;

/// Defines a scope for execution in ucg.
///
/// Scopes in ucg are defined by the currently executing file and
/// the complex data types in that file. (Tuple, List, Modules, and the
/// left operands for dot selectors).
///
/// UCG Scopes do not descend up into their parent scopes so we do not maintain a stack
/// for those.
#[derive(Debug, PartialEq, Clone)]
pub struct Scope {
    pub import_stack: Vec<String>,
    pub env: Rc<Val>,
    pub curr_val: Option<Rc<Val>>,
    pub build_output: ValueMap,
    pub search_curr_val: bool,
    pub strict: bool,
}

impl Scope {
    // Construct a new scope with environment variables.
    pub fn new(env: Rc<Val>) -> Self {
        Self {
            import_stack: Vec::new(),
            env: env,
            // CurrVal represents the currently processing value.
            // (eg: Tuple, List. left side of a dot selection.)
            curr_val: None,
            build_output: HashMap::new(),
            search_curr_val: false,
            strict: false,
        }
    }

    pub fn use_strict(mut self) -> Self {
        self.strict = true;
        self
    }

    pub fn use_curr_val(mut self) -> Self {
        self.search_curr_val = true;
        self
    }

    /// Spawn a child scope based on the current scope but without the current
    /// val set.
    pub fn spawn_child(&self) -> Self {
        Self {
            import_stack: self.import_stack.clone(),
            env: self.env.clone(),
            // Children start with no current val
            curr_val: None,
            build_output: self.build_output.clone(),
            search_curr_val: false,
            strict: self.strict,
        }
    }

    pub fn spawn_clean(&self) -> Self {
        Self {
            import_stack: self.import_stack.clone(),
            env: self.env.clone(),
            // Children start with no current val
            curr_val: None,
            build_output: HashMap::new(),
            search_curr_val: false,
            strict: self.strict,
        }
    }

    /// Push an import onto the import stack.
    pub fn push_import<S: Into<String>>(&mut self, path: S) {
        self.import_stack.push(path.into());
    }

    pub fn prepend_import_stack(&mut self, imports: &Vec<String>) {
        let mut new_stack = self.import_stack.clone();
        new_stack.append(imports.clone().as_mut());
        self.import_stack = new_stack;
    }

    /// Set the current value for our execution context.
    pub fn set_curr_val(mut self, val: Rc<Val>) -> Self {
        self.curr_val = Some(val);
        self
    }

    /// Lookup up a list index in the current value
    pub fn lookup_idx(&self, pos: &Position, idx: &Val) -> Result<Rc<Val>, Box<dyn Error>> {
        if self.search_curr_val && self.curr_val.is_some() {
            if let &Val::List(ref fs) = self.curr_val.as_ref().unwrap().as_ref() {
                return Self::lookup_in_list(pos, idx, fs);
            }
        }
        Err(Box::new(error::BuildError::new_with_pos(
            "Not a list in index lookup.",
            error::ErrorType::TypeFail,
            pos.clone(),
        )))
    }

    /// Lookup a symbol in the current execution context.
    ///
    /// The lookup rules are simple.
    ///
    /// * `env` is always an environment variable lookup.
    /// * `self` is always the current value. This symbol is only
    ///    valid when the current value is a tuple.
    /// * everything else is looked up in the currently accumulated build output
    ///   for this execution context.
    pub fn lookup_sym(&self, sym: &PositionedItem<String>, is_symbol: bool) -> Option<Rc<Val>> {
        if &sym.val == "env" && is_symbol {
            return Some(self.env.clone());
        }
        if &sym.val == "self" && is_symbol {
            return self.curr_val.clone();
        }
        if self.search_curr_val && self.curr_val.is_some() {
            match self.curr_val.as_ref().unwrap().as_ref() {
                Val::Env(ref fs) => {
                    for (name, val) in fs.iter() {
                        if name == &sym.val {
                            return Some(Rc::new(Val::Str(val.clone())));
                        }
                    }
                    if !self.strict {
                        return Some(Rc::new(Val::Empty));
                    }
                }
                Val::Tuple(ref fs) => match Self::lookup_in_tuple(&sym.pos, &sym.val, fs) {
                    Ok(v) => return Some(v),
                    Err(_) => {
                        // noop
                    }
                },
                Val::List(ref fs) => {
                    match Self::lookup_in_list(&sym.pos, &Val::Str(sym.val.clone()), fs) {
                        Ok(v) => return Some(v),
                        Err(_) => {
                            // noop
                        }
                    }
                }
                Val::Boolean(_)
                | Val::Empty
                | Val::Float(_)
                | Val::Int(_)
                | Val::Module(_)
                | Val::Str(_)
                | Val::Func(_) => {
                    // noop
                }
            };
        }
        if self.build_output.contains_key(sym) {
            return Some(self.build_output[sym].clone());
        }
        None
    }

    fn lookup_in_tuple(
        pos: &Position,
        field: &str,
        fs: &Vec<(String, Rc<Val>)>,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        if let Some(vv) = find_in_fieldlist(&field, fs) {
            Ok(vv)
        } else {
            Err(Box::new(error::BuildError::new_with_pos(
                format!("Unable to {} match element in tuple.", field,),
                error::ErrorType::NoSuchSymbol,
                pos.clone(),
            )))
        }
    }

    fn lookup_in_list(
        pos: &Position,
        field: &Val,
        elems: &Vec<Rc<Val>>,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        let idx = match field {
            &Val::Int(i) => i as usize,
            &Val::Str(ref s) => s.parse::<usize>()?,
            _ => {
                return Err(Box::new(error::BuildError::new_with_pos(
                    format!("Invalid idx type {} for list lookup", field),
                    error::ErrorType::TypeFail,
                    pos.clone(),
                )));
            }
        };
        if idx < elems.len() {
            Ok(elems[idx].clone())
        } else {
            Err(Box::new(error::BuildError::new_with_pos(
                format!("idx {} out of bounds in list", idx),
                error::ErrorType::NoSuchSymbol,
                pos.clone(),
            )))
        }
    }
}
