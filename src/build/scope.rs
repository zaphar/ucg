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

pub fn find_in_fieldlist(target: &str, fs: &Vec<(Rc<str>, Rc<Val>)>) -> Option<Rc<Val>> {
    for (key, val) in fs {
        if target == key.as_ref() {
            return Some(val.clone());
        }
    }
    None
}

/// Defines a set of values in a parsed file.
pub type ValueMap = HashMap<PositionedItem<Rc<str>>, Rc<Val>>;

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
            env,
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

    pub fn prepend_import_stack(&mut self, imports: &[String]) {
        let mut new_stack = self.import_stack.clone();
        new_stack.extend_from_slice(imports);
        self.import_stack = new_stack;
    }

    /// Set the current value for our execution context.
    pub fn set_curr_val(mut self, val: Rc<Val>) -> Self {
        self.curr_val = Some(val);
        self
    }

    /// Lookup up a list index in the current value
    pub fn lookup_idx(&self, pos: &Position, idx: &Val) -> Result<Rc<Val>, Box<dyn Error>> {
        if self.search_curr_val {
            if let Some(ref cv) = self.curr_val {
                if let Val::List(fs) = cv.as_ref() {
                    return Self::lookup_in_list(pos, idx, fs);
                }
            }
        }
        Err(error::BuildError::with_pos(
            "Not a list in index lookup.",
            error::ErrorType::TypeFail,
            pos.clone(),
        )
        .to_boxed())
    }

    /// Lookup a symbol in the current execution context.
    ///
    /// The lookup rules are simple.
    ///
    /// * `env` is always an environment variable lookup.
    /// * `self` is always the current value. This symbol is only
    ///   valid when the current value is a tuple.
    /// * everything else is looked up in the currently accumulated build output
    ///   for this execution context.
    pub fn lookup_sym(&self, sym: &PositionedItem<Rc<str>>, is_symbol: bool) -> Option<Rc<Val>> {
        if sym.val.as_ref() == "env" && is_symbol {
            return Some(self.env.clone());
        }
        if sym.val.as_ref() == "self" && is_symbol {
            return self.curr_val.clone();
        }
        if self.search_curr_val {
            if let Some(ref cv) = self.curr_val {
            match cv.as_ref() {
                Val::Env(fs) => {
                    for (name, val) in fs.iter() {
                        if name == &sym.val {
                            return Some(Rc::new(Val::Str(val.clone())));
                        }
                    }
                    if !self.strict {
                        return Some(Rc::new(Val::Empty));
                    }
                }
                Val::Tuple(fs) => match Self::lookup_in_tuple(&sym.pos, &sym.val, fs) {
                    Ok(v) => return Some(v),
                    Err(_) => {
                        // noop
                    }
                },
                Val::List(fs) => {
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
                | Val::Str(_)
                | Val::Constraint(_) => {
                    // noop
                }
            };
            }
        }
        if self.build_output.contains_key(sym) {
            return Some(self.build_output[sym].clone());
        }
        None
    }

    fn lookup_in_tuple(
        pos: &Position,
        field: &str,
        fs: &Vec<(Rc<str>, Rc<Val>)>,
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        if let Some(vv) = find_in_fieldlist(field, fs) {
            Ok(vv)
        } else {
            Err(error::BuildError::with_pos(
                format!("Unable to {} match element in tuple.", field,),
                error::ErrorType::NoSuchSymbol,
                pos.clone(),
            )
            .to_boxed())
        }
    }

    fn lookup_in_list(
        pos: &Position,
        field: &Val,
        elems: &[Rc<Val>],
    ) -> Result<Rc<Val>, Box<dyn Error>> {
        let idx = match field {
            &Val::Int(i) => i as usize,
            Val::Str(s) => s.parse::<usize>()?,
            _ => {
                return Err(error::BuildError::with_pos(
                    format!("Invalid idx type {} for list lookup", field),
                    error::ErrorType::TypeFail,
                    pos.clone(),
                )
                .to_boxed());
            }
        };
        if idx < elems.len() {
            Ok(elems[idx].clone())
        } else {
            Err(error::BuildError::with_pos(
                format!("idx {} out of bounds in list", idx),
                error::ErrorType::NoSuchSymbol,
                pos.clone(),
            )
            .to_boxed())
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::rc::Rc;

    fn test_pos() -> Position {
        Position::new(0, 0, 0)
    }

    fn positioned<T: Clone>(val: T) -> PositionedItem<T> {
        PositionedItem {
            pos: test_pos(),
            val,
        }
    }

    fn make_env() -> Rc<Val> {
        Rc::new(Val::Env(vec![
            (Rc::from("HOME"), Rc::from("/home/user")),
            (Rc::from("PATH"), Rc::from("/usr/bin")),
        ]))
    }

    // Construction and builder tests

    #[test]
    fn new_scope_defaults() {
        let scope = Scope::new(make_env());
        assert!(!scope.strict);
        assert!(!scope.search_curr_val);
        assert!(scope.curr_val.is_none());
        assert!(scope.build_output.is_empty());
        assert!(scope.import_stack.is_empty());
    }

    #[test]
    fn use_strict_sets_flag() {
        let scope = Scope::new(make_env()).use_strict();
        assert!(scope.strict);
    }

    #[test]
    fn use_curr_val_sets_flag() {
        let scope = Scope::new(make_env()).use_curr_val();
        assert!(scope.search_curr_val);
    }

    #[test]
    fn set_curr_val() {
        let val = Rc::new(Val::Int(42));
        let scope = Scope::new(make_env()).set_curr_val(val.clone());
        assert_eq!(scope.curr_val, Some(val));
    }

    // spawn_child / spawn_clean tests

    #[test]
    fn spawn_child_inherits_output() {
        let mut scope = Scope::new(make_env()).use_strict();
        scope
            .build_output
            .insert(positioned(Rc::from("x")), Rc::new(Val::Int(1)));
        scope.push_import("file.ucg");

        let child = scope.spawn_child();
        assert!(child.strict);
        assert!(!child.search_curr_val);
        assert!(child.curr_val.is_none());
        assert_eq!(child.build_output.len(), 1);
        assert_eq!(child.import_stack, vec!["file.ucg".to_string()]);
    }

    #[test]
    fn spawn_clean_has_empty_output() {
        let mut scope = Scope::new(make_env()).use_strict();
        scope
            .build_output
            .insert(positioned(Rc::from("x")), Rc::new(Val::Int(1)));

        let clean = scope.spawn_clean();
        assert!(clean.strict);
        assert!(clean.build_output.is_empty());
    }

    // Import stack tests

    #[test]
    fn push_import() {
        let mut scope = Scope::new(make_env());
        scope.push_import("a.ucg");
        scope.push_import("b.ucg");
        assert_eq!(scope.import_stack, vec!["a.ucg", "b.ucg"]);
    }

    #[test]
    fn prepend_import_stack() {
        let mut scope = Scope::new(make_env());
        scope.push_import("a.ucg");
        scope.prepend_import_stack(&vec!["b.ucg".to_string(), "c.ucg".to_string()]);
        assert_eq!(scope.import_stack, vec!["a.ucg", "b.ucg", "c.ucg"]);
    }

    // lookup_sym tests

    #[test]
    fn lookup_sym_env() {
        let scope = Scope::new(make_env());
        let sym = positioned(Rc::from("env"));
        let result = scope.lookup_sym(&sym, true);
        assert!(result.is_some());
        assert!(result.unwrap().is_env());
    }

    #[test]
    fn lookup_sym_env_not_symbol_skips() {
        let scope = Scope::new(make_env());
        let sym = positioned(Rc::from("env"));
        // is_symbol=false means "env" is treated as a regular field name
        let result = scope.lookup_sym(&sym, false);
        assert!(result.is_none());
    }

    #[test]
    fn lookup_sym_self_with_curr_val() {
        let val = Rc::new(Val::Int(99));
        let scope = Scope::new(make_env()).set_curr_val(val.clone());
        let sym = positioned(Rc::from("self"));
        let result = scope.lookup_sym(&sym, true);
        assert_eq!(result, Some(val));
    }

    #[test]
    fn lookup_sym_self_without_curr_val() {
        let scope = Scope::new(make_env());
        let sym = positioned(Rc::from("self"));
        let result = scope.lookup_sym(&sym, true);
        assert!(result.is_none());
    }

    #[test]
    fn lookup_sym_in_tuple_curr_val() {
        let tuple = Rc::new(Val::Tuple(vec![(
            Rc::from("name"),
            Rc::new(Val::Str(Rc::from("alice"))),
        )]));
        let scope = Scope::new(make_env()).set_curr_val(tuple).use_curr_val();
        let sym = positioned(Rc::from("name"));
        let result = scope.lookup_sym(&sym, false);
        assert!(result.is_some());
        assert!(result.unwrap().equal(&Val::Str(Rc::from("alice"))).unwrap());
    }

    #[test]
    fn lookup_sym_in_build_output() {
        let mut scope = Scope::new(make_env());
        let key = positioned(Rc::from("myvar"));
        scope.build_output.insert(key.clone(), Rc::new(Val::Int(7)));
        let result = scope.lookup_sym(&key, true);
        assert_eq!(result, Some(Rc::new(Val::Int(7))));
    }

    #[test]
    fn lookup_sym_missing_returns_none() {
        let scope = Scope::new(make_env());
        let sym = positioned(Rc::from("nonexistent"));
        assert!(scope.lookup_sym(&sym, true).is_none());
    }

    #[test]
    fn lookup_sym_in_env_curr_val() {
        let scope = Scope::new(make_env())
            .set_curr_val(make_env())
            .use_curr_val();
        let sym = positioned(Rc::from("HOME"));
        let result = scope.lookup_sym(&sym, false);
        assert!(result.is_some());
        assert!(result
            .unwrap()
            .equal(&Val::Str(Rc::from("/home/user")))
            .unwrap());
    }

    #[test]
    fn lookup_sym_in_env_curr_val_missing_nonstrict_returns_empty() {
        let scope = Scope::new(make_env())
            .set_curr_val(make_env())
            .use_curr_val();
        let sym = positioned(Rc::from("MISSING"));
        let result = scope.lookup_sym(&sym, false);
        assert_eq!(result, Some(Rc::new(Val::Empty)));
    }

    // lookup_idx tests

    #[test]
    fn lookup_idx_valid() {
        let list = Rc::new(Val::List(vec![
            Rc::new(Val::Int(10)),
            Rc::new(Val::Int(20)),
        ]));
        let scope = Scope::new(make_env()).set_curr_val(list).use_curr_val();
        let result = scope.lookup_idx(&test_pos(), &Val::Int(1)).unwrap();
        assert_eq!(*result, Val::Int(20));
    }

    #[test]
    fn lookup_idx_out_of_bounds() {
        let list = Rc::new(Val::List(vec![Rc::new(Val::Int(10))]));
        let scope = Scope::new(make_env()).set_curr_val(list).use_curr_val();
        assert!(scope.lookup_idx(&test_pos(), &Val::Int(5)).is_err());
    }

    #[test]
    fn lookup_idx_not_a_list() {
        let scope = Scope::new(make_env())
            .set_curr_val(Rc::new(Val::Int(1)))
            .use_curr_val();
        assert!(scope.lookup_idx(&test_pos(), &Val::Int(0)).is_err());
    }

    #[test]
    fn lookup_idx_string_index() {
        let list = Rc::new(Val::List(vec![
            Rc::new(Val::Str(Rc::from("a"))),
            Rc::new(Val::Str(Rc::from("b"))),
        ]));
        let scope = Scope::new(make_env()).set_curr_val(list).use_curr_val();
        let result = scope
            .lookup_idx(&test_pos(), &Val::Str(Rc::from("1")))
            .unwrap();
        assert_eq!(*result, Val::Str(Rc::from("b")));
    }

    // find_in_fieldlist tests

    #[test]
    fn find_in_fieldlist_found() {
        let fields = vec![(Rc::from("a"), Rc::new(Val::Int(1)))];
        assert!(find_in_fieldlist("a", &fields).is_some());
    }

    #[test]
    fn find_in_fieldlist_not_found() {
        let fields = vec![(Rc::from("a"), Rc::new(Val::Int(1)))];
        assert!(find_in_fieldlist("b", &fields).is_none());
    }
}
