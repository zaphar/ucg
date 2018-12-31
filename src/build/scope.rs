use std::clone::Clone;
use std::collections::HashMap;
use std::convert::Into;
use std::rc::Rc;

use crate::ast::PositionedItem;
use crate::build::ir::Val;

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
#[derive(Clone)]
pub struct Scope {
    pub import_stack: Vec<String>,
    pub env: Rc<Val>,
    pub curr_val: Option<Rc<Val>>,
    pub build_output: ValueMap,
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
        }
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
        }
    }

    pub fn spawn_clean(&self) -> Self {
        Self {
            import_stack: self.import_stack.clone(),
            env: self.env.clone(),
            // Children start with no current val
            curr_val: None,
            build_output: HashMap::new(),
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
    pub fn set_curr_val(&mut self, val: Rc<Val>) {
        self.curr_val = Some(val);
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
    pub fn lookup_sym(&self, sym: &PositionedItem<String>) -> Option<Rc<Val>> {
        if &sym.val == "env" {
            return Some(self.env.clone());
        }
        if &sym.val == "self" {
            return self.curr_val.clone();
        }
        if self.build_output.contains_key(sym) {
            return Some(self.build_output[sym].clone());
        }
        None
    }
}
