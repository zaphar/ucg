use std::result::Result;

use ast::{Expression, ListDef, ListOpDef, ListOpType, MacroDef, ModuleDef, SelectDef, Value};
use ast::{Token, TokenType};

#[derive(PartialEq, Clone, Debug)]
pub struct MacroTypedef {
    pub args: Vec<Production>,
    pub body: Vec<(String, Production)>,
}

#[derive(PartialEq, Clone, Debug)]
pub struct ModuleTypedef {
    pub args: Vec<(String, Production)>,
    // Each Statement should resolve to a production.
    pub body: Vec<(String, Production)>,
}

#[derive(PartialEq, Clone, Debug)]
pub enum Typedef {
    // Always equivalent to Any.
    Empty,
    Boolean,
    Int,
    Float,
    Str,
    // [Any...]
    List(Vec<Production>),
    // String -> Any
    Tuple(Vec<(String, Box<Production>)>),
    // Always string -> string
    // Env,
    Macro(MacroTypedef),
    Module(ModuleTypedef),
}

impl Typedef {
    pub fn reduce(&self, other: &Self) -> Result<Self, String> {
        match (self, other) {
            // handle the Empty case
            (&Typedef::Empty, ref other) => Ok((*other).clone()),
            (ref other, &Typedef::Empty) => Ok((*other).clone()),
            // handle the boolean cases
            (&Typedef::Boolean, &Typedef::Boolean) => Ok(Typedef::Boolean),
            (&Typedef::Boolean, _) => Err("Incompatible types".to_string()),
            // handle the Int cases
            (&Typedef::Int, &Typedef::Int) => Ok(Typedef::Int),
            (&Typedef::Int, _) => Err("Incompatible types".to_string()),
            // handle the Float cases
            (&Typedef::Float, &Typedef::Float) => Ok(Typedef::Float),
            (&Typedef::Float, _) => Err("Incompatible types".to_string()),
            // handle the Str cases
            (&Typedef::Str, &Typedef::Str) => Ok(Typedef::Str),
            (&Typedef::Str, _) => Err("Incompatible types".to_string()),
            // Handle List
            (&Typedef::List(ref lfs), &Typedef::List(ref rfs)) => {
                let idx = std::cmp::min(lfs.len(), rfs.len());
                let mut reduced_fs = Vec::new();
                for i in 0..idx {
                    let (lval, rval) = (&lfs[i], &rfs[i]);
                    match lval.reduce(rval) {
                        Err(e) => reduced_fs.push(Production::Any),
                        Ok(p) => reduced_fs.push(p),
                    }
                }
                Ok(Typedef::List(reduced_fs))
            }
            (&Typedef::List(_), _) => Err("Incompatible types".to_string()),
            // Handle Tuple
            (&Typedef::Tuple(ref lfs), &Typedef::Tuple(ref rfs)) => {
                let idx = std::cmp::min(lfs.len(), rfs.len());
                let mut reduced_fs = Vec::new();
                for i in 0..idx {
                    let (lkey, rkey) = (&lfs[i].0, &rfs[i].0);
                    let (lval, rval) = (&lfs[i].1, &rfs[i].1);
                    if lkey != rkey {
                        continue;
                    }
                    match lval.reduce(rval) {
                        Err(e) => return Err(e),
                        Ok(p) => reduced_fs.push((lkey.clone(), Box::new(p))),
                    }
                }
                Ok(Typedef::Tuple(reduced_fs))
            }
            (&Typedef::Tuple(_), _) => Err("Incompatible types".to_string()),
            // FIXME(jwall): Handle Macro
            (&Typedef::Macro(_), &Typedef::Macro(_)) => Err("Unimplemented...".to_string()),
            (&Typedef::Macro(_), _) => Err("Incompatible types".to_string()),
            // FIXME(jwall): Handle Module
            (&Typedef::Module(_), &Typedef::Module(_)) => Err("Unimplemented...".to_string()),
            (&Typedef::Module(_), _) => Err("Incompatible types".to_string()),
            _ => Err("Unimplemented...".to_string()),
        }
    }
}

// Productions can specialize or narrow from Any -> Either -> Exactly.
#[derive(PartialEq, Clone, Debug)]
pub enum Production {
    Exactly(Typedef),
    Either(Vec<Typedef>),
    // Any of our TypeDefs are allowed
    Any,
}

impl Production {
    pub fn reduce(&self, other: &Self) -> Result<Self, String> {
        match (self, other) {
            (&Production::Exactly(ref ldef), &Production::Exactly(ref rdef)) => {
                // We actually want to choose the subset of each other.
                if ldef == rdef {
                    Ok(Production::Exactly(ldef.clone()))
                } else {
                    Ok(Production::Exactly(ldef.reduce(&rdef)?))
                }
            }
            (&Production::Exactly(ref ldef), &Production::Either(ref rdefs)) => {
                for def in rdefs.iter() {
                    if let Ok(def) = ldef.reduce(&def) {
                        return Ok(Production::Exactly(def));
                    }
                }
                Err(format!(
                    "Incompatible types {:?} is not equivalent to {:?}",
                    self, other
                ))
            }
            (&Production::Exactly(ref ldef), &Production::Any) => {
                Ok(Production::Exactly(ldef.clone()))
            }
            (&Production::Either(ref ldefs), &Production::Exactly(ref rdef)) => {
                for def in ldefs.iter() {
                    if let Ok(def) = rdef.reduce(&def) {
                        return Ok(Production::Exactly(def));
                    }
                }
                Err(format!(
                    "Incompatible types {:?} is not equivalent to {:?}",
                    self, other
                ))
            }
            (&Production::Either(ref ldefs), &Production::Either(ref rdefs)) => {
                for ldef_item in ldefs.iter() {
                    for rdef_item in rdefs.iter() {
                        if let Ok(def) = rdef_item.reduce(ldef_item) {
                            return Ok(Production::Exactly(def));
                        }
                    }
                }
                Err(format!(
                    "Incompatible types {:?} is not equivalent to {:?}",
                    self, other
                ))
            }
            (&Production::Either(ref ldef), &Production::Any) => {
                Ok(Production::Either(ldef.clone()))
            }
            (&Production::Any, &Production::Exactly(ref rdef)) => {
                Ok(Production::Exactly(rdef.clone()))
            }
            (&Production::Any, &Production::Either(ref rdef)) => {
                Ok(Production::Either(rdef.clone()))
            }
            (&Production::Any, &Production::Any) => Ok(Production::Any),
        }
    }
}

// Produce a production rule from a Value.
pub fn produce_value_production(val: &Value) -> Result<Production, String> {
    match val {
        // Empty always maps to Any.
        &Value::Empty(_) => Ok(Production::Any),
        // Scalar literals map to exactly their scalar types.
        &Value::Int(_) => Ok(Production::Exactly(Typedef::Int)),
        &Value::Float(_) => Ok(Production::Exactly(Typedef::Float)),
        &Value::Boolean(_) => Ok(Production::Exactly(Typedef::Boolean)),
        &Value::Str(_) => Ok(Production::Exactly(Typedef::Str)),
        // Symbol lookups always map to any
        &Value::Symbol(_) => Ok(Production::Any),
        &Value::Selector(ref def) => {
            // TODO(jwall): We can do more specific validations here.
            let production = if def.sel.tail.is_none() {
                Production::Any
            } else {
                let tail = def.sel.tail.as_ref().unwrap();
                if tail.len() == 0 {
                    Production::Any
                } else {
                    produce_selector_production(&tail[0], &tail[1..])?
                }
            };
            Ok(production)
        }
        &Value::Tuple(ref fs) => Ok(produce_tuple_production(&fs.val)?),
        &Value::List(ref def) => Ok(produce_list_production(def)?),
    }
}

fn produce_list_production(def: &ListDef) -> Result<Production, String> {
    let mut listspec = Vec::new();
    for elem in def.elems.iter() {
        listspec.push(produce_expression_production(elem)?);
    }
    Ok(Production::Exactly(Typedef::List(listspec)))
}

fn produce_tuple_production(tpl: &Vec<(Token, Expression)>) -> Result<Production, String> {
    let mut fieldspec = Vec::new();
    for field in tpl.iter() {
        let tplspec = (
            field.0.fragment.clone(),
            Box::new(produce_expression_production(&field.1)?),
        );
        fieldspec.push(tplspec);
    }
    Ok(Production::Exactly(Typedef::Tuple(fieldspec)))
}

fn produce_selector_production(t: &Token, tokens: &[Token]) -> Result<Production, String> {
    if tokens.len() == 0 {
        return Ok(Production::Any);
    }
    if t.typ == TokenType::DIGIT {
        // Then this should be a list of at least digit length.
        let mut listspec = Vec::new();
        let idx = match t.fragment.parse::<usize>() {
            Ok(idx) => idx,
            Err(_) => return Err(format!("Invalid idx {}", t.fragment)),
        };
        if idx == 1 {
            listspec.push(Production::Any);
        } else if idx > 1 {
            for _ in 1..idx {
                listspec.push(Production::Any);
            }
        }
        // Populate the Vec with digit-1 items of type Any
        // Populate last item with result of produce_selector_rule call with rest of tokens
        listspec.push(produce_selector_production(&tokens[0], &tokens[1..])?);
        return Ok(Production::Exactly(Typedef::List(listspec)));
    } else if t.typ == TokenType::BAREWORD {
        let tplspec = (
            t.fragment.clone(),
            Box::new(produce_selector_production(&tokens[0], &tokens[1..])?),
        );
        return Ok(Production::Exactly(Typedef::Tuple(vec![tplspec])));
    }
    // Otherwise this is a type violation.
    Err("Unimplemented...".to_string())
}

pub fn produce_expression_production(expr: &Expression) -> Result<Production, String> {
    match expr {
        &Expression::Simple(ref val) => produce_value_production(val),
        &Expression::Binary(ref def) => {
            // get left production
            let left = produce_expression_production(&def.left)?;
            // get right production
            let right = produce_expression_production(&def.right)?;
            // FIXME(jwall): We should first check that the types are allowed
            //  For a binary operator.
            Ok(left.reduce(&right)?)
        }
        &Expression::Compare(ref def) => {
            // get left production
            let left = produce_expression_production(&def.left)?;
            // get right production
            let right = produce_expression_production(&def.right)?;
            left.reduce(&right)?;
            // If the two types are type compatible then compare
            // expressions always produce Boolean values exactly
            Ok(Production::Exactly(Typedef::Boolean))
        }
        &Expression::Grouped(ref expr) => produce_expression_production(expr),
        // Format expressions always produce Str values exactly
        &Expression::Format(_) => Ok(Production::Exactly(Typedef::Str)),
        // Copy must produce a tuple with at least the specified fields.
        &Expression::Copy(ref _def) => Err("Unimplemented...".to_string()),
        // Calls must validate the args and they always produce a tuple.
        &Expression::Call(ref _def) => Err("Unimplemented...".to_string()),
        &Expression::Select(ref _def) => Err("Unimplemented...".to_string()),
        &Expression::Macro(ref _def) => Err("Unimplemented...".to_string()),
        &Expression::ListOp(ref _def) => Err("Unimplemented...".to_string()),
        &Expression::Module(ref _def) => Err("Unimplemented...".to_string()),
    }
}
