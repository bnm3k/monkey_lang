use std::rc::Rc;

use crate::object::Object;

pub struct BuiltinFunction(pub Box<dyn Fn(Vec<Rc<Object>>) -> Rc<Object>>);
impl std::fmt::Debug for BuiltinFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "builtin-function")
    }
}

unsafe impl Sync for BuiltinFunction {}

pub struct Builtins {}
impl Builtins {
    pub fn puts() -> BuiltinFunction {
        let puts_fn = |items: Vec<Rc<Object>>| -> Rc<Object> {
            for item in items {
                println!("{}", item.inspect());
            }
            Object::null()
        };
        BuiltinFunction(Box::new(puts_fn))
    }

    pub fn to_str() -> BuiltinFunction {
        let puts_fn = |items: Vec<Rc<Object>>| -> Rc<Object> {
            let mut items = items;
            if items.len() != 1 {
                return Rc::new(Object::Error(format!(
                    "wrong number of arguments. Got {}, want 1",
                    items.len()
                )));
            }
            Rc::new(Object::Str(items[0].inspect()))
        };
        BuiltinFunction(Box::new(puts_fn))
    }
    pub fn len() -> BuiltinFunction {
        let len_fn = |items: Vec<Rc<Object>>| -> Rc<Object> {
            let mut items = items;
            if items.len() != 1 {
                return Rc::new(Object::Error(format!(
                    "wrong number of arguments. Got {}, want 1",
                    items.len()
                )));
            }
            let arg = items.swap_remove(0);
            Rc::new(match &*arg {
                Object::Str(s) => Object::Int(s.len() as i64),
                Object::Array(vs) => Object::Int(vs.len() as i64),
                Object::Hash(map) => Object::Int(map.len() as i64),
                _ => Object::Error(format!(
                    "argument to `len` not supported, got {}",
                    arg.type_as_str()
                )),
            })
        };
        BuiltinFunction(Box::new(len_fn))
    }

    pub fn first() -> BuiltinFunction {
        let first_fn = |items: Vec<Rc<Object>>| -> Rc<Object> {
            let mut items = items;
            if items.len() != 1 {
                return Rc::new(Object::Error(format!(
                    "wrong number of arguments. Got {}, want 1",
                    items.len()
                )));
            }
            let arg = items.swap_remove(0);
            match &*arg {
                Object::Array(vs) => vs.get(0).map(|v| Rc::clone(v)).unwrap_or(Object::null()),
                _ => Rc::new(Object::Error(format!(
                    "argument to `len` not supported, got {}",
                    arg.type_as_str()
                ))),
            }
        };
        BuiltinFunction(Box::new(first_fn))
    }

    pub fn last() -> BuiltinFunction {
        let last_fn = |items: Vec<Rc<Object>>| -> Rc<Object> {
            let mut items = items;
            if items.len() != 1 {
                return Rc::new(Object::Error(format!(
                    "wrong number of arguments. Got {}, want 1",
                    items.len()
                )));
            }
            let arg = items.swap_remove(0);
            match &*arg {
                Object::Array(vs) => vs
                    .get(vs.len() - 1)
                    .map(|v| Rc::clone(v))
                    .unwrap_or(Object::null()),
                _ => Rc::new(Object::Error(format!(
                    "argument to `len` not supported, got {}",
                    arg.type_as_str()
                ))),
            }
        };
        BuiltinFunction(Box::new(last_fn))
    }

    pub fn rest() -> BuiltinFunction {
        let rest_fn = |items: Vec<Rc<Object>>| -> Rc<Object> {
            let mut items = items;
            if items.len() != 1 {
                return Rc::new(Object::Error(format!(
                    "wrong number of arguments. Got {}, want 1",
                    items.len()
                )));
            }
            let arg = items.swap_remove(0);
            match &*arg {
                Object::Array(vs) => {
                    if vs.len() > 0 {
                        Rc::new(Object::Array(
                            vs[1..].iter().map(|v| Rc::clone(v)).collect::<Vec<_>>(),
                        ))
                    } else {
                        Object::null()
                    }
                }
                _ => Rc::new(Object::Error(format!(
                    "argument to `len` not supported, got {}",
                    arg.type_as_str()
                ))),
            }
        };
        BuiltinFunction(Box::new(rest_fn))
    }

    pub fn push() -> BuiltinFunction {
        let push_fn = |items: Vec<Rc<Object>>| -> Rc<Object> {
            let mut items = items;
            if items.len() != 2 {
                return Rc::new(Object::Error(format!(
                    "wrong number of arguments. Got {}, want 1",
                    items.len()
                )));
            }
            let arg = items.swap_remove(0);
            let new_val = items.swap_remove(0);
            match &*arg {
                Object::Array(vs) => {
                    let mut new_vs = Vec::with_capacity(vs.len() + 1);
                    new_vs.extend(vs.iter().map(|v| Rc::clone(v)));
                    new_vs.push(new_val);
                    Rc::new(Object::Array(new_vs))
                }
                _ => Rc::new(Object::Error(format!(
                    "argument to `len` not supported, got {}",
                    arg.type_as_str()
                ))),
            }
        };
        BuiltinFunction(Box::new(push_fn))
    }
}
