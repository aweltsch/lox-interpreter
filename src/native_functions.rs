use crate::interpreter::LoxCallable;
use crate::interpreter::LoxValue;
use crate::interpreter::RuntimeError;
use crate::interpreter::Interpreter;

pub struct ClockFunction;

impl LoxCallable for ClockFunction {
    fn call(&self, interpreter: &mut Interpreter, arguments: Vec<LoxValue>) -> Result<LoxValue, RuntimeError> {
        Err("not implemented".to_string())
    }

    fn arity(&self) -> usize {
        0
    }
}
