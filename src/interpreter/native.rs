use super::{CodeLocation, Interpreter, LoxExprError, LoxObject, NativeFunction};

use std::collections::HashMap;

type FuncReturn = Result<LoxObject, (LoxExprError, CodeLocation)>;

fn eprint(_: &mut dyn Interpreter, args: &[LoxObject]) -> FuncReturn {
    eprintln!("{}", args[0]);
    Ok(LoxObject::Nil)
}

fn input(_: &mut dyn Interpreter, _: &[LoxObject]) -> FuncReturn {
    use std::io::stdin;
    let mut input = String::new();
    stdin().read_line(&mut input).map_err(|e| {
        (
            LoxExprError::RuntimeError(format!("Error reading input: {}", e)),
            CodeLocation::NoInfo,
        )
    })?;
    Ok(LoxObject::String(input))
}

fn print(_: &mut dyn Interpreter, args: &[LoxObject]) -> FuncReturn {
    println!("{}", args[0]);
    Ok(LoxObject::Nil)
}

fn time(_: &mut dyn Interpreter, _: &[LoxObject]) -> FuncReturn {
    use std::time::{Duration, SystemTime, UNIX_EPOCH};
    let duration = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or(Duration::from_secs(0));
    Ok(LoxObject::Float(duration.as_secs_f64()))
}

fn object_constructor(_: &mut dyn Interpreter, _: &[LoxObject]) -> FuncReturn {
    Ok(LoxObject::Object(HashMap::new()))
}

pub fn get_std_items() -> std::collections::HashMap<String, LoxObject> {
    let mut data = std::collections::HashMap::new();
    data.insert(
        "eprint".to_string(),
        LoxObject::Native(NativeFunction {
            func: &eprint,
            arity: 1,
            debug: "eprint",
        }),
    );
    data.insert(
        "input".to_string(),
        LoxObject::Native(NativeFunction {
            func: &input,
            arity: 0,
            debug: "input",
        }),
    );
    data.insert(
        "print".to_string(),
        LoxObject::Native(NativeFunction {
            func: &print,
            arity: 1,
            debug: "print",
        }),
    );
    data.insert(
        "time".to_string(),
        LoxObject::Native(NativeFunction {
            func: &time,
            arity: 0,
            debug: "time",
        }),
    );
    data.insert(
        "object".to_string(),
        LoxObject::Object(
            [(
                "$new".to_string(),
                LoxObject::Native(NativeFunction {
                    arity: 0,
                    func: &object_constructor,
                    debug: "object constructor",
                }),
            )]
            .iter()
            .cloned()
            .collect(),
        ),
    );
    data
}
