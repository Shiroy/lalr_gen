extern crate liquid;

use grammar::{LexicalUnit, ProductionRule, RuleComponent, Grammar};
use self::liquid::{Renderable, Context, Value, FilterError};

static template : &'static str = include_str!("parser.liquid");

pub fn generate(grammar: Grammar) {
    let tmplt = liquid::parse(template, Default::default()).unwrap();

    let mut ctx = Context::new();

    ctx.add_filter("capitalize", Box::new(|input, _args| {
        if let &Value::Str(ref s) = input {
            let res = s.chars().enumerate().map(|(i, c)| if i == 0 {
                c.to_uppercase().next().unwrap()
            } else {
                c.to_lowercase().next().unwrap()
            }).collect();

            Ok(Value::Str(res))
        } else {
            Err(FilterError::InvalidType("Expected a string".to_owned()))
        }
    }));

    ctx.set_val("production_rules", Value::Array(grammar.get_all_production_rules_name()
                                                        .iter()
                                                        .map(|x| Value::Str(x.clone()))
                                                        .collect()));

    match tmplt.render(&mut ctx) {
        Err(msg) => println!("Error : {}", msg),
        Ok(generated_code) => save_generated_code(generated_code.unwrap()),
    }
}

pub fn save_generated_code(code : String) {
    println!("{}", code);
}
