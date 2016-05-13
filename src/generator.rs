/*
Copyright (c) 2016 Antoine Wacheux

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

extern crate liquid;

use grammar::{LexicalUnit, ProductionRule, RuleComponent, Grammar, FirstElement};
use self::liquid::{Renderable, Context, Value, FilterError};
use std::fs::File;
use std::io;
use std::io::Write;
use liquidobject::LiquidObject;
use std::path::Path;

static template : &'static str = include_str!("parser.liquid");

fn initialize_filter(ctx : &mut liquid::Context) {
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
}

fn initialize_liquid_contex(grammar: &Grammar) -> liquid::Context {
    let mut ctx = Context::new();

    initialize_filter(&mut ctx);

    ctx.set_val("production_rules", Value::Array(grammar.get_all_production_rules_name()
                                                        .iter()
                                                        .map(|x| Value::Str(x.clone()))
                                                        .collect()));

    ctx.set_val("lexical_units", Value::Array(grammar.get_all_lexical_units().iter()
                                                    .map(|lu| lu.to_liquid_object())
                                                    .collect()));

    ctx
}

pub fn generate(grammar_file: String, grammar: Grammar) {

    for (rule, set) in grammar.first() {
        print!("FIRST({}) = {{ ", rule);

        for f in set.iter() {
            match f {
                &FirstElement::LexicalUnit(ref name) => {print!("{} ", name);}
                &FirstElement::Epsilon => {print!("ε ");}
            }
        }

        println!("}}", );
    }

    let tmplt = liquid::parse(template, Default::default()).unwrap();
    let output_filename = normalize_output_filename(grammar_file);

    let mut ctx = initialize_liquid_contex(&grammar);

    match tmplt.render(&mut ctx) {
        Err(msg) => println!("Error : {}", msg),
        Ok(generated_code) => {
            match save_generated_code(output_filename, generated_code.unwrap()) {
                Ok(()) => {},
                Err(err) => println!("Error : {}", err),
            }
        },
    }
}

fn normalize_output_filename(grammar_file: String) -> String {
    let path = Path::new(&grammar_file);
    let filename = path.file_stem().unwrap().to_str().unwrap();
    match path.parent() {
        None => format!("{}.rs", filename),
        Some(p) => format!("{}/{}.rs", p.to_str().unwrap(), filename)
    }

}

pub fn save_generated_code(output_filename: String, code : String) -> io::Result<()> {
    let mut f = try!(File::create(output_filename));
    try!(f.write_all(code.as_bytes()));

    Ok(())
}
