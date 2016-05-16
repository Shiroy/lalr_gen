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

mod grammar;
mod grammar_parser;
mod generator;
mod liquidobject;

use std::env;
use std::fs::File;
use std::io::Read;

fn load_grammar(filename : &String) -> Result<String, String> {
    let mut f = match File::open(filename) {
        Err(io_error) => {
            return Err(io_error.to_string());
        },
        Ok(f) => f,
    };

    let mut grammar_content = String::new();
    match f.read_to_string(&mut grammar_content) {
        Err(io_error) => {
            return Err(io_error.to_string());
        },
        Ok(_) => {},
    };

    Ok(grammar_content)
}

fn main() {
    let mut args = env::args();

    let grammar_file = match args.nth(1) {
        Some(f) => f,
        None => {
            println!("Error : You must provide a grammar file to parse");
            return;
        }
    };

    let grammar_content = match load_grammar(&grammar_file) {
        Ok(s) => s,
        Err(msg) => {
            println!("Error : {}", msg);
            return;
        }
    };

    match grammar_parser::parse_grammar(grammar_content) {
        Ok(g) => generator::generate(grammar_file, g),
        Err(msg) => {println!("Error : {}", msg); return;},
    };
}
