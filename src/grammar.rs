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

use liquidobject::LiquidObject;
use self::liquid::Value;
use std::collections::HashMap;

pub struct LexicalUnit {
    name : String,
    regex : String,
}

impl LiquidObject for LexicalUnit {
    fn to_liquid_object(&self) -> Value {
        let mut result = HashMap::new();
        result.insert("name".to_owned(), liquid::Value::Str(self.name.clone()));
        result.insert("regex".to_owned(), liquid::Value::Str(self.regex.clone()));
        Value::Object(result)
    }
}

pub struct ProductionRule {
    name: String,
    rule: Vec<RuleComponent>
}

pub enum RuleComponent {
    ProductionRule(String),
    LexicalUnit(String)
}

pub struct Grammar {
    lexical_units : Vec<LexicalUnit>,
    production_rules: Vec<ProductionRule>,
    axiom : String
}

impl Grammar {
    fn new() -> Grammar {
        Grammar {
            lexical_units: Vec::new(),
            production_rules: Vec::new(),
            axiom: String::new(),
        }
    }

    fn get_production_rule(&self, name: &String) -> Vec<&ProductionRule> {
        self.production_rules.iter().filter(|x| x.name == *name).collect()
    }

    pub fn get_all_production_rules_name(&self) -> Vec<String> {
        let uniq_filter = {
            let mut found : Vec<String> = Vec::new();

            move |x:&String| {
                if found.iter().find(|elem| *elem == x).is_some() {
                    false
                }
                else {
                    found.push(x.clone());
                    true
                }
            }
        };

        self.production_rules.iter().map(|x| x.name.clone()).filter(uniq_filter).collect()
    }

    pub fn get_all_lexical_units(&self) -> &Vec<LexicalUnit> {
        &self.lexical_units
    }
}

//Methods
//get_rule(name: String&) -> Vec<ProductionRule>&

struct ParserContext {
    src: String,
    row: u32,
    col: u32,
    str_pos: usize,
}

impl ParserContext {
    fn new(src : String) -> ParserContext {
        ParserContext {
            src: src,
            row: 1,
            col: 0,
            str_pos: 0
        }
    }

    fn next_word(&mut self) -> Option<String> {
        let mut iter = self.src.chars();
        let mut ch = iter.nth(self.str_pos);
        let mut c : char;
        let mut read_word = String::new();

        match ch {
            None => { return None },
            Some(chr) => { c = chr}
        }

        macro_rules! next_char {
            ($err:stmt) => ({
                self.str_pos += 1;
                ch = iter.next();

                match ch {
                    None => { $err },
                    Some(chr) => { c = chr }
                }

                match c { //TODO : Implement a better line/column tracking and forward this information in the word
                    ' ' | '\t' => self.col += 1,
                    '\n' => self.row += 1,
                    _ => {}
                }
            });
        }

        while (c == ' ') || (c == '\n') || (c == '\t') { //We skip leading whitespaces
            next_char! { return None }
        }

        while !((c == ' ') || (c == '\n') || (c == '\t')) {
            read_word.push(c);
            next_char! { return Some(read_word) }
        }

        Some(read_word)
    }

    pub fn read_line(&mut self) -> Option<String> {
        let mut iter = self.src.chars();
        let mut ch = iter.nth(self.str_pos);
        let mut c : char;
        let mut read_word = String::new();

        match ch {
            None => { return None },
            Some(chr) => { c = chr}
        }

        macro_rules! next_char {
            ($err:stmt) => ({
                self.str_pos += 1;
                ch = iter.next();

                match ch {
                    None => { $err },
                    Some(chr) => { c = chr }
                }

                match c {
                    ' ' | '\t' => self.col += 1,
                    '\n' => self.row += 1,
                    _ => {}
                }
            });
        }

        while (c == ' ') || (c == '\n') || (c == '\t') { //We skip leading whitespaces
            next_char! { return None }
        }

        while c != '\n' {
            read_word.push(c);
            next_char! { return Some(read_word) }
        }

        Some(read_word)
    }
}

struct GrammarParser {
    ctx: ParserContext,
    grammar: Grammar,
    auto_counter: u32, //Counter for generating automatic lexical unit name
}

impl GrammarParser {
    fn new(src: String) -> GrammarParser {
        GrammarParser {
            ctx : ParserContext::new(src),
            grammar: Grammar::new(),
            auto_counter: 1,
        }
    }

    fn get_resulting_grammar(self) -> Grammar {
        self.grammar
    }

    fn parse(&mut self) -> Result<(), String> {
        while let Some(str_word) = self.ctx.next_word() {
            match str_word.chars().next().unwrap() {
                'A'...'Z' => {
                    match self.parse_grammar_rule(str_word) {
                        Ok(()) => {},
                        Err(msg) => {return Err(msg)},
                    }
                },
                'a'...'z' => {
                    match self.parse_lexical_unit(str_word) {
                        Ok(()) => {},
                        Err(msg) => {return Err(msg)},
                    }
                }
                _ => { return Err(String::from("Unknown token : ") + str_word.as_str())}
            }
        }

        Ok(())
    }

    fn parse_grammar_rule(&mut self, name : String) -> Result<(), String> {
        match self.ctx.next_word() {
            None => {return Err(format!("line {} : Expected '->'", self.ctx.row))},
            Some(token) => {
                if token != "->" {
                    return Err(format!("line {} : Expected '->' found '{}'", self.ctx.row, token));
                }
            }
        }

        if let Some(rule_str) = self.ctx.read_line() {
            let mut production_rule = ProductionRule{
                name: name,
                rule: Vec::new()
            };

            for comp_str in rule_str.split_whitespace() {
                let component_str = String::from(comp_str);
                let component = match component_str.chars().next().unwrap() {
                    'a'...'z' => {RuleComponent::LexicalUnit(component_str.to_owned())},
                    'A'...'Z' => {RuleComponent::ProductionRule(component_str.to_owned())},
                    '\'' => {
                        let s = component_str.len();
                        if s < 3 {
                            return Err(format!("line {} : The regex in a production rule cannot be empty.", self.ctx.row));
                        }

                        let regex = unsafe {component_str.slice_unchecked(1, s-1) }; //TODO : Remove this unsafe
                        let name = format!("auto_{}", self.auto_counter);
                        self.auto_counter += 1;

                        if self.grammar.lexical_units.iter().find(|rule| rule.name == name).is_none() {
                            self.grammar.lexical_units.push(LexicalUnit{name:name.clone(), regex:String::from(regex)});
                        }

                        RuleComponent::LexicalUnit(name)
                    },
                    _ => {return Err(format!("line {}: '{}' is unexpected", self.ctx.row, component_str))}
                };

                production_rule.rule.push(component);
            }
            self.grammar.production_rules.push(production_rule);
            Ok(())
        }
        else {
            Err(format!("A production rule cannot be empty"))
        }
    }

    fn parse_lexical_unit(&mut self, name : String) -> Result<(), String> {
        //Let's read the ':'
        match self.ctx.next_word() {
            None => {return Err(format!("line {} : Expected ':'", self.ctx.row))},
            Some(token) => {
                if token != ":" {
                    return Err(format!("line {} : Expected ':' found '{}'", self.ctx.row, token));
                }
            }
        }

        //Ok, now the regex
        if let Some(regex) = self.ctx.read_line() {
            let lexical_rule = LexicalUnit {
                name: name,
                regex: regex,
            };

            if self.grammar.lexical_units.iter().find(|rule| rule.name == lexical_rule.name).is_some() {
                return Err(format!("line {} : the lexical unit {} has already been declared", self.ctx.row, lexical_rule.name));
            }

            //TODO : Implement a check by regex too
            //TODO : Check that the regex is syntacticly corret (https://crates.io/crates/regex-syntax)
            self.grammar.lexical_units.push(lexical_rule);
            Ok(())
        }
        else {
            Err(format!("line {} : The regex is expected after ':' but nothing was found", self.ctx.row))
        }
    }

    fn check_consistency(& self) -> Result<(), String> {
        for production_rule in &self.grammar.production_rules {
            for rule_component in &production_rule.rule {
                match rule_component {
                    &RuleComponent::ProductionRule(ref name) => {
                        if self.grammar.production_rules.iter().find(|r| *name == r.name).is_none() {
                            return Err(format!("The production rule '{}' does not exist", name));
                        }
                    },
                    &RuleComponent::LexicalUnit(ref name) => {
                        if self.grammar.lexical_units.iter().find(|lu| *name  == lu.name).is_none() {
                            return Err(format!("The lexical unit '{}' does not exist", name));
                        }
                    },
                }
            }
        }

        Ok(())
    }

    fn check_for_left_recursion(& self) -> Result<(), String> {
        self.grammar.production_rules.iter()
            .map(|p| self.begin_with_production_rule(&p.name, &p.name))
            .fold(Ok(()), |acc, x| acc.and(x))
    }

    fn begin_with_production_rule(& self, name: &String, production_to_look_in: &String) -> Result<(), String> {
        for p in self.grammar.get_production_rule(production_to_look_in) {
            let first_component = &p.rule.iter().next().unwrap();
            if let &RuleComponent::ProductionRule(ref p_name) = *first_component {
                if name == p_name {
                    return Err(format!("Left recursion for the production rule {}", name));
                }
                else {
                    return self.begin_with_production_rule(name, p_name);
                }
            }
        }

        Ok(())
    }
}

pub fn parse_grammar(src : String) -> Result<Grammar, String> {
    let mut parser = GrammarParser::new(src);

    if let Err(msg) = parser.parse() {
        return Err(msg);
    }

    if let Err(msg) = parser.check_consistency() {
        return Err(msg);
    }

    if let Err(msg) = parser.check_for_left_recursion() {
        return Err(msg);
    }

    Ok(parser.get_resulting_grammar())
}

#[test]
fn test_next_word() {
    let mut ctx = ParserContext::new("test and run".to_owned());

    assert_eq!(ctx.next_word().unwrap(), "test");
    assert_eq!(ctx.next_word().unwrap(), "and");
    assert_eq!(ctx.next_word().unwrap(), "run");
}

#[test]
fn test_read_line() {
    let mut ctx = ParserContext::new("line 1\nline2\nline3\nline4".to_owned());
    ctx.next_word();

    assert_eq!(ctx.read_line().unwrap(), "1");
    assert_eq!(ctx.read_line().unwrap(), "line2");
    assert_eq!(ctx.read_line().unwrap(), "line3");
    assert_eq!(ctx.read_line().unwrap(), "line4");
}
