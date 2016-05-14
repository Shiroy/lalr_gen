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
extern crate regex_syntax;
extern crate core;

use liquidobject::LiquidObject;
use self::liquid::Value;
use std::collections::HashMap;
use std::collections::HashSet;
use self::regex_syntax::Expr;
use self::core::iter::FromIterator;

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
    LexicalUnit(String),
    Epsilon
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum FirstElement { //Elements that appear in the first sets
    LexicalUnit(String),
    Epsilon
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum FollowElement {
    LexicalUnit(String),
    EndOfString
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

    fn first(&self) -> HashMap<String, HashSet<FirstElement> > {
        let mut result : HashMap<String, HashSet<FirstElement> > = HashMap::new();

        for rule in self.get_all_production_rules_name() {
            result.insert(rule, HashSet::new());
        }

        let mut modified = true;

        while modified {
            modified = false;
            //println!("Iteration because of modified");

            for rule in &self.production_rules {
                let mut epsilon_found = true;
                let mut rule_node_iter = rule.rule.iter();
                let mut new_first_set : HashSet<FirstElement> = {
                    let mut set = HashSet::new();
                    let first_set = result.get(&rule.name).unwrap();
                    set = set.union(&first_set).map(|x| x.clone()).collect();
                    set
                };

                while epsilon_found {
                    //println!("Iteration because of epsilon_found");
                    epsilon_found = false;

                    if let Some(rule_node) = rule_node_iter.next() {
                        match rule_node {
                            &RuleComponent::LexicalUnit(ref name) => {
                                modified = modified || new_first_set.insert(FirstElement::LexicalUnit(name.clone()));
                            },
                            &RuleComponent::Epsilon => {
                                modified = modified || new_first_set.insert(FirstElement::Epsilon);
                            }
                            &RuleComponent::ProductionRule(ref name) => {
                                let first_set = result.get(name).unwrap();
                                //Check if we have an epsilon
                                epsilon_found = first_set.iter().any(|x| match x {
                                    &FirstElement::Epsilon => true,
                                    _ => false,
                                });

                                for elem in first_set {
                                    if let &FirstElement::LexicalUnit(ref name) = elem {
                                        //println!("Inserting {}", name);
                                        modified = modified || new_first_set.insert(elem.clone());
                                    }
                                }
                            }
                        };
                    }
                    result.insert(rule.name.clone(), new_first_set.clone());
                }
            }
        }

        result
    }

    fn follow(&self, first : &HashMap<String, HashSet<FirstElement>>) -> HashMap<String, HashSet<FollowElement> > {
        let mut result : HashMap<String, HashSet<FollowElement>> = HashMap::new();

        for rule_name in self.get_all_production_rules_name() {
            result.insert(rule_name.clone(), {
                if rule_name == self.axiom {
                    let mut set = HashSet::new();
                    set.insert(FollowElement::EndOfString);
                    set
                }
                else {
                    HashSet::new()
                }
            });

        }

        let mut modified = true;

        while modified {
            modified = false;

            for rule in &self.production_rules {
                let mut rule_node_iter = rule.rule.iter().peekable();

                loop {
                    if let Some(rule_node) = rule_node_iter.next() {
                        match rule_node {
                            &RuleComponent::ProductionRule(ref pr_name) => {
                                let mut new_follow_set = {
                                    let mut set = HashSet::new();
                                    let follow_set = result.get(pr_name).unwrap();
                                    set = set.union(&follow_set).map(|x| x.clone()).collect();
                                    set
                                };

                                if let Some(next_elem) = rule_node_iter.peek() { //Case A -> aBb => Add FIRST(b) to FOLLOW(B) except epsilon
                                    if let &&RuleComponent::ProductionRule(ref b_name) = next_elem {
                                        let first_b = first.get(b_name).unwrap();

                                        for b_elem in first_b.iter() {
                                            match b_elem {
                                                &FirstElement::LexicalUnit(ref lu_name) => {
                                                    modified = new_follow_set.insert(FollowElement::LexicalUnit(lu_name.clone())) || modified;
                                                },
                                                &FirstElement::Epsilon => { //In this case, add FOLLOW(A) to FOLLOW(B)
                                                    let follow_A = result.get(&rule.name).unwrap();
                                                    for elem in follow_A.iter() {
                                                        modified = new_follow_set.insert(elem.clone()) || modified;
                                                    }
                                                }
                                            }
                                        }

                                    } else if let &&RuleComponent::LexicalUnit(ref lu_name) = next_elem {
                                        modified = new_follow_set.insert(FollowElement::LexicalUnit(lu_name.clone())) || modified;
                                    }
                                }
                                else { // Case A -> aB => add FOLLOW(A) to FOLLOW(B)
                                    let follow_A = result.get(&rule.name).unwrap();
                                    for elem in follow_A.iter() {
                                        modified = new_follow_set.insert(elem.clone()) || modified;
                                    }
                                }

                                result.insert(pr_name.clone(), new_follow_set.clone());
                            },
                            _ => {}
                        }
                    }
                    else {
                        break;
                    }
                }
            }
        }

        result
    }

    pub fn first_and_follow(&self) ->  (HashMap<String, HashSet<FirstElement> >, HashMap<String, HashSet<FollowElement> >) {
        let first = self.first();
        let follow = self.follow(&first);

        (first, follow)
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
                    'a'...'z' => if component_str == "epsilon" {
                        RuleComponent::Epsilon
                    } else {
                        RuleComponent::LexicalUnit(component_str.to_owned())
                    },
                    'A'...'Z' => {RuleComponent::ProductionRule(component_str.to_owned())},
                    '\'' => {
                        let s = component_str.len();
                        if s < 3 {
                            return Err(format!("line {} : The regex in a production rule cannot be empty.", self.ctx.row));
                        }

                        let regex : String = component_str.chars()
                                                    .enumerate()
                                                    .filter(|&(i, _)| 0 < i && i < s-1)
                                                    .map(|(_, c)| c)
                                                    .collect();

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

                if self.grammar.axiom.is_empty() {
                    self.grammar.axiom = production_rule.name.clone();
                }
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
                    &RuleComponent::Epsilon => {},
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

    fn check_regexs(&self) -> Result<(), String> {
        let regex_check_job = |regex: &LexicalUnit| match Expr::parse(&regex.regex) {
                Ok(_) => Ok(()),
                Err(err) => Err(format!("Error in regex {} : {}", regex.name, err)),
            };

        self.grammar.lexical_units.iter().map(regex_check_job).fold(Ok(()), |acc, x| acc.and(x))
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

    if let Err(msg) = parser.check_regexs() {
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
