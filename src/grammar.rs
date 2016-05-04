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

pub struct LexicalUnit {
    name : String,
    regex : String,
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
    grammar: Grammar
}

impl GrammarParser {
    fn new(src: String) -> GrammarParser {
        GrammarParser {
            ctx : ParserContext::new(src),
            grammar: Grammar::new(),
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
                        let name = format!("auto_{}", regex);

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

            self.grammar.lexical_units.push(lexical_rule);
            Ok(())
        }
        else {
            Err(format!("line {} : The regex is expected after ':' but nothing was found", self.ctx.row))
        }
    }
}

pub fn parse_grammar(src : String) -> Result<Grammar, String> {
    let mut parser = GrammarParser::new(src);

    match parser.parse() {
        Ok(()) => Ok(parser.get_resulting_grammar()),
        Err(msg) => Err(msg)
    }
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
