use grammar::{Grammar, ProductionRule, LexicalUnit, RuleComponent};

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

                        let lu = LexicalUnit{
                            name: name.clone(),
                            regex: regex.clone(),
                        };

                        if !self.grammar.lexical_unit_exist(&lu) {
                            self.grammar.add_lexical_unit(lu);
                            RuleComponent::LexicalUnit(name)
                        }
                        else {
                            let lu = self.grammar.get_lexical_unit_by_regex(&regex).unwrap();
                            RuleComponent::LexicalUnit(lu.name.clone())
                        }
                    },
                    _ => {return Err(format!("line {}: '{}' is unexpected", self.ctx.row, component_str))}
                };

                if let RuleComponent::Epsilon = component {
                    if production_rule.rule.len() > 0 {
                        return Err(format!("Cannot use epsilon in the middle of a production rule, for rule {}", production_rule.name));
                    }
                }
                production_rule.rule.push(component);
            }

            self.grammar.add_production_rule(production_rule);
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

            if self.grammar.lexical_unit_exist(&lexical_rule) {
                return Err(format!("line {} : the lexical unit {} has already been declared", self.ctx.row, lexical_rule.name));
            }

            //TODO : Implement a check by regex too
            //TODO : Check that the regex is syntacticly corret (https://crates.io/crates/regex-syntax)
            self.grammar.add_lexical_unit(lexical_rule);
            Ok(())
        }
        else {
            Err(format!("line {} : The regex is expected after ':' but nothing was found", self.ctx.row))
        }
    }
}

pub fn parse_grammar(src : String) -> Result<Grammar, String> {
    let mut parser = GrammarParser::new(src);

    if let Err(msg) = parser.parse() {
        return Err(msg);
    }

    let grammar = parser.get_resulting_grammar();

    if let Err(msg) = grammar.check_consistency() {
        return Err(msg);
    }

    if let Err(msg) = grammar.check_regexs() {
        return Err(msg);
    }

    if let Err(msg) = grammar.check_for_first_first_conflicts() {
        return Err(msg);
    }

    if let Err(msg) = grammar.check_for_first_follow_conflicts() {
        return Err(msg);
    }

    Ok(grammar)
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

#[test]
fn test_first_first_conflict_1() {
    let grammar = String::from(include_str!("../test/test_grammars/first_first_1.ll"));
    let parse_result = parse_grammar(grammar);
    assert_eq!(parse_result.unwrap_err(), "First-First conflict for the non-terminal A");
}

#[test]
fn test_first_first_conflict_2() {
    let grammar = String::from(include_str!("../test/test_grammars/first_first_2.ll"));
    let parse_result = parse_grammar(grammar);
    assert_eq!(parse_result.unwrap_err(), "First-First conflict for the non-terminal T");
}

#[test]
fn test_first_follow_conflict_1() {
    let grammar = String::from(include_str!("../test/test_grammars/first_follow_1.ll"));
    let parse_result = parse_grammar(grammar);
    assert_eq!(parse_result.unwrap_err(), "First-Follow conflict for the non-terminal T");
}

#[test]
fn test_first_follow_conflict_2() {
    let grammar = String::from(include_str!("../test/test_grammars/first_follow_2.ll"));
    let parse_result = parse_grammar(grammar);
    assert!(parse_result.is_ok());
}

#[test]
fn test_epsilon_in_the_middle() {
    let grammar = String::from(include_str!("../test/test_grammars/epsilon_in_the_middle.ll"));
    let parse_result = parse_grammar(grammar);
    assert_eq!(parse_result.unwrap_err(), "Cannot use epsilon in the middle of a production rule, for rule S");
}
