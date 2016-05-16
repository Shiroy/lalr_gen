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
extern crate core;
extern crate regex_syntax;

use liquidobject::LiquidObject;
use self::liquid::Value;
use std::collections::HashMap;
use std::collections::HashSet;
use self::regex_syntax::Expr;

#[derive(Debug)]
pub struct LexicalUnit {
    pub name : String,
    pub regex : String,
}

impl LiquidObject for LexicalUnit {
    fn to_liquid_object(&self) -> Value {
        let mut result = HashMap::new();
        result.insert("name".to_owned(), liquid::Value::Str(self.name.clone()));
        result.insert("regex".to_owned(), liquid::Value::Str(self.regex.clone()));
        Value::Object(result)
    }
}

#[derive(Debug)]
pub struct ProductionRule {
    pub name: String,
    pub rule: Vec<RuleComponent>
}

#[derive(Debug)]
pub enum RuleComponent {
    ProductionRule(String),
    LexicalUnit(String),
    Epsilon
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum SetElement { //Elements that appear in the first and follow sets
    LexicalUnit(String),
    EndOfString,
    Epsilon
}

#[derive(Debug)]
pub struct Grammar {
    lexical_units : Vec<LexicalUnit>,
    production_rules: Vec<ProductionRule>,
    axiom : String
}

impl Grammar {
    pub fn new() -> Grammar {
        Grammar {
            lexical_units: Vec::new(),
            production_rules: Vec::new(),
            axiom: String::new(),
        }
    }

    pub fn add_lexical_unit(&mut self, lu: LexicalUnit) {
        if self.lexical_units.iter().find(|rule| rule.regex == lu.regex).is_none() {
            self.lexical_units.push(lu);
        }
    }

    pub fn add_production_rule(&mut self, pr: ProductionRule) {
        if self.axiom.is_empty() {
            self.axiom = pr.name.clone();
        }
        self.production_rules.push(pr);
    }

    pub fn check_consistency(&self) -> Result<(), String> {
        for production_rule in &self.production_rules {
            for rule_component in &production_rule.rule {
                match rule_component {
                    &RuleComponent::ProductionRule(ref name) => {
                        if self.production_rules.iter().find(|r| *name == r.name).is_none() {
                            return Err(format!("The production rule '{}' does not exist", name));
                        }
                    },
                    &RuleComponent::LexicalUnit(ref name) => {
                        if self.lexical_units.iter().find(|lu| *name  == lu.name).is_none() {
                            return Err(format!("The lexical unit '{}' does not exist", name));
                        }
                    },
                    &RuleComponent::Epsilon => {},
                }
            }
        }

        Ok(())
    }

    pub fn check_regexs(&self) -> Result<(), String> {
        let regex_check_job = |regex: &LexicalUnit| match Expr::parse(&regex.regex) {
                Ok(_) => Ok(()),
                Err(err) => Err(format!("Error in regex {} : {}", regex.name, err)),
            };

        self.lexical_units.iter().map(regex_check_job).fold(Ok(()), |acc, x| acc.and(x))
    }

    pub fn lexical_unit_exist(&self, lexical_rule: &LexicalUnit) -> bool {
        self.lexical_units.iter().find(|rule| rule.regex == lexical_rule.regex).is_some()
    }

    pub fn get_lexical_unit_by_regex(&self, regex: &String) -> Option<&LexicalUnit> {
        self.lexical_units.iter().find(|rule| rule.regex == *regex)
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

    fn first(&self) -> HashMap<String, HashSet<SetElement> > {
        let mut result : HashMap<String, HashSet<SetElement> > = HashMap::new();

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
                let mut new_first_set : HashSet<SetElement> = {
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
                                modified = modified || new_first_set.insert(SetElement::LexicalUnit(name.clone()));
                            },
                            &RuleComponent::Epsilon => {
                                modified = modified || new_first_set.insert(SetElement::Epsilon);
                            }
                            &RuleComponent::ProductionRule(ref name) => {
                                let first_set = result.get(name).unwrap();
                                //Check if we have an epsilon
                                epsilon_found = first_set.iter().any(|x| match x {
                                    &SetElement::Epsilon => true,
                                    _ => false,
                                });

                                for elem in first_set {
                                    if let &SetElement::LexicalUnit(_) = elem {
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

    fn follow(&self, first : &HashMap<String, HashSet<SetElement>>) -> HashMap<String, HashSet<SetElement> > {
        let mut result : HashMap<String, HashSet<SetElement>> = HashMap::new();

        for rule_name in self.get_all_production_rules_name() {
            result.insert(rule_name.clone(), {
                if rule_name == self.axiom {
                    let mut set = HashSet::new();
                    set.insert(SetElement::EndOfString);
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
                                                &SetElement::LexicalUnit(ref lu_name) => {
                                                    modified = new_follow_set.insert(SetElement::LexicalUnit(lu_name.clone())) || modified;
                                                },
                                                &SetElement::Epsilon => { //In this case, add FOLLOW(A) to FOLLOW(B)
                                                    let follow_a = result.get(&rule.name).unwrap();
                                                    for elem in follow_a.iter() {
                                                        modified = new_follow_set.insert(elem.clone()) || modified;
                                                    }
                                                }
                                                &SetElement::EndOfString => { unreachable!(); }
                                            }
                                        }

                                    } else if let &&RuleComponent::LexicalUnit(ref lu_name) = next_elem {
                                        modified = new_follow_set.insert(SetElement::LexicalUnit(lu_name.clone())) || modified;
                                    }
                                }
                                else { // Case A -> aB => add FOLLOW(A) to FOLLOW(B)
                                    let follow_a = result.get(&rule.name).unwrap();
                                    for elem in follow_a.iter() {
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

    fn first_and_follow(&self) ->  (HashMap<String, HashSet<SetElement> >, HashMap<String, HashSet<SetElement> >) {
        let first = self.first();
        let follow = self.follow(&first);

        (first, follow)
    }

    pub fn check_for_first_first_conflicts(&self) -> Result<(), String> {

        let first = self.first();

        for rule_name in self.get_all_production_rules_name() {
            let mut accumulator_set : HashSet<SetElement> = HashSet::new();

            for rule in self.get_production_rule(&rule_name) {
                let mut node_iter = rule.rule.iter();

                let rule_node = node_iter.next().unwrap();

                match rule_node {
                    &RuleComponent::LexicalUnit(ref name) => {
                        if !accumulator_set.insert(SetElement::LexicalUnit(name.clone())) {
                            return Err(format!("First-First conflict for the non-terminal {}", rule_name));
                        }
                    },
                    &RuleComponent::ProductionRule(ref name) => { //Form A -> B...
                        let first_of_b = first.get(name).unwrap();
                        for first_elem in first_of_b {
                            if !accumulator_set.insert(first_elem.clone()) {
                                return Err(format!("First-First conflict for the non-terminal {}", rule_name));
                            }
                        }
                    },
                    _ => {} //We don't care about epsilon in First-First conflicts
                }
            }
        }

        Ok(())
    }

    pub fn check_for_first_follow_conflicts(&self) -> Result<(), String> {
        let (first, follow) = self.first_and_follow();

        for rule_name in self.get_all_production_rules_name() {
            let first_of_a = first.get(&rule_name).unwrap();
            let follow_of_a = follow.get(&rule_name).unwrap();

            if first_of_a.iter().any(|x| match x {
                &SetElement::Epsilon => true,
                &SetElement::EndOfString => { unreachable!() },
                _ => false
            }) {
                if !first_of_a.is_disjoint(&follow_of_a) {
                    return Err(format!("First-Follow conflict for the non-terminal {}", rule_name));
                }
            }
        }

        Ok(())
    }
}
