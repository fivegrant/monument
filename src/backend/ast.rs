use pest::iterators::Pairs;
use pest::Parser;
#[derive(Debug)]
#[derive(Parser)]
#[grammar = "./monument.pest"]
pub struct Monument;

pub fn add_rule<'i>(input: &'i str) {
    let tree: Pairs<'i, Rule> = Monument::parse(Rule::rule, input).expect("malformed rule");
    println!("{:?}", tree)
}
