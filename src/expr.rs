extern crate nom;

use crate::node::Node;
use nom::branch::alt;
use nom::character::streaming::*;
use nom::bytes::streaming::tag;
use nom::combinator::{map,value,recognize,opt};
use nom::multi::{many0,many1,separated_list0,separated_list1};
use nom::sequence::{delimited, pair,preceded,terminated};
use nom::{IResult};

pub fn parse(input:&str)->IResult<&str,Box<Node>>{
    expr(input)
 }
 
 pub fn expr(input:&str)->IResult<&str,Box<Node>>{
   assign_expr(input)
 }

 fn assign_expr(input:&str)->IResult<&str,Box<Node>>{
   let token = |word|{preceded(multispace0, word)};
   let assignment = pair(pattern_match,pair(token(tag("=")),expr));
   let decl = pair(token(tag("var")),pair(pattern_match,pair(token(tag("=")),expr)));
   alt((
      map(assignment,|(lhs,(_,rhs))|{Box::new(Node::Assign(lhs, rhs))}),
      map(decl,|(_,(lhs,(_,rhs)))|{Box::new(Node::Assign(lhs, rhs))}),
      condition_expr
   ))(input)
 }
 fn condition_expr(input:&str)->IResult<&str,Box<Node>>{
    map(pair(logical_union,opt(pair(char('?'),pair(condition_expr,pair(char(':'),condition_expr))))),
        |nested:(Box<Node>,Option<(char,(Box<Node>,(char,Box<Node>)))>)|{
          let (condi,body) = nested;
          if let Some((_,(if_true,(_,if_false)))) = body{
             Box::new(Node::Conditional(
                condi,
                if_true,
                if_false
             ))
          }else{
             condi
          }
        })(input)
 }
 fn logical_union(input:&str)->IResult<&str,Box<Node>>{
    let ops = alt((tag("||"),));
    delimited(space0, map(
       pair(logical_join,many0(pair(ops,logical_join))),
       |literals:(Box<Node>,Vec<(&str,Box<Node>)>)|{
          let (val, ops_and_nodes) = literals;
          if ops_and_nodes.len() == 0 {
             val
          }else{
             let mut cur = val;
             for (op, node) in ops_and_nodes{
                cur = match op {
                   "||"=> Box::new(Node::Or(cur,node)),
                   _   => panic!("Error: unsupported operator:{}",op)
                }
             }
             cur
          }
       }), space0)(input)
 }
 fn logical_join(input:&str)->IResult<&str,Box<Node>>{
    let ops = alt((tag("&&"),));
    delimited(space0, map(
       pair(equality,many0(pair(ops,equality))),
       |literals:(Box<Node>,Vec<(&str,Box<Node>)>)|{
          let (val, ops_and_nodes) = literals;
          if ops_and_nodes.len() == 0 {
             val
          }else{
             let mut cur = val;
             for (op, node) in ops_and_nodes{
                cur = match op {
                   "&&"=> Box::new(Node::And(cur,node)),
                   _  => panic!("Error: unsupported operator:{}",op)
                }
             }
             cur
          }
       }), space0)(input)
 }
 
 fn equality(input:&str)->IResult<&str,Box<Node>>{
    let ops = alt((tag("=="),tag("!=")));
    delimited(space0, map(pair(comparation,opt(pair(ops,comparation))),|nodes|{
       let (first,optional) = nodes;
       if let Some((op,second)) = optional{
          match op {
             "==" => Box::new(Node::Eq(first,second)),
             "!=" => Box::new(Node::Ne(first,second)),
             _  => panic!("Error: unsupported operator:{}",op)
          }
       }else{
          first
       }
    }), space0)(input)
 }
 fn comparation(input:&str)->IResult<&str,Box<Node>>{
    let ops = alt((tag("<"),tag(">"),tag("<="),tag(">=")));
    delimited(space0, map(pair(sum,opt(pair(ops,sum))),|nodes:(Box<Node>,Option<(&str,Box<Node>)>)|{
       let (first,optional) = nodes;
       if let Some((op,second)) = optional{
          match op {
             ">" => Box::new(Node::Gt(first,second)),
             "<" => Box::new(Node::Lt(first,second)),
             ">=" => Box::new(Node::Ge(first,second)),
             "<=" => Box::new(Node::Le(first,second)),
             _  => panic!("Error: unsupported operator:{}",op)
          }
       }else{
          first
       }
    }), space0)(input)
 }
 fn sum(input:&str)->IResult<&str,Box<Node>>{
    let ops = alt((char('+'),char('-')));
    delimited(space0, map(
       pair(term,many0(pair(ops,term))),
       |literals:(Box<Node>,Vec<(char,Box<Node>)>)|{
          let (val, ops_and_nodes) = literals;
          if ops_and_nodes.len() == 0 {
             val
          }else{
             let mut cur = val;
             for (op, node) in ops_and_nodes{
                cur = match op {
                   '+'=> Box::new(Node::Add(cur,node)),
                   '-'=> Box::new(Node::Sub(cur,node)),
                   _  => panic!("Error: unsupported operator:{}",op)
                }
             }
             cur
          }
       }), space0)(input)
 }
 fn term(input:&str)->IResult<&str,Box<Node>>{
    let ops = alt((char('*'),char('/'),char('%')));
    delimited(space0, map(
       pair(unary,many0(pair(ops,unary))),
       |literals:(Box<Node>,Vec<(char,Box<Node>)>)|{
          let (val, ops_and_nodes) = literals;
          if ops_and_nodes.len() == 0 {
             val
          }else{
             let mut cur = val;
             for (op, node) in ops_and_nodes{
                cur = match op {
                   '*'=> Box::new(Node::Mul(cur,node)),
                   '/'=> Box::new(Node::Div(cur,node)),
                   '%'=> Box::new(Node::Mod(cur,node)),
                   _  => panic!("Error: unsupported operator:{}",op)
                }
             }
             cur
          }
       }), space0)(input)
 }
 fn unary(input:&str)->IResult<&str,Box<Node>>{
    let ops = alt((char('-'),char('!')));
    alt((map(delimited(space0, pair(ops, call), space0),|nodes:(char,Box<Node>)|{
       let (op,node) = nodes;
       match op{
          '-' => Box::new(Node::Neg(node)),
          '!' => Box::new(Node::Not(node)),
          _  => panic!("Error: unsupported operator:{}",op)
       }
       
    }),call))(input)
 }
 fn call(input:&str)->IResult<&str,Box<Node>>{
    let callable = alt((
       ident,
       literal,
       delimited(multispace0, delimited(char('('), expr, char(')')), multispace0)
    ));
    alt((
       map(pair(callable,many1(delimited(char('('), separated_list0(delimited(multispace0,char(','),multispace0), expr), char(')')))),|nodes:(Box<Node>,Vec<Vec<Box<Node>>>)|{
          let (mut call_name,params) = nodes;
          for param_list in params{
             call_name = Box::new(Node::Call(call_name,param_list))
          }
          call_name
       }),
       atom
    ))(input)
 }
 fn atom(input:&str)->IResult<&str,Box<Node>>{
   let token = |parser|{preceded(multispace0, parser)};
    alt((
         literal,
         ident,
         literal_expr,
         delimited(token(tag("(")), expr, token(tag(")"))),
      ))(input)
 }
 fn literal_expr(input:&str)->IResult<&str,Box<Node>>{
    alt((
       pattern_def,
    ))(input)
 }
 fn pattern_def(input:&str)->IResult<&str,Box<Node>>{
    
    let space_ignored_char = |c:char|{preceded(multispace0, char(c))};
    let left_enclosure = space_ignored_char('{');
    let right_enclosure = space_ignored_char('}');
    let comma = space_ignored_char(',');
    alt((
       map(delimited(left_enclosure, separated_list1(comma, pattern_def), right_enclosure),|nodes|{
          Box::new(Node::PatternDef(nodes))
       }),
    ))(input)
 }
 
 fn pattern_match(input:&str)->IResult<&str,Box<Node>>{
    let space_ignored_char = |c:char|{preceded(multispace0, char(c))};
 
    let left_enclosure = space_ignored_char('{');
    let right_enclosure = space_ignored_char('}');
    let comma = space_ignored_char(',');
    let alias = terminated(opt(ident),space_ignored_char('@'));
    let struct_match = map(pair(alias,delimited(left_enclosure, separated_list1(comma, pattern_match), right_enclosure)),|nodes|{
       let (name,pat) = nodes;
       if let Some(alias_name) = name{
          Box::new(Node::StructMatch(alias_name,pat))
       }else{
          Box::new(Node::StructMatch(Box::new(Node::Ident(String::from(""))),pat))   
       }
       
    });
 
    let left_enclosure_list = space_ignored_char('[');
    let right_enclosure_list = space_ignored_char(']');
    let column = space_ignored_char(':');
    let alias_list = terminated(opt(ident),space_ignored_char('@'));
    let list_match = map(pair(alias_list,delimited(left_enclosure_list, separated_list1(column, pattern_match), right_enclosure_list)),|nodes|{
       let (name,pat) = nodes;
       if let Some(alias_name) = name{
          Box::new(Node::ListMatch(alias_name,pat))
       }else{
          Box::new(Node::ListMatch(Box::new(Node::Ident(String::from(""))),pat))   
       }
       
    });
 
    alt((
       struct_match,
       list_match,
       ident,
       literal
    ))(input)
 }
 fn ident(input:&str)->IResult<&str,Box<Node>>{
 
    map(
       preceded(multispace0, recognize(pair(alt((tag("_"),alpha1)),many0(alt((tag("_"),alphanumeric1)))))), 
       |literal:&str|{
          Box::new(Node::Ident(String::from(literal)))
       }
    )(input)
 }
 
 fn literal(input:&str)->IResult<&str,Box<Node>>{
    alt((
       integer,
       boolean,
    ))(input)
 }
 fn integer(input:&str)->IResult<&str,Box<Node>>{
     map(
         preceded(multispace0, digit1),
         |literal:&str|{
             Box::new(Node::Integer(literal.parse::<i64>().unwrap()))
         })(input)
 }
 
 fn boolean(input:&str)->IResult<&str,Box<Node>>{
   let token = |word|{preceded(multispace0, tag(word))};
     alt((
        value(Box::new(Node::Boolean(true)),token("true")),
        value(Box::new(Node::Boolean(false)),token("fasle"))
     ))(input)
 }
