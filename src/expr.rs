extern crate nom;

use super::node::Node;
use nom::branch::alt;
use nom::character::complete::*;
use nom::bytes::complete::tag;
use nom::combinator::{map,value,recognize,opt};
use nom::multi::{many0,many1,separated_list0,separated_list1};
use nom::sequence::{delimited, pair,preceded,terminated,tuple};
use nom::IResult;

pub fn parse(input:&str)->IResult<&str,Box<Node>>{
    func_bind(input)
 }
 
 fn func_bind(input:&str)->IResult<&str,Box<Node>>{
    let LET = delimited(space0, tag("let"), space0);
    let EQ = delimited(space0, tag("="), space0);
    map(pair(delimited(LET,ident,EQ),expr),|node|{
       let (name,body) = node;
       Box::new(Node::FuncBind(name,body))
    })(input)
 }
 fn expr(input:&str)->IResult<&str,Box<Node>>{
    condition_expr(input)
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
    alt((
         literal,
         ident,
         literal_expr,
         delimited(multispace0, delimited(char('('), expr, char(')')), multispace0),
      ))(input)
 }
 fn literal_expr(input:&str)->IResult<&str,Box<Node>>{
    alt((
       pattern_def,
       lambda_def
    ))(input)
 }
 fn pattern_def(input:&str)->IResult<&str,Box<Node>>{
    
    let space_ignored_char = |c:char|{delimited(multispace0, char(c), multispace0)};
    let left_enclosure = space_ignored_char('{');
    let right_enclosure = space_ignored_char('}');
    let comma = space_ignored_char(',');
    alt((
       map(delimited(left_enclosure, separated_list1(comma, pattern_def), right_enclosure),|nodes|{
          Box::new(Node::PatternDef(nodes))
       }),
    ))(input)
 }
 fn lambda_def<'a>(input:&'a str)->IResult<&str,Box<Node>>{
    let space_ignored_char = |c:char|{delimited(multispace0, char(c), multispace0)};
    let space_ignored_tag = |s:&'a str|{delimited(multispace0, tag(s), multispace0)};
    let header = preceded(space_ignored_tag("\\"),separated_list0(space_ignored_char(','),pattern_match));
    let conditional_case = pair(delimited(space_ignored_char('|'), separated_list1(space_ignored_char(','), pattern_match), space_ignored_tag("->")),expr);
    let default_case = preceded(space_ignored_tag("->"), expr);
    //handle newline
    map(tuple((terminated(header,opt(newline)), many0(terminated(conditional_case,space_ignored_char('\n'))),terminated(default_case,space_ignored_char('\n')))),|nodes|{
       let(param_patterns,conditional_patterns,default) = nodes;
       Box::new(Node::LambdaDef(param_patterns,conditional_patterns,default))
    })(input)
 }
 fn pattern_match(input:&str)->IResult<&str,Box<Node>>{
    let space_ignored_char = |c:char|{delimited(multispace0, char(c), multispace0)};
 
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
       delimited(multispace0, recognize(pair(alt((tag("_"),alpha1)),many0(alt((tag("_"),alphanumeric1))))), multispace0), 
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
         delimited(space0, digit1, space0),
         |literal:&str|{
             Box::new(Node::Integer(literal.parse::<i64>().unwrap()))
         })(input)
 }
 
 fn boolean(input:&str)->IResult<&str,Box<Node>>{
     alt((
        value(Box::new(Node::Boolean(true)),delimited(space0,tag("true"),space0)),
        value(Box::new(Node::Boolean(false)),delimited(space0,tag("false"),space0))
     ))(input)
 }