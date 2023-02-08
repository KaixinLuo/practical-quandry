extern crate nom;

use crate::atom::atom;
use crate::node::Node;
use nom::branch::{alt, permutation};
use nom::character::complete::*;
use nom::bytes::complete::tag;
use nom::combinator::{map,value,recognize,opt};
use nom::multi::{many0,many1,separated_list0, separated_list1};
use nom::sequence::{delimited, pair,preceded, self};
use nom::{IResult};
use std::iter;

pub fn parse(input:&str)->IResult<&str,Box<Node>>{
    expr(input)
 }
 
 pub fn expr(input:&str)->IResult<&str,Box<Node>>{
   call(input)
 }

 fn assign_expr(input:&str)->IResult<&str,Box<Node>>{
   let token = |word|{preceded(multispace0, word)};
   let assignment = pair(atom,pair(token(tag("=")),expr));
   let decl = pair(token(tag("var")),pair(atom,pair(token(tag("=")),expr)));
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
   let token = 
      |parser|{
         preceded(multispace0, parser)};
   alt((map(
      pair(
         opt(mem_access),
         delimited(
            token(tag("(")), 
            separated_list0(token(tag(",")), expr), 
            token(tag(")")))), 
      |(optional_name,param_list)|{
         Box::new(Node::Call(optional_name, param_list))
      }),mem_access))(input)
 }
fn mem_access(input:&str)->IResult<&str,Box<Node>>{
   let token = |parser|{
      preceded(multispace0, parser)};
   
   let call_postfix = 
   map(delimited(
      token(char('(')), 
      separated_list0(
         token(char(',')),
         expr), 
      token(char(')'))),
      |params|{
         |name|{
            Box::new(Node::Call(name, params))}});

   let member_postfix = map(pair(token(char('.')),atom),|(_,field_name)|{|var_name|{Box::new(Node::MemberOf(var_name, field_name))}});
   let index_postfix = map(delimited(
      token(char('[')), 
         expr, 
      token(char(']'))),|idx|{|arr_name|{Box::new(Node::IndexOf(arr_name, idx))}});
   map(pair(atom,many1(permutation((
      index_postfix,
      call_postfix,
      member_postfix,
   )))),|(base,params)|{
      let mut res = base;
      for param in params{
         res = param(res);
      }
      res
   })(input)
}
fn indexing(input:&str)->IResult<&str,Box<Node>>{
   let token = |parser|{
      preceded(multispace0, parser)};

   map(
      pair(
         atom,
         many1(delimited(
            token(tag("[")), 
            expr, 
            token(tag("]"))))
      ),
      |(first,second)|{
         Box::new(Node::IndexOf(first, second))
      }
   )(input)
}
 fn member(input:&str)->IResult<&str,Box<Node>>{
   let token = 
   |parser|{
      preceded(multispace0, parser)};
   map(
      pair(
         atom,
         pair(
            token(char('.')),
            mem_access)
      ),|(op1,(_,op2))|{Box::new(Node::MemberOf(op1, op2))})(input)
 }

#[cfg(test)]
mod tests{
   use super::*;
   #[test]
   fn test_member_ok(){
      assert_eq!(expr("arr.length"), Ok(("",Box::new(
         Node::MemberOf(
               Box::new(
                  Node::Ident(String::from("arr"))),
                  Box::new(Node::Ident(String::from("length"))))))));
               }
   #[test]
   fn test_index_ok(){
      assert_eq!(indexing("arr[1][2][34]"), Ok(("",Box::new(
         Node::IndexOf(
               Box::new(
                  Node::Ident(String::from("arr"))),
                  vec![Box::new(Node::Integer(1)),Box::new(Node::Integer(2)),Box::new(Node::Integer(34))])))));
               }

   #[test]
   fn test_call_ok(){
      assert_eq!(expr("func(1,2,34)"), Ok(("",Box::new(
         Node::Call(
            Option::Some(
               Box::new(
                  Node::Ident(String::from("func")))),
                  vec![Box::new(Node::Integer(1)),
                     Box::new(Node::Integer(2)),
                     Box::new(Node::Integer(34))])))));

   }

   #[test]
   fn test_call_ok_with_space(){
      assert_eq!(expr("func( \n1,\t2,\r34)"), Ok(("",Box::new(
         Node::Call(
            Option::Some(
               Box::new(
                  Node::Ident(String::from("func")))),
                  vec![Box::new(Node::Integer(1)),
                     Box::new(Node::Integer(2)),
                     Box::new(Node::Integer(34))])))));

   }
   #[test]
   fn test_call_ok_with_space_noparam(){
      assert_eq!(expr("func( \n\t\r)"), Ok(("",Box::new(
         Node::Call(
            Option::Some(
               Box::new(
                  Node::Ident(String::from("func")))),
                  vec![])))));

   }
}