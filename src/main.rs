
use practical_quandry::expr::parse;
use practical_quandry::node::Node;
fn main() {
   let res = parse("let main = \\args -> println(hello_world)
                                  ");
   if let Ok((_,out)) = res{
      match *out{
         Node::Boolean(literal)=>{println!("Boolean: {}",literal);}
         Node::Integer(literal)=>{println!("Integer: {}",literal);}
         Node::Ident(literal)=>{println!("Ident: {}",literal);}
         _=>{println!("{:?}",out);}
      }
   }else{
      println!("Error{:?}",res);
   }
}
