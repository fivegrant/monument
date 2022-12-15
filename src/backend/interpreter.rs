use std::io;
use std::io::Write;
use crate::backend::ast::add_rule;

enum InterpreterResponse {
   Continue,
   Stop
}

fn read_input() -> InterpreterResponse {
   let mut input = String::new();
   print!("Λ:");
   io::stdout().flush();
   io::stdin().read_line(&mut input);
   match input.as_str().trim_end() {
       "exit" => {
           return InterpreterResponse::Stop;
       }
       _ => {
           print!("echo:{}", input);
           add_rule(input.as_str().trim_end());
           return InterpreterResponse::Continue;
       }
   }
}

pub fn interpret() {
  loop {
      match read_input() {
          InterpreterResponse::Continue => {}
          InterpreterResponse::Stop => {
              println!("EOI");
              return; 
          }
      }
  }
}
