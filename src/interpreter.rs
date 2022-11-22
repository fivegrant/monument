use std::io;
use std::io::Write;

enum InterpreterResponse {
   Continue,
   Stop
}

fn read_input() -> InterpreterResponse {
   let mut input = String::new();
   print!("Λ:");
   io::stdout().flush();
   io::stdin().read_line(&mut input);
   match *input.as_str() {
       "exit" => {
           return InterpreterResponse::Stop;
       }
       _ => {
           println!("echo:{}", input);
           return InterpreterResponse::Continue;
       }
   }
}

pub fn interpret() {
  loop {
      match read_input() {
          Continue => {}
          Stop => {
              println!("EOI");
              break; 
          }
      }
  }
}
