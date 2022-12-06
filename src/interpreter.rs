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
   match input.as_str().trim_end() {
       "exit" => {
           return InterpreterResponse::Stop;
       }
       _ => {
           print!("echo:{}", input);
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
