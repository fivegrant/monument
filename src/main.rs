pub mod ast;
pub mod interpreter;

use clap::{Parser, Subcommand};
extern crate pest;
#[macro_use]
extern crate pest_derive;


#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    Compile {
        #[arg(short, long)]
        arg: String,
    },
    Interactive
}

fn main() {
    let cli = Cli::parse();
    match &cli.command {
        Some(Commands::Compile { arg }) => {
            println!("{:?}", ast::Monument);
            println!("Passed in {}. Nothing is being done yet.", arg);
        }
        Some(Commands::Interactive) => {
            interpreter::interpret();
        }
        None => {}
    }
}
