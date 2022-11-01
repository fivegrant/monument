pub mod ast;

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
    Run {
        #[arg(short, long)]
        arg: String,
    },
    Compile
}

fn main() {
    let cli = Cli::parse();
    println!("{:?}", ast::Monument);
    match &cli.command {
        Some(Commands::Run { arg }) => {
            println!("Passed in {}. Nothing is being done yet.", arg);
        }
        Some(Commands::Compile) => {
            println!("Compiler called")
        }
        None => {}
    }
}
