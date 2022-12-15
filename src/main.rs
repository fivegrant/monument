use clap::{Parser, Subcommand};
extern crate pest;
#[macro_use]
extern crate pest_derive;

mod backend;

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
            println!("{:?}", backend::ast::Monument);
            println!("Passed in {}. Nothing is being done yet.", arg);
        }
        Some(Commands::Interactive) => {
            backend::interpreter::interpret();
        }
        None => {}
    }
}
