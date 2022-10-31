use clap::{Parser, Subcommand};

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
