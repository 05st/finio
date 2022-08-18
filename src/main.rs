use clap::Parser;

#[derive(Parser)]
#[clap(version, about, long_about = None)]
struct Args {
    #[clap(short, long, default_value = "./", help = "Source directory")]
    dir: String,
    
    #[clap(short, long, default_value_t = format!("a{}", std::env::consts::EXE_SUFFIX), help = "Output path")]
    out: String
}

fn main() {
    let args = Args::parse();
}
