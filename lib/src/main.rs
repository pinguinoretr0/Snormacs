use clap::Parser;
use regex::Regex;
use std::{
		env,
		fs,
		fs::File,
		io::{self, Read},
		process::Command,
};

/// Snormacs CLI Program written in Rust
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
		/// Installation Mode 
		#[arg(short, long, default_value_t = 0)]
		installer: u8,
		/// Pattern to match
		#[arg(short, long)]
		parallel: String,
		/// File path to search
		#[arg(short, long)]
		path: String
}

fn program_check(program: &str) -> bool {
    if let Ok(path) = env::var("PATH") {
        for p in path.split(":") {
            let p_str = format!("{}/{}", p, program);
            if fs::metadata(p_str).is_ok() {
                return true;
            }
        }
    }
    false
}

fn main() {
    let args = Args::parse();

		// Installer Options
		if args.installer != 0 {
				let pkg: &str = "git";
				if program_check(pkg) {
						println!("(Program \"git\" is installed!)");
				} else {
						println!("Please install \"git!\"");
				}

				let mut fetch_repo = Command::new("git");
				fetch_repo.args(&["clone", "https://github.com/thelinuxpirate/Snormacs", ".emacs.d/"]);
				let mut fetch_nix = Command::new("sh"); // Single User
				fetch_nix.args(&["<(curl -L https://nixos.org/nix/install)", "--no-daemon"]); 
				let mut install_snormacs = Command::new("mv");
				install_snormacs.args(&[".emacs.d", "~/"]);
				
				match args.installer { // Uni
						1 => {
								println!("=+Snormacs Installer+=\nInstalling Snormacs;");
								println!("Fetching Snormacs...");								
								let _ = fetch_repo.output();
								println!("Done!\nNow removing the Git repository");
								let _ = install_snormacs.output();
								println!("Merged Snormacs into ~/.emacs.d\n[DONE]");
						},

						2 => { // Made for Non-NixOS Distros
								let pkg: &str = "wget";
								if program_check(pkg) {
										println!("(Program \"wget\" is installed!)");
								} else {
										println!("Please install \"wget!\"");
								}

								println!("=+Snormacs Installer; V2+=\nInstalling Nix;");
								let _ = fetch_nix.output();
								println!("Nix has been installed succesfully; make sure to add it to your $PATH\n \
													Now installing Snormacs...");

								println!("Fetching Snormacs...");								
								let _ = fetch_repo.output();
								println!("Done!\nNow removing the Git repository");
								let _ = install_snormacs.output();
								println!("Merged Snormacs into ~/.emacs.d\n[DONE]");
						},
						_ => println!("Invalid Parameter; 1 & 2 are your options"),
				}
		}

		// Parallel Command (Regex)
		let path = File::open(&args.path);
    let mut content = String::new();
		path.to_string();
    path.read_to_string(&mut content);

    let pattern = Regex::new(&args.parallel).unwrap();
    let count = pattern.find_iter(&content).count();

    println!("Number of matches: {}", count);
}
