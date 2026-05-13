use std::io::{self, Write};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    loop {
        {
            print!("db> ");
            io::stdout().flush()?;
        }
        {
            let mut input = String::new();
            match io::stdin().read_line(&mut input) {
                Ok(_) => {
                    let input = input.trim();
                    if input.is_empty() {
                        continue;
                    }
                }
                Err(error) => {
                    eprintln!("Error reading input: {}", error);
                    break;
                }
            }
        }
    }
    Ok(())
}
