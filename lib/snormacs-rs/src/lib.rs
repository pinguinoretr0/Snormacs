// Snormacs-RS is a Rust library dedicated to add custom \
// fucntionalites to Snormacs. - Created by TheLinuxPirate
use emacs::{defun, Env, Result, Value};

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Loaded \"Snormacs-RS\"")
}

// File Type Buffer Reader
// Sends a message according to file type. (Demo Code)
#[defun]
fn read_buffer_fs(env: &Env) -> Result<()> {
		let current_buf = env.call("buffer-name", &[])?;

		// Check if the buffer has a rust file type
    if current_buf.to_string().ends_with(".rs") {
				env.message(&format!("Hello from Rust! This is a Rust buffer."));
    } else { env.message(&format!("Basic Bitch")); }

    Ok(())
}
