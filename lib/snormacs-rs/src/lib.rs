// Snormacs-RS is a Rust library dedicated to add custom \
// fucntionalites to Snormacs. - Created by TheLinuxPirate
use emacs::{
		defun,
		Env,
		Result,
		Value
};

// plugin is GPL License compatible == true
emacs::plugin_is_GPL_compatible!();

// tell snormacs that the rust library is loaded 
#[emacs::module]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Loaded \"Snormacs-RS\"")
}

// the init message that says everything is loaded correctly
#[defun]
fn init_msg(env: &Env) -> Result<Value<'_>> {
		let init_msg: &str = "Hello, World!";
		env.message(&format!("{}", init_msg))
}

// custom function where usr can define a string to be displayed in modeline
#[defun]
fn str_msg(env: &Env, msg: String) -> Result<Value<'_>> {
    env.message(&format!("{}", msg))
}
