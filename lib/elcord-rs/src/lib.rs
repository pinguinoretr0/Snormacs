// TODO | After Snormacs-RS come back and finish this
use emacs::{defun, Env, Result, Value};

// Emacs won't load the module without this.
emacs::plugin_is_GPL_compatible!();

// Register the initialization hook that Emacs will call when it loads the module.
#[emacs::module]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Loaded \"Elcord-RS\"")
}

// Define a function callable by Lisp code.
#[defun]
fn init_message(env: &Env, str: String) -> Result<Value<'_>> {
    env.message(&format!("{}", str))
}
