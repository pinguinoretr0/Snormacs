/* Snormacs-RS is a Rust library dedicated to add custom
fucntionalites to Snormacs. - Created by TheLinuxPirate */
use std::{
    thread,
    time::Duration
};

use emacs::{
    defun,
    Env,
    Result,
    Value
};
use rodio::{
    Sink,
    OutputStream,
    Decoder
};

// plugin is GPL License compatible == true
emacs::plugin_is_GPL_compatible!();
// tell snormacs that the rust library is loaded 
#[emacs::module(name = "snormacs-rs")]
fn init(env: &Env) -> Result<Value<'_>> {
    env.message("Snormacs-RS Initialization Status: [O K]\nLoaded \"Snormacs-RS\"")
}

// custom function where usr can define a string to be displayed in modeline
#[defun]
fn str_msg(env: &Env, msg: String) -> Result<Value<'_>> {
    env.message(&format!("{}", msg))
}

#[defun]
fn playme(env: &Env, file_path: String) -> Result<Value<'_>> {
    let (_stream, stream_handle) = OutputStream::try_default().unwrap();
    let sink = Sink::try_new(&stream_handle).unwrap();

    let file = std::fs::File::open(file_path.clone()).unwrap();
    let source = Decoder::new(std::io::BufReader::new(file)).unwrap();

    sink.append(source);
    thread::sleep(Duration::from_secs(10));
    println!("SnormacsRS-Playme: completed...");
    sink.stop();
    env.message(&format!("Played: {}", file_path.to_string()))
}
