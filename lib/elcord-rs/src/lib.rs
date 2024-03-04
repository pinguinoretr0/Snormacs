use std::{
    str,
    thread,
    time::Duration
};
use emacs::{
    defun,
    Env,
    Result,
    Value
};
use xcb::{
    x,
    Connection
};

// GPL License Code
emacs::plugin_is_GPL_compatible!();
#[emacs::module(name = "elcord-rs")]
fn init(env: &Env) -> Result<Value<'_>> {
    // in *Messages* buffer print everything loaded fine
    env.message("Elcord-RS Initialization Status: [O K]\nLoaded \'Elcord-RS\'!")
}

// prints boolean value if Xorg is active or not
fn check_xorg_status() -> &'static str {
    if let Ok(_) = Connection::connect(None) {
        "(Elcord-RS)Xorg_Status: Stable connection"
    } else {
        "(Elcord-RS)Xorg_Status: Cannot connect"
    }
}

fn print_xorg_title(env: &Env) -> Result<Value<'_>> {
    let (conn, _) = xcb::Connection::connect(None)?;

    let setup = conn.get_setup();

    let wm_client_list = conn.send_request(&x::InternAtom {
        only_if_exists: true,
        name: "_NET_CLIENT_LIST".as_bytes(),
    });
    let wm_client_list = conn.wait_for_reply(wm_client_list)?.atom();
    assert!(wm_client_list != x::ATOM_NONE, "EWMH not supported");
    
    let mut last_title = String::new();

    loop {
        let mut titles = Vec::new();

        for screen in setup.roots() {
            let window = screen.root();
            let pointer = conn.send_request(&x::QueryPointer { window });
            let pointer = conn.wait_for_reply(pointer)?;

            if pointer.same_screen() {
                let list = conn.send_request(&x::GetProperty {
                    delete: false,
                    window,
                    property: wm_client_list,
                    r#type: x::ATOM_NONE,
                    long_offset: 0,
                    long_length: 100,
                });
                let list = conn.wait_for_reply(list)?;

                for client in list.value::<x::Window>() {
                    let cookie = conn.send_request(&x::GetProperty {
                        delete: false,
                        window: *client,
                        property: x::ATOM_WM_NAME,
                        r#type: x::ATOM_STRING,
                        long_offset: 0,
                        long_length: 1024,
                    });
                    let reply = conn.wait_for_reply(cookie)?;
                    let title = reply.value();
                    let title = String::from_utf8_lossy(title).to_string();
                    titles.push(title);
                }
            }
        }

        let current_win = titles.join("");
        
        if current_win != last_title {
            println!("(Elcord-RS)Xorg_Win_Status: {}", current_win);
            //  env.message(&format!("(Elcord-RS)Xorg_Win_Status: {}", current_win));
            last_title = current_win.clone();
            break;
        }

        // adjust the sleep duration according to your needs
        //thread::sleep(Duration::from_secs(5));
    }
    env.message(&format!("(Elcord-RS)Xorg_Win_Status: {}", last_title))
}

#[defun]
fn main(env: &Env) -> Result<Value<'_>> {
    env.message(&format!("{}", check_xorg_status()))
}
// grabs title of current X window & prints it
// #[defun]
// fn print_xorg_title(env: &Env) -> Result<Value<'_>> {
//     let (conn, _) = xcb::Connection::connect(None).unwrap();
//     let setup = conn.get_setup();

//     let wm_client_list = conn.send_request(&x::InternAtom {
//         only_if_exists: true,
//         name: "_NET_CLIENT_LIST".as_bytes(),
//     });
//     let wm_client_list = conn.wait_for_reply(wm_client_list)?.atom();
//     assert!(wm_client_list != x::ATOM_NONE, "EWMH not supported");

//     let mut titles = Vec::new();

//     for screen in setup.roots() {
//         let window = screen.root();

//         let pointer = conn.send_request(&x::QueryPointer { window });
//         let pointer = conn.wait_for_reply(pointer)?;

//         if pointer.same_screen() {
//             let list = conn.send_request(&x::GetProperty {
//                 delete: false,
//                 window,
//                 property: wm_client_list,
//                 r#type: x::ATOM_NONE,
//                 long_offset: 0,
//                 long_length: 100,
//             });
//             let list = conn.wait_for_reply(list)?;

//             for client in list.value::<x::Window>() {
//                 let cookie = conn.send_request(&x::GetProperty {
//                     delete: false,
//                     window: *client,
//                     property: x::ATOM_WM_NAME,
//                     r#type: x::ATOM_STRING,
//                     long_offset: 0,
//                     long_length: 1024,
//                 });
//                 let reply = conn.wait_for_reply(cookie)?;
//                 let title = reply.value();
//                 let title = str::from_utf8(title).expect("invalid UTF-8");
//                 let title_display = format!("{}", title);
//                 titles.push(title_display);
//             }
//         }
//     }

//     let current_win = titles.join("");
//     println!("(Elcord-RS)Xorg_Win_Status: {current_win}");
//     env.message(&format!("(Elcord-RS)Xorg_Win_Status: {}", current_win))
// }
