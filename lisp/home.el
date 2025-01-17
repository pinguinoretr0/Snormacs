  ;; Elcord-RS | Custom Elcord code in Rust | Check the *Messages* buffer for more information
  (add-to-list 'load-path "~/.emacs.d/lib/target/debug/") ;; Sets up binary loading
  (add-to-list 'load-path "~/.emacs.d/lib/elcord-rs/") ;; Rust library path
  (load-library "libelcord_rs") ;; Loads custom elcord library
  (require 'elcord-rs) ;; requires custom elcord library for usage
  (elcord-rs-main) ;; states everything is ok upon startup 
 ;; (elcord-rs-print-xorg-window) ;; prints current active window 

  ;; EXWM Plugins
  (use-package perspective-exwm :elpaca t)
  (use-package buffer-move :elpaca t)

  ;; Custom Functions
  (defun snor/desktop-manual () (interactive)
         (get-buffer-create "Snormacs/Desktop-Manual")
         (switch-to-buffer "Snormacs/Desktop-Manual") 

         (find-file "~/.emacs.d/.custom/Desktop-Manual.org")
         (org-present)

         (message "Here is the Manual!")
         (read-only-mode)) 

  (defun snor/autostart (cmd)
    "Start CMD unless already running."
    (let ((buf-name (concat "*" cmd "*")))
      (unless (process-live-p (get-buffer-process buf-name))
        (start-process-shell-command cmd buf-name cmd))))

  (defun snor/set-command (command &optional buffer)
    "Start shell COMMAND in the background. If BUFFER is provided, log process output to that buffer."
    (interactive (list (read-shell-command "Run: ")))
    (start-process-shell-command command buffer command))

  (defun exwm/run-in-background (command)
    (let ((command-parts (split-string command "[ ]+")))
      (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

  (defun snor/switch-start (buffer cmd)
    "Switch to buffer with name BUFFER or start one with CMD."
    (if-let (b (get-buffer buffer)) 
        (switch-to-buffer b)
      (snor/set-command cmd)))

  (defun snor/update-wallpaper () (interactive)
    (start-process-shell-command
     "feh" nil "feh --bg-scale ~/Pictures/Wallpapers/space.jpg"))

  (defun snor/exwm-input-set-key (key command)
    "Similar to `exwm-input-set-key', but always refreshes prefix keys.
    This allows defining keys from any place in config."
    (exwm-input-set-key key command)
    ;; Alternatively, try general-setq (which calls customize handler)
    (exwm-input--update-global-prefix-keys))

  (setq snor/barp nil)

  (defun snor/kill-bar () (interactive)
    (when snor/barp
      (ignore-errors
        (kill-process snor/barp)))
    (setq snor/barp nil))

  (defun snor/set-bar () (interactive)
    (snor/kill-bar)
    (setq snor/barp (start-process-shell-command "polybar" nil "polybar exwm-bar")))

  (defun snor/exwm-init-hook ()
    (snor/set-command "xset r rate 200 60" nil) ;; Set shitty default X keyboard rate to be faster
    (snor/set-command "xset b off" nil) ;; Disable annoying X beep noise on some systems
    (run-at-time "2 sec" nil (lambda () (snor/update-wallpaper)))
    (snor/set-command "picom --daemon" nil)
    (snor/set-bar)
    (snor/set-command "dunst" nil))

  (use-package exwm
    :elpaca t
    :init
    ;; Stops asking to replace current Window Manager, if there is a current session
    (setq exwm-replace nil) 
    (setq mouse-autoselect-window nil
          focus-follows-mouse t)
    (perspective-exwm-mode)
    :config
    (setq exwm-workspace-number 8)
    ;; The next two make all buffers available on all workspaces
    (setq exwm-workspace-show-all-buffers t)
    (setq exwm-layout-show-all-buffers t)

    ;; Start the init-hook & hide modeline on all X Windows
    (add-hook 'exwm-init-hook #'snor/exwm-init-hook)
    (add-hook 'exwm-floating-setup-hook
              (lambda ()
                (exwm-layout-hide-mode-line)))
  
    (setq perspective-exwm-override-initial-name
          '((0 . "home")
            (1 . "web")
            (2 . "term")
            (3 . "music")
            (4 . "discrd")
            (5 . "audio")
            (6 . "game")
            (7 . "ctrl")
            (8 . "&othr")
            (9 . "dev")))

    ;; Make class name the buffer name
    (add-hook 'exwm-update-class-hook
              (lambda ()
                (exwm-workspace-rename-buffer exwm-class-name)))

  (defun exwm/run-term ()
    (interactive)
    (start-process-shell-command
     "terminal" nil "kitty")
    (exwm-workspace-switch-create 2)
    (exwm-layout-toggle-mode-line))
  
  (defun exwm/run-browser ()
    (interactive)
    (start-process-shell-command
     "browser" nil "firefox-developer-edition")
    (exwm-workspace-switch-create 1)
    (exwm-layout-toggle-mode-line))

  (defun exwm/run-discord ()
    (interactive)
    (start-process-shell-command
     "discord" nil "discord")
    (exwm-workspace-switch-create 4))

  (defun exwm/run-spotify ()
    (interactive)
    (call-process-shell-command "spotify" nil 0)
    (exwm-workspace-switch-create 3))

  (defun exwm/run-pavucontrol ()
    (interactive)
    (start-process-shell-command
     "ctrl" nil "pavucontrol")
    (exwm-workspace-switch-create 5))

  (defun exwm/run-emu-dolphin ()
    (interactive)
    (start-process-shell-command
     "gamecube" nil "dolphin-emu")
    (exwm-workspace-switch-create 6))
  
  (defun exwm/run-slippi ()
    (interactive)
    (start-process-shell-command
     "slippi-launcher" nil "./System/Applications/Slippi/Slippi-Launcher.AppImage")
    (exwm-workspace-switch-create 6))

  (defun exwm/run-gd ()
    (interactive)
    (start-process-shell-command
     "godot" nil "./System/Applications/Godot/GD-Linux.x86_64")
    (exwm-workspace-switch-create 9))

  ;; Move Window to Workspace Functions
  (defvar workspace-number)
  (defun exwm/move-win-to-0 ()
    (interactive)
    (setq workspace-number 0)
    (let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
          (id (exwm--buffer->id (window-buffer))))
      (exwm-workspace-move-window frame id)))

  (defun exwm/move-win-to-1 ()
    (interactive)
    (setq workspace-number 1)
    (let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
          (id (exwm--buffer->id (window-buffer))))
      (exwm-workspace-move-window frame id)))

  (defun exwm/move-win-to-2 ()
    (interactive)
    (setq workspace-number 2)
    (let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
          (id (exwm--buffer->id (window-buffer))))
      (exwm-workspace-move-window frame id)))

  (defun exwm/move-win-to-3 ()
    (interactive)
    (setq workspace-number 3)
    (let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
          (id (exwm--buffer->id (window-buffer))))
      (exwm-workspace-move-window frame id)))

  (defun exwm/move-win-to-4 ()
    (interactive)
    (setq workspace-number 4)
    (let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
          (id (exwm--buffer->id (window-buffer))))
      (exwm-workspace-move-window frame id)))

  (defun exwm/move-win-to-5 ()
    (interactive)
    (setq workspace-number 5)
    (let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
          (id (exwm--buffer->id (window-buffer))))
      (exwm-workspace-move-window frame id)))

  (defun exwm/move-win-to-6 ()
    (interactive)
    (setq workspace-number 6)
    (let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
          (id (exwm--buffer->id (window-buffer))))
      (exwm-workspace-move-window frame id)))

  (defun exwm/move-win-to-7 ()
    (interactive)
    (setq workspace-number 7)
    (let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
          (id (exwm--buffer->id (window-buffer))))
      (exwm-workspace-move-window frame id)))

  (defun exwm/move-win-to-8 ()
    (interactive)
    (setq workspace-number 8)
    (let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
          (id (exwm--buffer->id (window-buffer))))
      (exwm-workspace-move-window frame id)))

  (defun exwm/move-win-to-9 ()
    (interactive)
    (setq workspace-number 9)
    (let ((frame (exwm-workspace--workspace-from-frame-or-index workspace-number))
          (id (exwm--buffer->id (window-buffer))))
      (exwm-workspace-move-window frame id)))

    (setq exwm-input-prefix-keys
          '(?\s-\\ ;; Super+Space
            ?\C-\\
            ?\M-x
            ?\M-&
            ?\M-h
            ?\M-j
            ?\M-k
            ?\M-l
            ?\M-H
            ?\M-J
            ?\M-K
            ?\M-L
            ?\M-:))  

    ;; Ctrl+Q will enable the next key to be sent directly
    (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

    (setq exwm-input-global-keys
        `(;; EXWM Management
          ([?\s-r] . exwm-reset)
          ([?\s-Q] . kill-emacs)          
          ;; EXWM
          ([?\s-e] . perspective-exwm-switch-perspective)
          ([?\s-b] . exwm-layout-toggle-mode-line)
          ;; Window Management
          ([?\s-w] . kill-current-buffer)
          ([?\s-W] . kill-buffer-and-window)
          ([?\s-i] . split-window-vertically)
          ([?\s-o] . split-window-horizontally)
          ([?\s-f] . exwm-layout-toggle-fullscreen)
          ;; Bar Management
          ([?\s-p] . snor/set-bar)
          ([?\s-P] . snor/kill-bar)
          ;; Move Window to Workspace
          ([?\s-~] . exwm/move-win-to-0)
          ([?\s-!] . exwm/move-win-to-1)
          ([?\s-@] . exwm/move-win-to-2)
          ([?\s-#] . exwm/move-win-to-3)
          ([?\s-$] . exwm/move-win-to-4)
          ([?\s-%] . exwm/move-win-to-5)
          ([?\s-^] . exwm/move-win-to-6)
          ([?\s-&] . exwm/move-win-to-7)
          ([?\s-*] . exwm/move-win-to-8)
          ;; Window Movement
          ([?\M-h] . windmove-left)
          ([?\M-j] . windmove-down)
          ([?\M-k] . windmove-up)
          ([?\M-l] . windmove-right)
          ([?\M-H] . buf-move-left)
          ([?\M-J] . buf-move-down)
          ([?\M-K] . buf-move-up)
          ([?\M-L] . buf-move-right)
          ;; Workspaces
          ([?\s-`] . (lambda () (interactive) (exwm-workspace-switch-create 0)))
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  ;; Use input-set-key for KeyChords & other Bindings
   ;; Applications
    (exwm-input-set-key (kbd "s-<return>")   'vterm)
    (exwm-input-set-key (kbd "s-S-<return>") 'exwm/run-term)
    (exwm-input-set-key (kbd "s-SPC b")      'exwm/run-browser)
    (exwm-input-set-key (kbd "s-SPC D")      'exwm/run-discord)
    (exwm-input-set-key (kbd "s-SPC S")      'exwm/run-spotify)
    (exwm-input-set-key (kbd "s-SPC p")      'exwm/run-pavucontrol)
    (exwm-input-set-key (kbd "s-SPC d g")    'exwm/run-gd)
    (exwm-input-set-key (kbd "s-SPC E d")    'exwm/run-emu-dolphin)
    (exwm-input-set-key (kbd "s-SPC m")      'exwm/run-slippi)

    ;; EXWM Management
    (exwm-input-set-key (kbd "s-SPC s") 'switch-to-buffer)
    (exwm-input-set-key (kbd "s-SPC w") 'delete-window)

   ;; Multimedia Management
    ;; Volume
    (exwm-input-set-key (kbd "<f10>") 'desktop-environment-toggle-mute)
    (exwm-input-set-key (kbd "<f8>")  'desktop-environment-volume-decrement-slowly)
    (exwm-input-set-key (kbd "<f9>")  'desktop-environment-volume-increment-slowly)

    ;; Media Controlls
    (exwm-input-set-key (kbd "<f5>") 'desktop-environment-toggle-music)
    (exwm-input-set-key (kbd "<f6>") 'desktop-environment-music-previous)
    (exwm-input-set-key (kbd "<f7>") 'desktop-environment-music-next)

    ;; Screenshit
    (exwm-input-set-key (kbd "<f11>") 'desktop-environment-screenshot)
    (exwm-enable)) ;; Add at end of config

  (use-package desktop-environment
    :elpaca t
    :after exwm
    :config (desktop-environment-mode)
    (setq desktop-environment-brightness-small-increment "2%+")
    (setq desktop-environment-brightness-small-decrement "2%-")
    (setq desktop-environment-brightness-normal-increment "5%+")
    (setq desktop-environment-brightness-normal-decrement "5%-")
    (setq desktop-environment-screenshot-command "flameshot gui"))
