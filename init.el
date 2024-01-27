  (defvar elpaca-installer-version 0.6)
  (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
  (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
  (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
  (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                                :ref nil
                                :files (:defaults (:exclude "extensions"))
                                :build (:not elpaca--activate-package)))
  (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
         (build (expand-file-name "elpaca/" elpaca-builds-directory))
         (order (cdr elpaca-order))
         (default-directory repo))
    (add-to-list 'load-path (if (file-exists-p build) build repo))
    (unless (file-exists-p repo)
      (make-directory repo t)
      (when (< emacs-major-version 28) (require 'subr-x))
      (condition-case-unless-debug err
          (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                   ((zerop (call-process "git" nil buffer t "clone"
                                         (plist-get order :repo) repo)))
                   ((zerop (call-process "git" nil buffer t "checkout"
                                         (or (plist-get order :ref) "--"))))
                   (emacs (concat invocation-directory invocation-name))
                   ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                         "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                   ((require 'elpaca))
                   ((elpaca-generate-autoloads "elpaca" repo)))
              (kill-buffer buffer)
            (error "%s" (with-current-buffer buffer (buffer-string))))
        ((error) (warn "%s" err) (delete-directory repo 'recursive))))
    (unless (require 'elpaca-autoloads nil t)
      (require 'elpaca)
      (elpaca-generate-autoloads "elpaca" repo)
      (load "./elpaca-autoloads")))
  (add-hook 'after-init-hook #'elpaca-process-queues)
  (elpaca `(,@elpaca-order))

  (elpaca elpaca-use-package
    (elpaca-use-package-mode)
    (setq elpaca-use-package-by-default t))
  (elpaca-wait)
  (defun +elpaca-unload-seq (e)
   (and (featurep 'seq) (unload-feature 'seq t))
    (elpaca--continue-build e))

  (defun +elpaca-seq-build-steps ()
    (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                         elpaca--pre-built-steps elpaca-build-steps))
            (list '+elpaca-unload-seq 'elpaca--activate-package)))

  (use-package seq :elpaca `(seq :build ,(+elpaca-seq-build-steps)))

  (use-package no-littering
  :elpaca t
  :config
  (setq no-littering-etc-directory
      (expand-file-name ".config/" user-emacs-directory))
  (setq no-littering-var-directory
      (expand-file-name ".data/" user-emacs-directory)))

  (set-default-coding-systems 'utf-8)
  (add-to-list 'default-frame-alist '(font . "Comic Mono:9" )) ;; Sets font to Comic Mono (comment this line for default)
  (add-hook 'prog-mode-hook 'global-display-line-numbers-mode)
  (add-hook 'text-mode-hook 'visual-line-mode)
  (global-hl-line-mode 1)

  (setq-default cursor-in-non-selected-windows nil)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)

  (electric-pair-mode 1)
  (show-paren-mode 1) 

  (load "~/.emacs.d/lisp/elisp.el")
  (load "~/.emacs.d/lisp/home.el") ;; EXWM Configuration 

  (add-hook 'evil-write-post-hook #'snor/untabify-on-save)
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

  (add-to-list 'load-path "~/.emacs.d/lib/target/debug/") ;; Rust libraries $PATHs
  (add-to-list 'load-path "~/.emacs.d/lib/snormacs-rs/")
  
  (load-library "libsnormacs_rs") ;; Load the rust libraries
  (require 'snormacs-rs)
 ;; (snormacs-rs-init-msg)

  (use-package doom-themes
    :elpaca t
    :init (load-theme 'doom-tokyo-night t) ;; tokyo-night is the main theme
    :config
    (setq doom-themes-enable-bold t    
          doom-themes-enable-italic t))

  ;; Completion Setup
  (use-package ivy :elpaca t :init (ivy-mode))
  (use-package projectile :elpaca t)

  (use-package dashboard
    :elpaca t
    :after projectile
    :config
    (dashboard-setup-startup-hook)
    (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
    ;; Set the title
    (setq dashboard-banner-logo-title "Welcome to Snormacs")
    ;; Set the banner
    (setq dashboard-startup-banner "~/.emacs.d/.custom/.dashboard_logos/pingu.txt")

    ;; Content is not centered by default. To center, set
    (setq dashboard-center-content t)
    (setq dashboard-show-shortcuts t)

    (setq dashboard-items '((recents  . 5)
                            (bookmarks . 5)
                            (projects . 5)
                            (agenda . 5)
                            (registers . 5)))

    (setq dashboard-icon-type 'all-the-icons) 
    (setq dashboard-display-icons-p t)
    (setq dashboard-icon-type 'nerd-icons)
    
    (setq dashboard-set-navigator t)
    (setq dashboard-set-init-info t)

    (setq dashboard-set-init-info t)
    (setq dashboard-set-footer t)
    (setq dashboard-footer-messages '("\“An idiot admires complexity, a genius admires simplicity\" - Terry A. Davis"))

    (setq dashboard-week-agenda t) ;; Org Agenda
    (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda))

  (use-package which-key
    :elpaca t
    :config
    (setq which-key-idle-delay 0.2)
    :init (which-key-mode))
  (use-package beacon :elpaca t :init (beacon-mode))

  (use-package doom-modeline
    :elpaca t
    :init (doom-modeline-mode)
    :custom
    (doom-modeline-height 28)
    (doom-modeline-bar-width 6)

    (doom-modeline-env-version t)
    (doom-modeline-hud t)
    (doom-modeline-lsp t)
    (doom-modeline-github t)
    (doom-modeline-minor-modes nil)
    (doom-modeline-major-mode-icon t)
    (doom-modeline-enable-word-count t)
    (doom-modeline-buffer-file-name-style 'truncate-with-project))

  (use-package rainbow-mode :elpaca t :config (add-hook 'prog-mode-hook (lambda () (rainbow-mode))))

  ;; Remember to M-x all-the-icons-install-fonts & nerd-icons-install-fonts
  (use-package treemacs :elpaca t) ;; Required here or else a dependency blockage accurs
  (use-package all-the-icons :elpaca t :if (display-graphic-p))
  (use-package nerd-icons :elpaca t)
  (use-package treemacs-all-the-icons :elpaca t :config (treemacs-load-theme "all-the-icons"))

  (use-package vterm :elpaca t)

  (use-package calfw :elpaca t)
  (use-package calfw-org
    :elpaca t
    :config
    (setq cfw:org-agenda-schedule-args '(:timestamp))) ;; TODO // Create calendar setup

  (use-package typo :elpaca t :init (typo-global-mode 1))
  (use-package speed-type :elpaca t)

  (use-package magit :elpaca t)
  (use-package restart-emacs :elpaca t)
  (use-package crux :elpaca t)
  (use-package eat :elpaca t)

  (use-package literate-calc-mode :elpaca t :init (literate-calc-mode))
  (use-package move-text :elpaca t)
  (use-package aggressive-indent
    :elpaca t
    :config
    (global-aggressive-indent-mode 1)
    (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

  (use-package pdf-tools :elpaca t)
  (use-package pdf-view-restore
    :after pdf-tools
    :config
    (setq pdf-view-restore-filename "~/.emacs.d/.custom/.pdf-view-restore")
    (add-hook 'pdf-view-mode-hook 'pdf-view-restore-mode))

  (use-package clipmon
    :elpaca t
    :init (clipmon-mode)
    :config
    (add-to-list 'after-init-hook 'clipmon-persist)
    (setq savehist-autosave-interval (* 7 60))
    (setq clipmon-timer-interval 2)
    (setq clipmon-autoinsert-color "green")
    (setq clipmon-autoinsert-timeout 8))

  (use-package perspective
    :elpaca t
    :custom
    (persp-mode-prefix-key (kbd "C-."))
    (persp-initial-frame-name "1")
    :init (persp-mode))

  (use-package avy :elpaca t)
  (use-package ace-jump-buffer :elpaca t)
  (use-package vimish-fold :elpaca t :init (vimish-fold-global-mode 1))

  (use-package sublimity
    :elpaca t
    :config
    (setq sublimity-scroll-vertical-frame-delay 0.01)
    (setq sublimity-scroll-weight 5
        sublimity-scroll-drift-length 10))

  (use-package ranger
    :elpaca t
    :init (ranger-override-dired-mode t)
    :config
    (setq ranger-cleanup-eagerly t)
    (setq ranger-modify-header t)
    (setq ranger-show-hidden t))

  (use-package multiple-cursors :elpaca t)
  (use-package sudo-edit :elpaca t)
  (use-package sudo-utils :elpaca t)
  (use-package elcord :elpaca t :init (elcord-mode))

  (use-package tree-sitter :elpaca t :init (global-tree-sitter-mode))
  (use-package tree-sitter-langs :elpaca t)

  (use-package lsp-mode
    :elpaca t
    :init (setq lsp-keymap-prefix "C-c l")
    (add-hook 'prog-mode-hook #'lsp)
    (add-hook 'lsp-mode #'lsp-enable-which-key-integration)
    :config
    (setq lsp-warn-no-matched-clients nil)
    :commands lsp)
  ;; Technically "Extra" LSP Packages
  (use-package lsp-ui :elpaca t :commands lsp-ui-mode) 
  (use-package lsp-ivy :elpaca t :commands lsp-ivy-workspace-symbol)
  (use-package lsp-treemacs :elpaca t :commands lsp-treemacs-errors-list)
  (use-package dap-mode :elpaca t)
  (use-package company-box :elpaca t :config (add-hook 'company-mode #'company-box-mode))
  (use-package company :elpaca t :config (add-hook 'prog-mode-hook #'global-company-mode))

  (use-package paredit ;; The most useful shit for LISP (wraps parentheses & quotes)
    :elpaca t
    :init (autoload 'enable-paredit-mode "paredit" t)
    :config
    (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
    (add-hook 'ielm-mode-hook #'enable-paredit-mode)
    (add-hook 'lisp-mode-hook #'enable-paredit-mode)
    (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
    (add-hook 'scheme-mode-hook #'enable-paredit-mode)
    (add-hook 'yuck-mode-hook #'enable-paredit-mode))

  (use-package yuck-mode :elpaca t)
  (use-package nix-mode :elpaca t)
  (use-package json-mode :elpaca t)
  
  (use-package rust-mode :elpaca t :config (add-hook 'rust-mode-hook #'cargo-minor-mode))
  (use-package cargo :elpaca t)
  (use-package go-mode :elpaca t)
  (use-package zig-mode :elpaca t)

  (use-package gdscript-mode :elpaca t)
  (use-package typescript-mode :elpaca t)
  (use-package npm :elpaca t)
  (use-package kotlin-mode :elpaca t)

  (use-package nim-mode :elpaca t)  
  (use-package lua-mode :elpaca t)

  (use-package haskell-mode :elpaca t)
  (use-package fsharp-mode :elpaca t)
  (use-package elixir-mode :elpaca t)
  (use-package clojure-mode :elpaca t)

  (use-package geiser :elpaca t)
  (use-package geiser-guile :elpaca t)
  

  (use-package go-translate
    :elpaca t
    :config
    (setq gts-translate-list '(("en" "ja") ("en" "es"))) ;; Add a longer list if you want to

    (setq gts-default-translator
          (gts-translator
           :picker (gts-prompt-picker)
           :engines (list (gts-bing-engine) (gts-google-engine))
           :render (gts-buffer-render))))

  (use-package general
    :elpaca t
    :config
    (general-evil-setup)
    (setq evil-want-keybinding nil)

    ;; Leader Keys Setup 
    (general-create-definer snor/leader-mappings-norm
      :states  'normal
      :keymaps 'override
      :prefix  ";") 

    (general-create-definer snor/leader-mappings-vis
      :states  'visual
      :keymaps 'override
      :prefix  ";")

    ;; Local-Leader Key  
    (general-create-definer snor/localleader-mappings-norm
      :states  'normal 
      :keymaps 'override
      :prefix  "SPC")

    ;; Key-Chord Bindings
    (general-create-definer snor/chord-mappings :keymaps 'override) 

    ;; God Mode Setup
    (general-create-definer snor/GOD :keymaps 'override)

    
    (snor/leader-mappings-norm
      ;; Buffer Management
      "j"       '(:ignore t                 :wk "Buffer KeyChords")
      "j s"     '(ace-jump-buffer           :wk "Switch to an Active Buffer")
      "j r"     '(revert-buffer             :wk "Reload Current Buffer")
      "j k"     '(kill-current-buffer       :wk "Kills Current Buffer")
      "j f"     '(ibuffer-list-buffers      :wk "List Buffers")
      "j <tab>" '(switch-to-prev-buffer     :wk "Switch to Previous Buffer")
      "j SPC"   '(switch-to-next-buffer     :wk "Switch to Next Buffer")

      ;; D | Leader
      "d"       '(:ignore t                    :wk "Section D")

      "d r"     '(restart-emacs                :wk "Restarts Emacs")

      "d l"     '(:ignore t                    :wk "Literate Calc")
      "d l l"   '(literate-calc-eval-buffer    :wk "Evaluates Buffer")
      "d l j"   '(literate-calc-clear-overlays :wk "Clears Current Buffer Evaluations")

      "d f"     '(:ignore t                    :wk "File Options")
      "d f d"   '(delete-file                  :wk "Select A File To Delete")
      "d f r"   '(rename-file                  :wk "Select A File To Rename")
      "d f c"   '(copy-file                    :wk "Select A File To Copy")
      "d f t"   '(move-file-to-trash           :wk "Select A File To Trash")

      ;; God-Mode Settings
      "g"       '(:ignore t                 :wk "GOD MODE MAPPINGS")
      "g g"     '(snor/evil-god-mode-all    :wk "SWITCH TO GOD MODE GLOBAL")
      "g l"     '(snor/evil-god-local-mode  :wk "SWITCH TO GOD MODE BUFFER")
      "g j"     '(evil-execute-in-god-state :wk "EXECUTE CMD IN GOD STATE")
      "g ?"     '(snor/god-mode-manual      :wk "OPEN GOD MODE MANUAL")

      ;; Root
      "s"       '(:ignore t                 :wk "Options as Root")
      "s e"     '(sudo-edit                 :wk "Open Current File as Root")
      "s f"     '(sudo-edit-find-file       :wk "Find File as Root")

      ;; Org
      "o"       '(:ignore t                 :wk "Org Mode Options")
      "o df"    '(org-babel-tangle          :wk "Babel Tangle File")

      ;; Misc
      "f"       '(find-file                 :wk "Find & Open File"))

    (snor/leader-mappings-vis
      ;; Visual Mode Leader Mappings
      "v"   '(:ignore t                     :wk "Visual Mode Bindings")   
      "v t" '(gts-do-translate              :wk "Translates Region")
      "v T" '(untabify                      :wk "Removes <Tab> From Region")    
      "v j" '(crux-upcase-region            :wk "Converts Region To Uppercase")
      "v k" '(crux-downcase-region          :wk "Converts Region To Lowercase")   
      "v f" '(comment-or-uncomment-region   :wk "Comments/Uncomments Region"))

    (snor/localleader-mappings-norm
      ;; WINDOW MANAGEMENT
      "s"   '(:ignore t                 :wk "Split Windows Prefix")
      "s s" '(split-window-vertically   :wk "Split Window Horizontally")
      "s h" '(split-window-horizontally :wk "Split Window Vertically")

      "h"   '(windmove-left             :wk "Move Window Focus to the Left")
      "j"   '(windmove-down             :wk "Move Window Focus to the Down")
      "k"   '(windmove-up               :wk "Move Window Focus to the Up")
      "l"   '(windmove-right            :wk "Move Window Focus to the Right")

      "s k" '(delete-window             :wk "Delete Current Window")

      ;; Calendar
      "c"   '(:ignore t                 :wk "Calendar Options")
      "c l" '(cfw:open-org-calendar     :wk "Launches Org-Calendar")

      ;; Treemacs
      "T"   '(treemacs                                :wk "Toggle Treemacs")
      "t a" '(treemacs-add-project-to-workspace       :wk "Adds Project to Treemacs")
      "t d" '(treemacs-remove-project-from-workspace  :wk "Removes Project from Treemacs")
      "t r" '(treemacs-rename-project                 :wk "Renames Treemacs Project")
      "t c" '(treemacs-collapse-project               :wk "Collapses Treemacs Project")

      ;; Misc
      "f" '(vimish-fold-toggle           :wk "Toggle Code Fold")     
      "F" '(vimish-fold-toggle-all       :wk "Toggle Code Fold")

      ;; Workspaces/Persp-Mode
      "<tab>"   '(:ignore t    :wk "Workspaces")

      "<tab> 1" '(snor/switch-to-workspace-01 :wk "Switch to Main Workspace")
      "<tab> 2" '(snor/switch-to-workspace-02 :wk "Switch to Workspace 2")
      "<tab> 3" '(snor/switch-to-workspace-03 :wk "Switch to Workspace 3")
      "<tab> 4" '(snor/switch-to-workspace-04 :wk "Switch to Workspace 4")
      "<tab> 5" '(snor/switch-to-workspace-05 :wk "Switch to Workspace 5")
      "<tab> 6" '(snor/switch-to-workspace-06 :wk "Switch to Workspace 6")
      "<tab> 7" '(snor/switch-to-workspace-07 :wk "Switch to Workspace 7")
      "<tab> 8" '(snor/switch-to-workspace-08 :wk "Switch to Workspace 8")
      "<tab> 9" '(snor/switch-to-workspace-09 :wk "Switch to Workspace 9")
      "<tab> 0" '(snor/switch-to-workspace-10 :wk "Switch to Workspace 10")

      "<tab> k" '(persp-next   :wk "Switch to Next Workspace")
      "<tab> j" '(persp-prev   :wk "Switch to Previous Workspace")
      "<tab> q" '(persp-kill   :wk "Kill A Workspace"))

    (snor/GOD
      ;; Movement
      "C-1" '(backward-char             :wk "Move Backward")
      "C-2" '(next-line                 :wk "Move Down")
      "C-3" '(forward-char              :wk "Move Foward")
      "C-o" '(previous-line             :wk "Move Up")
      
      "C-4" '(move-end-of-line          :wk "Move to the End of the Line")
      "C-`" '(move-beginning-of-line    :wk "Move to the Start of the Line")
      
      "C-x" '(:ignore t                 :wk "Action Key Prefix")
      "C-W" '(move-beginning-of-line    :wk "Move to the Start of the Line")

      "C-?" '(snor/god-mode-manual      :wk "Opens God-Mode Manual")
      "C-;" '(snor/become-human         :wk "Return to Human State"))

    (snor/chord-mappings
      ;; LSP-Mode Movement
      "M-j" '(company-select-next       :wk "Company-Mode Down")
      "M-k" '(company-select-previous   :wk "Company-Mode Up")

      ;; Misc
      "M-RET" '(vterm              :wk "Spawn Terminal")))

  (use-package evil
    :elpaca t
    :init (evil-mode)
    :config
    (setq-default tab-width 2)
    (setq-default evil-shift-width tab-width)

    (evil-define-key 'insert 'global (kbd "M-e") 'evil-normal-state)
    (evil-define-key 'god global-map [escape] 'evil-god-state-bail))
    ;; Extra stuff for Evil
    (use-package evil-god-state :elpaca t :after evil)
    (use-package evil-collection :elpaca t :after evil)

  (use-package god-mode
    :elpaca t
    :after evil
    :config
    (setq god-exempt-major-modes nil)
    (setq god-exempt-predicates nil)
    (setq god-mode-enable-function-key-translation nil))

  (use-package hydra :elpaca t)

  (use-package org
    :elpaca t
    :init (org-mode)
    :config
    ;;(evil-define-key 'normal 'global (kbd "<tab>") 'org-cycle)

    (setq org-src-preserve-indentation t)
    (setq org-startup-indented t)           
    (setq org-startup-with-inline-images t)
    (setq org-src-fontify-natively t))

  (use-package org-roam :elpaca t :after org)

  (use-package org-superstar 
    :elpaca t
    :after org-roam
    :config (add-hook 'org-mode-hook (lambda () (org-superstar-mode))))

  (use-package org-present :elpaca t :after org-roam)

    (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(elcord-editor-icon "emacs_material_icon")
   '(elcord-idle-message "Playing Melee...")
   '(elcord-idle-timer 500)
   '(elcord-quiet t)
   '(elcord-refresh-rate 1)
   '(warning-suppress-log-types
     '((org-element-cache)
       (org-element-cache)
       (org-element-cache)))
   '(warning-suppress-types '((org-element-cache) (org-element-cache))))
