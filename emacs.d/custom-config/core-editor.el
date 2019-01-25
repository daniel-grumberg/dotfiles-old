(provide 'dang/core-editor)

;; If Emacs is started via GUI toolkit it won't source the startup scripts which might cause issues (especially on macOS)
(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-initialize))

;; Preferred editor behaviors, this prevents backup files on auto-saves
(setq auto-save-list-prefix nil
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil)

(setq-default indent-tabs-mode nil)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(global-visual-line-mode)

(setq show-paren-delay 0)
(show-paren-mode 1)

(global-display-line-numbers-mode 1)

;; Make the editor more discoverable (provides a popup menu for incomplete chord prefixes)
(use-package which-key
  :demand t
  :init
  :config
  (which-key-mode 1))

;; Add support for customizing key-bindings
;; Here we add support for the basic key-definers (prefixes)
(use-package general
  :demand t
  :config
  (general-create-definer dang/leader/def
    :states '(motion normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (dang/leader/def
    "TAB" '((lambda ()
              (interactive)
              (switch-to-buffer (other-buffer)))
            :wk "previous-buffer"))

  (general-create-definer dang/local/def
    :states '(normal visual insert emacs)
    :prefix "SPC l"
    :non-normal-prefix "M-SPC l")

  (dang/local/def
    "" '(nil :wk "local"))

  (defmacro dang/generate-override-keymap (definer inf name)
    "Generate command, keymap and definer for global override editor prefixes
The forms of the generated symbols is:
- infix key: INF
- commands: dang/NAME/command
- keymap: dang/NAME/map
- definer: dang/NAME/def"
    `(progn
        (,definer
          :infix ,inf
          :prefix-command ',(intern (concat "dang/" name "/command"))
          :prefix-map ',(intern (concat "dang/" name "/map"))
          "" '(:ignore t :wk ,name))
        (general-create-definer ,(intern (concat "dang/" name "/def"))
          :wk-full-keys nil
          :keymaps ',(intern (concat "dang/" name "/map")))))

  ;; Cannot be looped as the NAME string needs to be the macro argument to be
  ;; able to generate the symbols
  (dang/generate-override-keymap dang/leader/def "w" "windows")
  (dang/generate-override-keymap dang/leader/def "s" "search")
  (dang/generate-override-keymap dang/leader/def "b" "buffers")
  (dang/generate-override-keymap dang/leader/def "h" "help")
  (dang/generate-override-keymap dang/leader/def "f" "files")
  (dang/generate-override-keymap dang/leader/def "t" "text")
  (dang/generate-override-keymap dang/leader/def "c" "completions")

  (dang/windows/def
    "1" '(delete-other-windows :wk "delete-other-window")
    "b" 'balance-windows
    "d" '(delete-window :wk "delete-window") ;; Needed for some reason
    "h" '(evil-window-left :wk "window-right")
    "j" '(evil-window-down :wk "window-down")
    "k" '(evil-window-up :wk "window-up")
    "l" '(evil-window-right :wk "window-right")
    "m" 'maximize-window
    "s" 'split-window-below
    "v" 'split-window-right)

  (dang/buffers/def
    "k" 'kill-buffer
    "K" '((lambda ()
            (interactive)
            (kill-buffer nil))
          :wk "kill-current-buffer"))

  (dang/help/def
    "f" 'describe-function
    "g" 'general-describe-keybindings
    "k" 'describe-key
    "m" 'describe-mode
    "p" 'describe-package
    "v" 'describe-variable
    "w" 'where-is)

  (dang/files/def
    "d" 'delete-file
    "o" '(find-file :wk "open-file")
    "s" '(save-buffer :wk "save-file"))

  (dang/text/def
    "f" 'indent-region
    "F" '(indent-according-to-mode :wk "indent-line")))

;; Display buffer list with groupings
(use-package ibuffer
  :init
  (setq ibuffer-saved-filter-groups
        '(("default"
	   ("emacs-config" (filename . ".emacs.d"))
	   ("Org" (or (mode . org-mode)
		      (filename . "OrgMode")))
	   ("Magit" (name . "\*magit\*"))
	   ("ERC" (mode . erc-mode))
	   ("Help" (or (name . "\*Help\*")
		       (name . "\*Apropos\*")
		       (name . "\*Messages\*")
		       (name . "\*info\*"))))))
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  :general
  (dang/buffers/def
    "l" '(ibuffer :wk "list-buffers"))
  :config
  (add-hook 'ibuffer-mode-hook
	    '(lambda ()
	       (ibuffer-auto-mode 1)
	       (ibuffer-switch-to-saved-filter-groups "default"))))

;; Select and act on windows in tree-style
(use-package ace-window
  :init
  (defun ace-delete-window ()
    (interactive)
    (aw-delete-window))
  (defun swap-windows-and-keep-focus ()
    (interactive)
    (ace-swap-window)
    (aw-flip-window))
  :general
  (dang/windows/def
    "D" '(ace-delete-window :wk "select-and-delete-window")
    "f" '(swap-windows-and-keep-focus :wk "flip-windows")
    "S" '(ace-window :wk "select-window")))

;; Narrowing completions across the editor using ivy
(use-package ivy
  :demand t
  :general
  (dang/buffers/def
    "g" '(ivy-switch-buffer :wk "goto-buffer"))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap t))

;; Enable narrowing completion optimized incremental search
(use-package swiper
  :general ('normal "/" '(swiper :wk "swipe")))

;; Enable various narrowing completion enabled commands
(use-package counsel
  :demand t
  :general
  (dang/search/def
    "/" '(counsel-grep-or-swiper :wk "rg-or-swipe")
    "f" '(counsel-find-file :wk "find-file"))
  :config
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")) ;; Use ripgrep because it works faster and is accurate enough

;; Install and enable evil-mode to get vim emulation goodness
(use-package evil
  :demand t
  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-want-integration t)   ;; Make sure we can use evil pervasively
  (setq evil-want-keybinding nil)  ;; Disable default evilified keybindings so we can rely on evil-collection
  :config
  ;; Ensure no major mode defaults to emacs or motion state unless explicitly specified
  (setq evil-emacs-state-modes nil
        evil-motion-state-modes nil)
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

;; Get Hydra for modal key-bindings
(use-package hydra
  :demand t)

;; Enable completions globally
(use-package company
  :demand t
  :init
  (setq dang/default-company-backends '(company-capf company-files))
  (setq company-backends dang/default-company-backends)
  :general
  (dang/completions/def
    "c" 'company-complete
    "o" 'company-other-backend)
  :hook ((after-init . global-company-mode)))
