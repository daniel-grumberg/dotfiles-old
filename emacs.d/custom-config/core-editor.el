(provide 'dang/core-editor)

;; Preferred editor behaviors, this prevents backup files on auto-saves
(setq auto-save-list-prefix nil
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil)

(setq-default indent-tabs-mode nil)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq show-paren-delay 0)
(show-paren-mode 1)

;; Make the editor more discoverable (provides a popup menu for incomplete chord prefixes)
(use-package which-key
  :init
  :config
  (which-key-mode 1))

;; Add support for customizing key-bindings
;; Here we add support for the basic key-definers (prefixes)
(use-package general
  :init
  (general-auto-unbind-keys)
  (general-create-definer dang/leader/def
    :states '(normal visual insert)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (dang/leader/def
    "TAB" '((lambda ()
              (interactive)
              (switch-to-buffer (other-buffer)))
            :wk "previous-buffer"))

  (general-create-definer dang/local/def
    :states '(normal visual insert)
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :infix "l")

  (dang/local/def
    "" '(nil :wk "local"))

  (defmacro dang/generate-override-keymap (inf name)
    "Generate command, keymap and definer for global override editor prefixes
The forms of the generated symbols is:
- infix key: INF
- commands: dang/NAME/command
- keymap: dang/NAME/map
- definer: dang/NAME/def"
    `(progn
        (dang/leader/def
          :infix ,inf
          :prefix-command ',(intern (concat "dang/" name "/command"))
          :prefix-map ',(intern (concat "dang/" name "/map"))
          "" '(:ignore t :wk ,name))
        (general-create-definer ,(intern (concat "dang/" name "/def"))
          :wk-full-keys nil
          :keymaps ',(intern (concat "dang/" name "/map")))))

  ;; Cannot be looped as the NAME string needs to be the macro argument to be
  ;; able to generate the symbols
  (dang/generate-override-keymap "w" "windows")
  (dang/generate-override-keymap "s" "search")
  (dang/generate-override-keymap "b" "buffers")
  (dang/generate-override-keymap "h" "help")
  (dang/generate-override-keymap "f" "files")
  (dang/generate-override-keymap "t" "text")

  (dang/windows/def
    "b" 'balance-windows
    "m" 'maximize-window
    "d" '(delete-window :wk "delete-window") ;; Needed for some reason
    "1" '(delete-other-windows :wk "delete-other-window")
    "h" '(evil-window-left :wk "window-right")
    "j" '(evil-window-down :wk "window-down")
    "k" '(evil-window-up :wk "window-up")
    "l" '(evil-window-right :wk "window-right"))

  (dang/buffers/def
    "k" 'kill-buffer
    "K" '((lambda ()
            (interactive)
            (kill-buffer nil))
          :wk "kill-current-buffer"))

  (dang/help/def
    "v" 'describe-variable
    "f" 'describe-function
    "k" 'describe-key
    "g" 'general-describe-keybindings
    "w" 'where-is
    "m" 'describe-mode
    "p" 'describe-package)

  (dang/files/def
    "o" '(find-file :wk "open-file")
    "d" 'delete-file
    "s" '(save-buffer :wk "save-file"))

  (dang/text/def
    "f" 'indent-region
    "F" '(indent-according-to-mode :wk "indent-line")))

;; Display buffer list with groupings
(use-package ibuffer
  :init
  (setq ibuffer-saved-filter-groups
        '(("default"
	   ("emacs-config" (filename . ".emacs.d")
	    ("Org" (or (mode . org-mode)
		       (filename . "OrgMode")))
	    (mode . css-mode)))
	  ("Magit" (name . "\*magit\*"))
	  ("ERC" (mode . erc-mode))
	  ("Help" (or (name . "\*Help\*")
		      (name . "\*Apropos\*")
		      (name . "\*Messages\*")
		      (name . "\*info\*")))))
  (setq ibuffer-expert t)
  (setq ibuffer-show-empty-filter-groups nil)
  :general
  (dang/buffers/def
    "l" '(ibuffer :wk "list-buffers"))
  :config
  (add-hook 'ibuffer-mode-hook
	    '(lambda ()
	       (ibuffer-auto-mode 1)
	       (ibuffer-switch-to-saved-filter-groups "home"))))

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
    "s" '(ace-window :wk "select-window")
    "D" '(ace-delete-window :wk "select-and-delete-window")
    "f" '(swap-windows-and-keep-focus :wk "flip-windows")))

;; Narrowing completions across the editor using ivy
(use-package ivy
  :general
  (dang/buffers/def
    "g" '(ivy-switch-buffer :wk "goto-buffer"))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap t))

;; Enable narrowing completion optimised incremental search
(use-package swiper
  :general ('normal "/" '(swiper :wk "swipe")))

;; Enable various narrowing completion enabled commands
(use-package counsel
  :general
  (dang/search/def
    "/" '(counsel-grep-or-swiper :wk "rg-or-swipe")
    "f" '(counsel-find-file :wk "find-file"))
  :config
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")) ;; Use ripgrep because it works faster and is accurate enough

;; Install and enable evil-mode to get vim emulation goodness
(use-package evil
  :init
  (setq evil-want-integration t)   ;; Make sure we can use evil pervasively
  (setq evil-want-keybinding nil)  ;; Disable default evilified keybindings so we can rely on evil-collection
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer t)
  :config
  (evil-collection-init))

;; Get Hydra for modal key-bindings
(use-package hydra)

(use-package flyspell
  :init
  ;; Ensure that spellcheckers are found on macOS
  (setq ispell-dictionary "american")
  (when (eq system-type 'darwin)
    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
    (setq exec-path (append exec-path '("/usr/local/bin")))
    (setq ispell-program-name "aspell"))
  :custom
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil))

(use-package flyspell-correct-ivy
  :after flyspell
  :custom (flyspell-correct-interface 'flyspell-correct-ivy)
  :config
  (defhydra dang/hydra-spelling nil
  "
  ^
  ^Spelling^              ^Errors^            ^Checker^
  ^────────^──────────────^──────^────────────^───────^───────
  _q_ quit                _<_ previous        _c_ correction
  _f_ flyspell-mode       _>_ next            _d_ dictionary
  _p_ flyspell-prog-mode  ^^                  _b_ check-buffer
  ^^                      ^^                  ^^
  "
  ("q" nil)
  ("<" flyspell-correct-previous)
  (">" flyspell-correct-next)
  ("c" ispell)
  ("d" ispell-change-dictionary)
  ("f" flyspell-mode)
  ("p" flyspell-prog-mode)
  ("b" flyspell-buffer))
  (dang/text/def
    "s" '(dang/hydra-spelling/body :wk "spellcheck-menu")))
