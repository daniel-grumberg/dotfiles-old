(provide 'dang/core-editor)

;; Prefered editor behaviors, this prevents backup files on auto-saves
(setq auto-save-list-prefix nil
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil)

(setq-default indent-tabs-mode nil)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq show-paren-delay 0)
(show-paren-mode 1)

;; Add support for customising key-bindings
;; Here we add support for the basic key-definers (prefixes)
(use-package general
  :init
  (general-create-definer dang/leader/def
    :states '(motion emacs)
    :prefix "SPC"
    :non-local-prefix "M-SPC")

  (dang/leader/def
    "" nil
    "l" '(nil :wk "local")
    "TAB" '((lambda ()
              (interactive)
              (switch-to-buffer (other-buffer)))
            :wk "previous-buffer"))

  (general-create-definer dang/local/def
    :states '(motion emacs)
    :prefix "SPC l"
    :non-local-prefix "M-SPC l"
    :wk-full-keys nil)

;;  (defmacro dang/generate-override-keymaps (inf pref-command pref-map help)
;;    `(dang/leader/def
;;       :infix ,inf
;;       :prefix-command ',pref-command
;;       :prefix-map ',pref-map
;;       :wk-full-keys nil
;;       "" '(:ignore t :wk ,help)))
;;
;;  ;; Create mapping functions for search related functionality
;;  (dang/generate-override-keymaps "w" dang/windows/command dang/windows/map "windows")
  (dang/leader/def
   :infix "w"
   :prefix-command 'dang/windows/command
   :prefix-map 'dang/windows/map
   :wk-full-keys nil
    "" '(:ignore t :wk "windows"))

  (general-create-definer dang/windows/def
    :keymaps 'dang/windows/map)

  (dang/windows/def
    "b" 'balance-windows
    "m" 'maximize-window
    "d" '(delete-window :wk "delete-window") ;; Needed for some reason
    "1" '(delete-other-windows :wk "delete-other-window")
    "h" '(evil-window-left :wk "window-right")
    "j" '(evil-window-down :wk "window-down")
    "k" '(evil-window-up :wk "window-up")
    "l" '(evil-window-right :wk "window-right"))

  ;; Create mapping functions for search related functionality
  (dang/leader/def
   :infix "s"
   :prefix-command 'dang/search/command
   :prefix-map 'dang/search/map
   :wk-full-keys nil
   "" '(:ignore t :wk "search"))

  (general-create-definer dang/search/def
    :keymaps 'dang/search/map)

  ;; Create mapping functions for buffers related functionality
  (dang/leader/def
   :infix "b"
   :prefix-command 'dang/buffers/command
   :prefix-map 'dang/buffers/map
   :wk-full-keys nil
    "" '(:ignore t :wk "buffers"))

  (general-create-definer dang/buffers/def
    :keymaps 'dang/buffers/map)

  (dang/buffers/def
    "k" 'kill-buffer
    "K" '((lambda ()
            (interactive)
            (kill-buffer nil))
          :wk "kill-current-buffer"))

  ;; Create mapping functions for help related functionality
  (dang/leader/def
   :infix "h"
   :prefix-command 'dang/help/command
   :prefix-map 'dang/help/map
   :wk-full-keys nil
    "" '(:ignore t :wk "help"))

  (general-create-definer dang/help/def
    :keymaps 'dang/help/map)

  (dang/help/def
    "v" 'describe-variable
    "f" 'describe-function
    "k" 'describe-key
    "g" 'general-describe-keybindings
    "w" 'where-is
    "m" 'describe-mode
    "p" 'describe-package)

  ;; Create mapping functions for file related functionality
  (dang/leader/def
   :infix "f"
   :prefix-command 'dang/files/command
   :prefix-map 'dang/files/map
   :wk-full-keys nil
   "" '(:ignore t :wk "files"))

  (general-create-definer dang/files/def
    :keymaps 'dang/files/map)

  (dang/files/def
    "o" '(find-file :wk "open-file")
    "d" 'delete-file
    "s" '(save-buffer :wk "save-file"))

  ;; Create mapping functions for text related functionality
  (dang/leader/def
   :infix "t"
   :prefix-command 'dang/text/command
   :prefix-map 'dang/text/map
   :wk-full-keys nil
   "" '(:ignore t :wk "text"))

  (general-create-definer dang/text/def
    :keymaps 'dang/text/map)

  (dang/text/def
    "f" 'indent-region
    "F" '(indent-according-to-mode :wk "indent-line"))

  (defun dang/show-msg ()
    (interactive)
    (message "emacs-lisp-mode-map"))

  (dang/local/def 'emacs-lisp-mode-map "z" 'dang/show-msg))

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

;; Make the editor more discoverable (provides a popup menu for incomplete chord prefixes)
(use-package which-key
  :init
  :config
  (which-key-mode 1))
