;; Prefered editor behviors, this prevents backup files on auto-saves
(setq auto-save-list-prefix nil
      make-backup-files nil
      auto-save-default nil)

(setq-default indent-tabs-mode nil)

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq show-paren-delay 0)
(show-paren-mode 1)

;; Add support for customising key-bindings
;; Here we add support for the basic key-definers (prefixes)
(use-package general)

(general-auto-unbind-keys)

(general-define-key
 :states '(motion emacs)
 :prefix "SPC"
 :non-normal-prefix "C-SPC"
 :prefix-map 'dang/leader/map)

(general-create-definer dang/leader/def
  :keymaps 'dang/leader/map)

(general-define-key
 :keymaps 'dang/leader/map
 :prefix "s"
 :prefix-command 'dang/search/command
 :prefix-map 'dang/search/map
 :wk-full-keys nil
 "" '(:ignore t :wk "search"))

(general-create-definer dang/search/def
  :keymaps 'dang/search/map)

;; Narrowing completions across the editor using ivy
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "(%d/%d) "))

;; Enable narrowing completion optimised incremental search
(use-package swiper)

;; Enable various narrowing completion enabled commands
(use-package counsel
  :general
  (dang/search/def "/" '(counsel-grep-or-swiper :wk "rg-or-swipe"))
  :config
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s")) ;; Use ripgrep because it works faster and is accurate enough

;; Install and enable evil-mode to get vim emulation goodness
(use-package evil
  :init
  (setq evil-mode-line-format nil) ;; Disable evil state indicator in mode line
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
