;; Remove all the GUI elements
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Set the default font
(add-to-list 'default-frame-alist '(font . "Source Code Pro 12"))

;; macOS specific UI tweak
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon  nil
	frame-title-format nil))

;; Editor theme (found at https://github.com/greduan/emacs-theme-gruvbox)
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-light-medium t))
