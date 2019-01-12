(require 'dang/core-editor "core-editor")

;; Remove all the GUI elements
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Set the default font
(add-to-list 'default-frame-alist '(font . "Source Code Pro 12"))

;; Ensure we start up in full screen mode

;; macOS specific UI tweak
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (add-to-list 'default-frame-alist '(fullscreen . fullscreen))
  (setq ns-use-proxy-icon  nil
	frame-title-format nil))

;; Editor theme (found at https://github.com/greduan/emacs-theme-gruvbox)
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-light-medium t))

;; Add mappings for text display manipulation
(dang/text/def
 "i" '((lambda ()
         (interactive)
         (text-scale-increase 1))
       :wk "increase-text-scale")
 "d" '((lambda ()
         (interactive)
         (text-scale-decrease 1))
       :wk "decrease-text-scale"))

(provide 'dang/core-ui)
