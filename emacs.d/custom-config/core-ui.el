(require 'dang/core-editor "core-editor")

;; Remove all the GUI elements
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

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
  (load-theme 'gruvbox-light-soft t))

(use-package dashboard
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-items '((recents  . 5)
                          (projects . 5))))

;; Add mappings for text display manipulation
(dang/text/def
 "d" 'text-scale-decrease
 "i" 'text-scale-increase)

(setq split-height-threshold 9999
      split-width-threshold 120)

(provide 'dang/core-ui)
