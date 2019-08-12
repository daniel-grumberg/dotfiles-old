(require 'dang/core-editor "core-editor")

;; Remove all the GUI elements
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)

(fset 'yes-or-no-p 'y-or-n-p)

;; Allow finer grained resize of frames
(setq frame-resize-pixelwise t)

;; Set the default font
(add-to-list 'default-frame-alist '(font . "Source Code Pro 13"))

;; Ensure we get maximized frames
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; macOS specific UI tweaks
(when (eq system-type 'darwin)
  ;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . f))
  (add-to-list 'default-frame-alist '(ns-appearance . light)))

(dang/generate-override-keymap dang/leader/def "w" "windows")
(dang/windows/def
  "1" '(delete-other-windows :wk "delete-other-window")
  "b" 'balance-windows
  "d" '(delete-window :wk "delete-window") ;; Needed for some reason
  "D" '(kill-buffer-and-window :wk "delete-buffer-and-window") ;; Needed for some reason
  "h" '(windmove-left :wk "window-right")
  "j" '(windmove-down :wk "window-down")
  "k" '(windmove-up :wk "window-up")
  "l" '(windmove-right :wk "window-right")
  "m" 'maximize-window
  "M" '(toggle-frame-maximized :wk "maximize-frame-toggle")
  "o" 'other-window
  "s" 'split-window-below
  "v" 'split-window-right)

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

(setq split-width-threshold 120)

;; Windows should be able to be resized
(setq fit-window-to-buffer-horizontally t)
;;Ensure side windows maitain their respective sizes
(setq window-resize-pixelwise t)
(setq display-buffer-alist
      `(("\\*compilation\\*" display-buffer-in-side-window
        (side . bottom) (slot . 0) (preserve-size . (t . nil)))
       ("\\*\\(help\\|grep\\|xref\\|Man .*\\)\\*" display-buffer-in-side-window
        (side . right) (slot . 0) (window-width . fit-window-to-buffer)
        (preserve-size . (t . nil)) )))

(dang/windows/def
  "t" '(window-toggle-side-windows :wk "toggle-side-windows"))

(provide 'dang/core-ui)
