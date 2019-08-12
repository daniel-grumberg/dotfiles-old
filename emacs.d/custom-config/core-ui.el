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
  "b" 'balance-windows
  "D" '(delete-window :wk "delete-current-window")
  "K" '(kill-buffer-and-window :wk "delete-current-buffer-and-window")
  "m" '(toggle-frame-maximized :wk "maximize-frame-toggle")
  "M" 'maximize-window
  "S" '(split-window-below :wk "split-current-window-below")
  "V" '(split-window-right:wk "split-current-window-right"))

(defun dang/ace-kill-buffer-and-window ()
  (interactive)
  (require 'ace-window)
  (aw-select " Ace - Kill Buffer and Window"
             #'kill-buffer-and-window))

(defun dang/ace-split-below ()
  (interactive)
  (require 'ace-window)
  (aw-select " Ace - Split Below"
             #'aw-split-window-vert))

(defun dang/ace-split-right()
  (interactive)
  (require 'ace-window)
  (aw-select " Ace - Split right"
             #'aw-split-window-horz))

(use-package ace-window
  :commands (dang/ace-kill-buffer-and-window dang/ace-split-below dang/ace-split-right)
  :general
  (dang/windows/def
    "d" '(ace-delete-window :wk "delete-window") ;; Needed for some reason
    "f" '(ace-swap-window :wk "swap-windows")
    "k" '(dang/ace-kill-buffer-and-window :wk "delete-buffer-and-window")
    "o" '(ace-delete-other-windows :wk "delete-other-windows")
    "s" '(dang/ace-split-below :wk "split-window-below")
    "v" '(dang/ace-split-right :wk "split-window-right")
    "w" '(ace-window :wk "select-window"))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-swap-invert t))

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
