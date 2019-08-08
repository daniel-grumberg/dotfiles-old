(require 'dang/core-editor "core-editor")

;; Remove all the GUI elements
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)

;; Allow finer grained resize of frames
(setq frame-resize-pixelwise t)

;; Set the default font
(add-to-list 'default-frame-alist '(font . "Source Code Pro 11"))

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

;; This scales up the size of the currently selected window to make
;; working a bit easier on a small screen
;; (use-package golden-ratio
;;   :config
;;   (golden-ratio-mode 1)
;;   (setq golden-ratio-auto-scale t))

;; Parameters that ensure that side windows maintain their purpose and
;; can not be accidentaly deleted
(defvar dang/side-window-params
  '(window-parameters . ((no-other-window . t)
                         (no-delete-other-windows . t))))

;; Windows should be able to be resized
;; (setq fit-window-to-buffer-horizontally t)
;;Ensure side windows maitain their respective sizes
(setq window-resize-pixelwise t)
(setq display-buffer-alist
      `(("\\*\\(help\\|grep\\|compilation\\|Man .*\\)\\*" display-buffer-in-side-window
         (side . bottom) (slot . 0)
         (preserve-size . (t . nil)) ,dang/side-window-params)
        ("\\*xref\\*" display-buffer-in-side-window
         (side . right) (slot . 0) (window-width . fit-window-to-buffer)
         (preserve-size . (t . nil)) ,dang/side-window-params)))

(use-package neotree
  :general
  (dang/windows/def
    "T" '(neotree-toggle :wk "toggle-tree"))
  :config
  (setq neo-theme 'arrows))

(dang/windows/def
  "t" '(window-toggle-side-windows :wk "toggle-side-windows"))

(provide 'dang/core-ui)
