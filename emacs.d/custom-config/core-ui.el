(require 'dang/core-editor "core-editor")

;; Remove all the GUI elements
(setq inhibit-startup-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(blink-cursor-mode -1)

;; Set the default font
(add-to-list 'default-frame-alist '(font . "Source Code Pro 12"))

;; Ensure we start up in full screen mode
(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

;; macOS specific UI tweaks
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
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

;; This scales up the size of the currently selected window to make
;; working a bit easier on a small screen
(use-package golden-ratio
  :config
  (setq golden-ratio-auto-scale t)
  (golden-ratio-mode 1))

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
      `(("\\*\\(help\\|grep\\compilation\\|Man .*\\)\\*" display-buffer-in-side-window
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
  (setq neo-theme'arrows))

(dang/windows/def
  "t" '(window-toggle-side-windows :wk "toggle-side-windows"))

(provide 'dang/core-ui)
