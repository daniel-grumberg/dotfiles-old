(add-to-list 'load-path (expand-file-name "custom-config" user-emacs-directory))

;; Load basic configurations
(load "package-management")
(load "core-editor")
(load "core-ui")
