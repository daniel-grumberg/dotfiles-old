(require 'dang/core-editor "core-editor")
(require 'dang/project-management "project-management")

(defun dang/create-shell-window ()
  (let ((size (- (round (* (window-height (frame-root-window)) (/ 30.0 100.0))))))
    (split-window-below size)))

(defun dang/projectile-term ()
  (interactive)
  (let ((root (projectile-project-root))
        (win (dang/create-shell-window)))
    (select-window win)
    (let ((default-directory root))
      (ansi-term (getenv "SHELL")))))

(defun dang/default-term ()
  (interactive)
  (let ((win (dang/create-shell-window)))
    (select-window win)
    (ansi-term (getenv "SHELL"))))

(dang/project/def "'" '(dang/projectile-term :wk "project-term"))
(dang/files/def "'" '(dang/default-term :wk "project-term"))
(provide 'dang/terminal-support)
