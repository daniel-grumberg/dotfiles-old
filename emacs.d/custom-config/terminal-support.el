(require 'dang/core-editor "core-editor")
(require 'dang/project-management "project-management")

(defun dang/generate-term-name (project-name)
    (concat project-name "--ansi-term"))

(defun dang/find-or-create-term-buffer (term-name window)
  (let ((maybe-buffer (get-buffer term-name)))
    (progn
      (select-window window)
      (if maybe-buffer
          (switch-to-buffer maybe-buffer)
        (ansi-term (getenv "SHELL"))
        (rename-buffer term-name)))))

(defun dang/create-term-window ()
  (let ((size (- (round (* (window-height (frame-root-window)) (/ 30.0 100.0))))))
    (split-window-below size)))

(defun dang/pop-term (project-name)
  (let ((term-window (get 'dang/pop-term 'terminal-window))
        (term-name (dang/generate-term-name project-name)))
    (if (window-valid-p term-window)
        (progn
          (delete-window term-window)
          (put 'dang/pop-term 'terminal-window nil))
      (let ((new-term-window (dang/create-term-window)))
        (progn
          (put 'dang/pop-term 'terminal-window new-term-window)
          (dang/find-or-create-term-buffer term-name new-term-window))))))

(defun dang/projectile-term ()
  (interactive)
  (dang/pop-term (projectile-project-name)))

(defun dang/default-term ()
  (interactive)
  (dang/pop-term nil))

(defun dang/kill-named-term (term-name)
  (let ((term-buffer (get-buffer term-name)))
    (when term-buffer
      (kill-buffer term-buffer))))

(defun dang/projectile-kill-term ()
  (interactive)
  (dang/kill-named-term (dang/generate-term-name (projectile-project-name))))

(defun dang/default-kill-term ()
  (interactive)
  (dang/kill-named-term (dang/generate-term-name nil)))

(dang/project/def
  "'" '(dang/projectile-term :wk "project-term")
  "\"" '(dang/projectile-kill-term :wk "kill-project-term"))

(dang/files/def
  "'" '(dang/default-term :wk "default-term")
  "\"" '(dang/default-kill-term :wk "kill-default-term"))

(provide 'dang/terminal-support)
