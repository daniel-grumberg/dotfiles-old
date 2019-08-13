(require 'dang/core-ui)

(setq dired-use-ls-dired nil)

(defun dang/dwim-toggle-or-open ()
  "Toggle subtree or open the file."
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (file-directory-p file)
      (progn (dired-subtree-cycle)
             (revert-buffer))
      (dang/ace-find-file file))))

(defun dang/mouse-dwim-toggle-or-open (event)
  "Toggle subtree or the open file on mouse-click in dired."
  (interactive "e")
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event)))
        (file nil))
    (select-window window)
    (goto-char pos)
    (dang/dwim-toggle-or-open)))

(use-package dired-subtree
  :demand t
  :general
  (dired-mode-map
   :states '(normal motion insert emacs)
   "<enter>" 'dang/dwim-toggle-or-open
   "<return>" 'dang/dwim-toggle-or-open
   "<tab>" 'dang/dwim-toggle-or-open
   ;; For weird historical reasons emacs translates mouse-1 clicks to
   ;; mouse-2 clicks for link events such as filenames in
   ;; dired... More info at:
   ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Clickable-Text.html
   "<mouse-2>" 'dang/mouse-dwim-toggle-or-open)
  :config
  (setq dired-subtree-use-backgrounds nil))

(defun dang/toggle-project-explorer ()
  "Toggle the project explorer window."
  (interactive)
  (let* ((buffer (dired-noselect (projectile-project-root)))
         (window (get-buffer-window buffer)))
    (if window
        (dang/hide-project-explorer)
      (dang/show-project-explorer))))

(defun dang/show-project-explorer ()
  "Project dired buffer on the side of the frame.
Shows the projectile root folder using dired on the left side of
the frame and makes it a dedicated window for that buffer."
  (let ((buffer (dired-noselect (projectile-project-root))))
    (with-current-buffer buffer
      (dired-hide-details-mode t)
      (display-line-numbers-mode -1))
    (display-buffer-in-side-window buffer '((side . left) (window-width . 35)))
    (set-window-dedicated-p (get-buffer-window buffer) t)))

(defun dang/hide-project-explorer ()
  "Hide the project-explorer window."
  (let ((buffer (dired-noselect (projectile-project-root))))
    (progn
      (delete-window (get-buffer-window buffer))
      (kill-buffer buffer))))

;; This cannot go in the :init section because use-package tries to generate the autoloads before the keymap exists?
(dang/generate-override-keymap dang/leader/def "p" "project")
(use-package projectile
  :general
  (dang/project/def
   "a" '(projectile-find-other-file :wk "toggle-interface-implementation")
   "b" '(projectile-switch-to-buffer :wk "goto-buffer")
   "c" '(projectile-compile-project :wk "compile")
   "C" '(projectile-configure-project :wk "configure")
   "d" '(dang/toggle-project-explorer :wk "toggle-explorer")
   "e" '(projectile-edit-dir-locals :wk "project-dir-locals")
   "f" '(projectile-find-file :wk "project-find-file")
   "g" '(projectile-vc :wk "version-control")
   "I" '(projectile-invalidate-cache :wk "invalidate-project-cache")
   "k" '(projectile-kill-buffers :wk "kill-project-buffers")
   "p" '(projectile-switch-project :wk "switch-project")
   "t" '(projectile-test-project :wk "test")
   "/" '(projectile-grep :wk "grep"))
  :config
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t)
  (projectile-mode 1))

(use-package nameframe
  :demand t)

(use-package nameframe-projectile
  :demand t
  :after nameframe
  :config
  (nameframe-projectile-mode t))

(provide 'dang/project-management)
