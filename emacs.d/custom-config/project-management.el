;; This cannot go in the :init section because use-package tries to generate the autoloads before the keymap exists?
(dang/generate-override-keymap dang/leader/def "p" "project")

(use-package projectile
  :general
  (dang/project/def
   "a" '(projectile-find-other-file :wk "toggle-interface-implementation")
   "b" '(projectile-switch-to-buffer :wk "goto-buffer")
   "c" '(projectile-compile-project :wk "compile")
   "d" '(projectile-find-dir :wk "goto-directory")
   "D" '(projectile-dired :wk "project-dired")
   "f" '(projectile-find-file :wk "find-file")
   "g" '(projectile-vc :wk "version-control")
   "I" '(projectile-invalidate-cache :wk "invalidate-project-cache")
   "k" '(projectile-kill-buffers :wk "kill-project-buffers")
   "p" '(projectile-switch-project :wk "switch-project")
   "t" '(projectile-test-project :wk "test")
   "/" '(projectile-grep :wk "grep"))
  :config
  (setq projectile-completion-system 'ivy))


(provide 'dang/project-management)
