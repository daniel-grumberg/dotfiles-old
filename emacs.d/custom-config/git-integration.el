(provide 'dang/git-integration)

;; This cannot go in the :init section because use-package tries to generate the autoloads before the keymap exists?
(dang/generate-override-keymap dang/leader/def "g" "git")
;; Proper installation of magit
(use-package magit
  :init
  (dolist (mode '(magit-cherry-mode magit-diff-mode magit-log-mode magit-log-select-mode magit-popup-mode magit-popup-sequence-mode magit-process-mode magit-reflog-mode magit-refs-mode magit-revision-mode magit-stash-mode magit-stashes-mode magit-status-mode magit-mode magit-branch-manager-mode magit-commit-mode magit-key-mode magit-rebase-mode magit-wazzup-mode))
    (setq evil-emacs-state-modes (cons mode evil-emacs-state-modes)))
  :general
  (dang/git/def
    ">" '(magit-submodule-popup :wk "submodules-popup")
    "b" '(magit-blame-addition :wk "blame")
    "i" '(magit-gitignore-locally :wk "ignore")
    "s" '(magit-status :wk "status")
    "S" '(magit-stage :wk "stage-file")
    "m" '(magit-dispatch-popup :wk "dispatch")
    "U" '(magit-unstage-file :wk "unstage-file")))
