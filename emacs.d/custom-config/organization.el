;; This is a small hack. Emacs comes bundled with an outdated version of org-mode, leading to issues reporting the org version.
;; To circumvent this, we provide org-version ourselves to ensure emacs does not get confused.
(require 'subr-x)
(use-package git)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

;; This cannot go in the :init section because use-package tries to geenrate the autoloads before the keymap exists?
(dang/generate-override-keymap "o" "org-mode")
;; Proper installation of org-mode
(use-package org
  :init
  ;; Ensure org-mode is the default for /\.(org(_archive)?|txt)/ files
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  :general
  (dang/org-mode/def
   "l" 'org-store-link
   "a" 'org-agenda
   "c" 'org-capture)
  :config
  (setq org-directory "~/org"
        org-agenda-files '("~/org/refile.org" "~/org/agenda")
        org-default-notes-file "~/org/refile.org")

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
        org-todo-keyword-faces
        '(("TODO" :foreground "red" :weight bold)
          ("NEXT" :foreground "blue" :weight bold)
          ("DONE" :foreground "forest green" :weight bold)
          ("WAITING" :foreground "orange" :weight bold)
          ("HOLD" :foreground "magenta" :weight bold)
          ("CANCELLED" :foreground "forest green" :weight bold))
        org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

  (setq org-use-fast-todo-selection t
        org-use-fast-tag-selection t)

  (setq org-capture-templates
        '(("t" "todo" entry (file "~/git/org/refile.org")
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "note" entry (file "~/git/org/refile.org")
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("j" "Journal" entry (file+datetree "~/git/org/diary.org")
           "* %?\n%U\n" :clock-in t :clock-resume t)
          ("w" "org-protocol" entry (file "~/git/org/refile.org")
           "* TODO Review %c\n%U\n" :immediate-finish t))))

(provide 'dang/org-mode)
