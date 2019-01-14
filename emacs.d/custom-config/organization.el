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

;; This cannot go in the :init section because use-package tries to generate the autoloads before the keymap exists?
(dang/generate-override-keymap dang/leader/def "o" "org-mode")
;; Proper installation of org-mode
(use-package org
  :init
  (setq org-hide-leading-stars t)
  (setq org-directory "~/org"
        org-agenda-files '("~/org/inbox.org"
                           "~/org/tickler.org"
                           "~/org/agenda")
        org-default-notes-file "~/org/inbox.org")

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

  (setq org-use-fast-todo-selection t)

  (setq org-use-fast-tag-selection t
        org-tag-persistent-alist '(("@home" . "h")
                                   ("@office" . "o")
                                   ("@email" . "e")))

  (setq org-capture-templates
        '(("t" "todo" entry (file "~/org/inbox.org")
           "* TODO %?\n")
          ("n" "note" entry (file "~/org/inbox.org")
           "* %? :NOTE:\n\n")
          ("j" "Journal" entry (file+datetree "~/org/diary.org")
           "* %?\n%U\n")
          ("w" "org-protocol" entry (file "~/org/inbox.org")
           "* TODO Review [[%:link][%:description]]\n")
          ("T" "Tickler" entry (file "~/org/tickler.org")
           "* %?\n%U\n")))

  (setq org-refile-targets '((org-agenda-files :maxlevel . 9)
                             ("~/org/tickler.org" :maxlevel . 9)))

  (setq org-archive-location "~/org/archive/%s_archive")
  ;; Ensure org-mode is the default for /\.(org(_archive)?|txt)/ files
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

  (defhydra dang/hydra-org-inbox-refile
    (:pre (setq which-key-inhibit t)
          :post (setq which-key-inhibit nil))
    ("j" org-next-item "next-item")
    ("k" org-previous-item "previous-item")
    ("r" org-refile "refile"))

  (defhydra dang/hydra-org-agenda (:pre (setq which-key-inhibit t)
                                        :post (setq which-key-inhibit nil)
                                        :hint none)
"
^Navigation^       ^Visit entry^                 ^Date^             ^Custom Commands^
^----------^------ ^-----------^---------------- ^----^-----------  ^---------------^
_j_ next           _SPC_ other-window            _ds_ schedule      ^^
_k_ previous       _TAB_ & goto-location         _dd_ set deadline  ^^
_J_ next-item      _RET_ & delete-other-windows  _dt_ timestamp     ^^
_K_ previous-item  ^^                            ^^                 ^^
^^                 ^^                            ^^                 ^^
^View^          ^Filter^              ^Headline^         ^Toggle mode^
^----^--------  ^------^------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ tag              _ht_ set-status    _tf_ follow
_vw_ week       _fr_ refine-by-tag    _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ category         _hr_ refile        _ta_ archive-trees
_vm_ month      _fh_ top headline     _ha_ archive       _tr_ clock-report
_vy_ year       _fx_ regexp           _h:_ set-tags      _td_ diaries
_vn_ next span  _fd_ delete--filters  _hp_ set-priority  ^^
_vp_ prev span  ^^                    ^^                 ^^
_vr_ reset      ^^                    ^^                 ^^
^^              ^^                    ^^                 ^^
^Other^
^-----^-------
_gr_ reload
_._ goto-today
_gd_ goto-date
^^
"
    ;; Navigation
    ("j" org-agenda-next-line)
    ("k" org-agenda-previous-line)
    ("J" org-agenda-next-item)
    ("K" org-agenda-previous-item)
    ;; Entry
    ("ha" org-agenda-archive-default)
    ("hk" org-agenda-kill)
    ("hp" org-agenda-priority)
    ("hr" org-agenda-refile)
    ("h:" org-agenda-set-tags)
    ("ht" org-agenda-todo)
    ;; Visit entry
    ("<tab>" org-agenda-goto :exit t)
    ("TAB" org-agenda-goto :exit t)
    ("SPC" org-agenda-show-and-scroll-up)
    ("RET" org-agenda-switch-to :exit t)
    ;; Date
    ("dt" org-agenda-date-prompt)
    ("dd" org-agenda-deadline)
    ("ds" org-agenda-schedule)
    ;; View
    ("vd" org-agenda-day-view)
    ("vw" org-agenda-week-view)
    ("vt" org-agenda-fortnight-view)
    ("vm" org-agenda-month-view)
    ("vy" org-agenda-year-view)
    ("vn" org-agenda-later)
    ("vp" org-agenda-earlier)
    ("vr" org-agenda-reset-view)
    ;; Toggle mode
    ("ta" org-agenda-archives-mode)
    ("tr" org-agenda-clockreport-mode)
    ("tf" org-agenda-follow-mode)
    ("tl" org-agenda-log-mode)
    ("td" org-agenda-toggle-diary)
    ;; Filter
    ("fc" org-agenda-filter-by-category)
    ("fx" org-agenda-filter-by-regexp)
    ("ft" org-agenda-filter-by-tag)
    ("fr" org-agenda-filter-by-tag-refine)
    ("fh" org-agenda-filter-by-top-headline)
    ("fd" org-agenda-filter-remove-all)
    ;; Other
    ("gd" org-agenda-goto-date)
    ("." org-agenda-goto-today)
    ("gr" org-agenda-redo))
  :general
  ('(motion emacs)
     "<f12>" 'org-agenda)
  ('(motion emacs) org-mode-map
     "M-h" 'org-metaleft
     "M-k" 'org-metaup
     "M-j" 'org-metadown
     "M-l" 'org-metaright
     "M-H" 'org-shiftmetaleft
     "M-K" 'org-shiftmetaup
     "M-J" 'org-shiftmetadown
     "M-L" 'org-shiftmetaright)
  (dang/org-mode/def
    "l" '(org-store-link :wk "store-link")
    "a" '(org-agenda :wk "agenda")
    "c" '(org-capture :wk "capture")
    "i" '((lambda ()
            (interactive)
            (find-file org-default-notes-file))
          :wk "open-inbox"))
  (dang/local/def 'org-mode-map
    "'" '(org-edit-special :wk "edit-special")
    "/" '(org-sparse-tree :wk "sparse-tree")
    "." '(org-time-stamp :wk "time-stamp")
    "!" '(org-time-stamp-inactive :wk "time-stamp-inactive")
    ":" '(org-set-tags :wk "set-tags")
    "a" '(org-archive-subtree :wk "archive-subtree")
    "b" '(org-tree-to-indirect-buffer :wk "tree-to-indirect-buffer")
    "c" '(org-capture :wk "capture")
    "d" '(org-deadline :wk "add-deadline")
    "D" '(org-insert-drawer :wk "insert-drawer")
    "e" '(org-export-dispatch :wk "export")
    "f" '(org-set-effort :wk "set-effort")
    "i" '(org-clock-in :wk "clock-in")
    "n" '(org-narrow-to-subtree :wk "narrow-to-subtree")
    "N" '(widen :wk "widen")
    "o" '(org-clock-out :wk "clock-out")
    "p" '(org-set-property :wk "set-property")
    "q" '(org-clock-cancel :wk "clock-cancel")
    "r" '(org-refile :wk "refile")
    "R" '(dang/hydra-org-inbox-refile/body :wk "inbox-refile-menu")
    "s" '(org-schedule :wk "schedule")
    "t" '(org-todo :wk "set-todo")
    "H" '(org-shiftleft :wk "shiftleft")
    "J" '(org-shiftdown :wk "shiftdown")
    "K" '(org-shiftup :wk "shiftup")
    "L" '(org-shiftright :wk "shiftright")
    "h" '(nil :wk "heading-insertion")
    "h i" '(org-insert-heading :wk "insert-heading")
    "h t" '(org-insert-todo-heading :wk "insert-todo")
    "h s" '(org-insert-subheading :wk "insert-subheading"))
  (dang/local/def 'org-capture-mode-map
    "f" '(org-capture-finalize :wk "finalize")
    "r" '(org-capture-refile :wk "finalize-and-refile")
    "a" '(org-capture-kill :wk "abort"))
  (dang/local/def 'org-agenda-mode-map
    "A" '(dang/hydra-org-agenda/body :wk "agenda menu")))

(use-package org-bullets
  :hook ((org-mode . org-bullets-mode)))

(provide 'dang/org-mode)
