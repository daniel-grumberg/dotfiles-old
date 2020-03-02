;; Ensure this is a server so we can use emacsclient, notably for SyncTeX
(load "server")
(unless (server-running-p) (server-start))

;; Easily locate handwriten configurations
(add-to-list 'load-path (expand-file-name "custom-config" user-emacs-directory))

;; Don't clobber hand-written configurations files with stuff generated by the customize interface
(setq custom-file "~/.emacs-custom.el")
(when (file-exists-p custom-file) (load custom-file))

;; Load basic required configurations
(require 'dang/package-management "package-management")

;; If Emacs is started via GUI toolkit it won't source the startup scripts which might cause issues (especially on macOS)
(use-package exec-path-from-shell
  :demand t
  :config
  (exec-path-from-shell-initialize))

(require 'dang/core-editor "core-editor")
(require 'dang/core-ui "core-ui")

;; TODO investigate why this cannot be a plain defun (leads to recursive load issues)
(defmacro dang/require-features (features)
    "Require features from the FEATURES alist where car is feature symbol and cdr is file in custom-config directory"
    `(mapc (lambda (feature)
            (let ((feature-symbol (car feature))
                  (feature-file (cdr feature)))
              (require feature-symbol feature-file)))
          ,features))

(defvar dang/core-features
  '((dang/org-mode . "organization")
    (dang/spellchecking . "spellchecking")
    (dang/git-integration . "git-integration")
    (dang/snippet . "snippet")
    (dang/project-management . "project-management")
    (dang/lsp-features . "lsp-features")
    (dang/terminal-support . "terminal-support")
    (dang/pdf-support . "pdf-support")
    (dang/presentation-support . "presentation-support")
    (dang/docker-support . "docker-support"))
  "Association list of core features and filenames where to find them within the custom-config directory")

(defvar dang/language-support-features
  '((dang/c-c++-support . "c-c++-support")
    (dang/latex-support . "latex-support")
    (dang/llvm-support . "llvm-support")
    (dang/z3-support . "z3-support"))
  "Association list of language support features and filenames where to find them within the custom-config directory")

;; Load core features
(dang/require-features dang/core-features)

;; Load language support features
(dang/require-features dang/language-support-features)
