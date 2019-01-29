(add-to-list 'load-path (expand-file-name "custom-config" user-emacs-directory))

;; Load basic required configurations
(require 'dang/package-management "package-management")
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

 ;;This list is ordered, so we need to ensure org-mode stays at the top.
 ;;Refer to the organization file to reed more about this.
(defvar dang/core-features
  '((dang/org-mode . "organization")
    (dang/spellchecking . "spellchecking")
    (dang/git-integration . "git-integration")
    (dang/project-management . "project-management")
    (dang/lsp-features . "lsp-features"))
  "Association list of core features and filenames where to find them within the custom-config directory")

(defvar dang/language-support-features
  '((dang/c-c++-support . "c-c++-support"))
  "Association list of language support features and filenames where to find them within the custom-config directory")

;; Load core features
(dang/require-features dang/core-features)

;; Load language support features
(dang/require-features dang/language-support-features)

(require 'dang/org-mode "~/.emacs.d/custom-config/organization.el")
