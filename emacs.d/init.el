(add-to-list 'load-path (expand-file-name "custom-config" user-emacs-directory))

;; Load basic required configurations
(require 'dang/package-management "package-management")
(require 'dang/core-editor "core-editor")
(require 'dang/core-ui "core-ui")

;; This list is ordered, so we need to ensure org-mode stays at the top.
;; Refer to the organization file to reed more about this.
(defvar dang/requested-features
  '((dang/org-mode . "organization")
    (dang/spellchecking . "spellchecking")
    (dang/git-integration . "git-integration")
    (dang/project-management . "project-management"))
  "Association list of features and filenames where to find them within the custom-config directory")

;; Load additional requested features
(mapc (lambda (feature)
        (let ((feature-symbol (car feature))
              (feature-file (cdr feature)))
          (require feature-symbol feature-file)))
        dang/requested-features)

(require 'dang/org-mode "~/.emacs.d/custom-config/organization.el")
