(add-to-list 'load-path (expand-file-name "custom-config" user-emacs-directory))

;; This list is ordered, so we need to ensure org-mode stays at the top.
;; Refer to the organization file to reed more about this.
(setq dang/requested-features
      '((dang/org-mode . "organization")
        (dang/spellchecking . "spellchecking")))

;; Load basic required configurations
(require 'dang/package-management "package-management")
(require 'dang/core-editor "core-editor")
(require 'dang/core-ui "core-ui")

;; Load additional requested features
(mapc (lambda (feature)
        (let ((feature-symbol (car feature))
              (feature-file (cdr feature)))
          (require feature-symbol feature-file)))
        dang/requested-features)

(require 'dang/org-mode "~/.emacs.d/custom-config/organization.el")
