;; Ensure we use the straight mirror of GNU ELPA
(setq straight-recipes-gnu-elpa-use-mirror t)
;; Bootstrap straight.el package management
;; Snippet from https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; We want the built-in org mode
;; This needs to be done ensure no one pulls in a different org-mode
(straight-use-package '(org :type built-in))

;; Install and configure use-package to use straight under the hood
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)


(provide 'dang/package-management)
