;; Shamelessly copied from llvm sources
(c-add-style "llvm.org"
             '("gnu"
 	       (fill-column . 80)
 	       (c++-indent-level . 2)
 	       (c-basic-offset . 2)
 	       (indent-tabs-mode . nil)
 	       (c-offsets-alist . ((arglist-intro . ++)
                                   (innamespace . 0)
                                   (member-init-intro . ++)))))

;; Apply llvm conventions when editing anything that seems to integrate with llvm
(defun dang/set-llvm-style ()
  "Sets the c-style to llvm.org when editing a path the that looks to belong
     to a llvm project"
  (when (and buffer-file-name
             (string-match "llvm" buffer-file-name))
    (c-set-style "llvm.org")))

(add-hook 'c-mode-hook 'dang/set-llvm-style)
(add-hook 'c++-mode-hook 'dang/set-llvm-style)

(use-package ccls
  :config
  (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc)) ;; Flycheck should not report garbage
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (require 'ccls) (lsp))))

(use-package cmake-font-lock
  :hook ((cmake-mode . cmake-font-lock-activate)))

(provide 'dang/c-c++-support)
