(use-package tex
  :straight auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :general
  (dang/local/def LaTeX-mode-map
    "*" '(LaTeX-mark-section :wk "mark-section")
    "." '(LaTeX-mark-environment :wk "mark-environment")
    "]" '(LaTeX-close-environment :wk "close-environment")
    "~" '(LaTeX-math-mode :wk "toggle-math-mode")
    "B" '(LaTeX-find-matching-begin :wk "find-begin")
    "E" '(LaTeX-find-matching-begin :wk "find-end")
    "e" '(LaTeX-environment :wk "insert-environment")
    "i" '(LaTeX-insert-item :wk "insert-env-item")
    "s" '(LaTeX-section :wk "insert-section"))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t)
  (setq-default TeX-master nil))

(provide 'dang/latex-support)
