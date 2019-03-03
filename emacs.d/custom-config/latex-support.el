(use-package tex
  :straight auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :general
  (dang/local/def LaTeX-mode-map
    "*" '(LaTeX-mark-section :wk "mark-section")
    "." '(LaTeX-mark-environment :wk "mark-environment")
    "]" '(LaTeX-close-environment :wk "close-environment")
    "~" '(LaTeX-math-mode :wk "toggle-math-mode")
    "b" '(TeX-command-run-all :wk "build")
    "B" '(LaTeX-find-matching-begin :wk "find-begin")
    "E" '(LaTeX-find-matching-begin :wk "find-end")
    "e" '(LaTeX-environment :wk "insert-environment")
    "i" '(LaTeX-insert-item :wk "insert-env-item")
    "m" '(TeX-insert-macro :wk "insert-macro")
    "s" '(LaTeX-section :wk "insert-section"))
  :config
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-PDF-mode t)
  (setq-default TeX-master nil))

(use-package ivy-bibtex
  :general
  (dang/local/def LaTeX-mode-map
    "r" '(ivy-bibtex :wk "search-bibliography"))
  :config
  (setq bibtex-completion-bibliography '("~/bibliography/main.bib")
        bibtex-completion-library-path '("~/bibliography/files")
        bibtex-completion-pdf-field "File"
        bibtex-completion-notes-path "~/bibliography/notes/"
        ivy-bibtex-default-action 'ivy-bibtex-insert-key))

(provide 'dang/latex-support)
