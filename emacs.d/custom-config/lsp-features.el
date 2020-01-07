(use-package flycheck)

(use-package lsp-mode
  :commands lsp
  :general
  (dang/help/def
    :predicate '(lsp-mode)
    "l" '(lsp-describe-session :wk "lsp-describe-session"))
  (dang/local/def lsp-mode-map
    "d" 'lsp-find-definition
    "D" 'lsp-find-declaration
    "f" 'lsp-format-buffer
    "g" 'lsp-goto-implementation
    "G" 'lsp-goto-type-definition
    "h" 'lsp-describe-thing-at-point
    "p" '(xref-pop-marker-stack :wk "goto-previous")
    "r" 'lsp-rename
    "R" 'lsp-find-references)
  :config
  (setq lsp-auto-guess-root t
        lsp-enable-indentation t
        lsp-enable-snippet t
        lsp-prefer-flymake nil
        lsp-file-watch-threshold nil
        lsp-auto-configure t)
  (add-to-list 'lsp-file-watch-ignored ".ccls-cache"))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil))

(provide 'dang/lsp-features)
