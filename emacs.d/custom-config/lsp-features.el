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
  (setq lsp-inhibit-message t
        lsp-auto-guess-root t
        lsp-auto-configure t
        lsp-enable-indentation t
        lsp-enable-snippet nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-prefer-flymake nil))

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(provide 'dang/lsp-features)
