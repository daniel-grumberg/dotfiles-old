(use-package flycheck)

(use-package lsp-mode
  :commands lsp
  :general
  (dang/help/def lsp-mode-map
    "l" 'lsp-describe-session)
  (dang/local/def lsp-mode-map
    "d" 'lsp-find-definition
    "D" 'lsp-find-declaration
    "f" 'lsp-format-buffer
    "g" 'lsp-goto-implementation
    "G" 'lsp-goto-type-definition
    "p" 'lsp-describe-thing-at-point
    "r" 'lsp-rename
    "R" 'lsp-find-references)
  (setq lsp-inhibit-message t
        lsp-enable-snippet t
        lsp-auto-guees-root t
        lsp-auto-configure t
        lsp-enable-indentation t))

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
