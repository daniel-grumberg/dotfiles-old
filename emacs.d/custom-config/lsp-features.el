(use-package flycheck)

(use-package lsp-mode
  :commands lsp
  :config
  :general
  (dang/help/def lsp-mode-map
    "l" 'lsp-describe-session)
  (dang/local/def lsp-mode-map
    "d" 'lsp-find-definition
    "f" 'lsp-format-buffer
    "g" 'lsp-goto-implementation
    "G" 'lsp-goto-type-definition
    "p" 'lsp-describe-thing-at-point
    "r" 'lsp-rename
    "R" 'lsp-find-references)
  (setq lsp-inhibit-message t
        lsp-enable-snippet t
        lsp-auto-guees-root t
        lsp-restart 'auto-restart
        lsp-auto-configure t
        lsp-prefer-flymake nil
        lsp-enable-indentation t))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook ((lsp-mode . lsp-ui-mode)))

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(provide 'dang/lsp-features)
