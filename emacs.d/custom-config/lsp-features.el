(use-package flycheck)

(use-package lsp-mode
  :commands lsp
  :config
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
