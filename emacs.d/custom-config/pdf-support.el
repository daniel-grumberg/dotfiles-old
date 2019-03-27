(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook ((pdf-view-mode . pdf-view-midnight-minor-mode))
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page))

(provide 'dang/pdf-support)
