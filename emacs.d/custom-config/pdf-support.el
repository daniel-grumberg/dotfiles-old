(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :hook ((pdf-view-mode . pdf-view-midnight-minor-mode)
         (pdf-view-mode . auto-revert-mode))
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq auto-revert-interval 0.5)
  (auto-revert-set-timer))

(provide 'dang/pdf-support)
