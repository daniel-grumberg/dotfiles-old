;; PDF Tool sdoes not work properly in macOS (At least the process is
;; not documented well enough for me to want to do it)

(when (not (eq system-type 'darwin))
  (use-package pdf-tools
    :mode ("\\.pdf\\'" . pdf-view-mode)
    :hook ((pdf-view-mode . pdf-view-midnight-minor-mode)
           (pdf-view-mode . auto-revert-mode))
    :config
    (pdf-tools-install)
    (setq-default pdf-view-display-size 'fit-page)
    (setq auto-revert-interval 0.5)
    (auto-revert-set-timer)))

(provide 'dang/pdf-support)
