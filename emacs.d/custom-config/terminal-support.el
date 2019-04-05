(require 'dang/core-editor "core-editor")

(setq display-buffer-alist
      `(("\\*eshell\\*" display-buffer-in-side-window
         (side . bottom) (slot . 0) ;;(window-height . 0.3)
         (preserve-size . (t . nil)) ,dang/side-window-params) ,@display-buffer-alist))

(dang/files/def
  "'" '(eshell :wk "eshell"))

(provide 'dang/terminal-support)
