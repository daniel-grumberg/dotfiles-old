(use-package flyspell
  :defer t
  :init
  ;; Ensure that spellcheckers are found on macOS
  (setq ispell-dictionary "american")
  (setq ispell-program-name "aspell")
  :custom
  (flyspell-issue-message-flag nil)
  (flyspell-issue-welcome-flag nil)
  :config
  (defun dang/flyspell-goto-previous-error (arg)
  "Go to ARG previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; go to beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))
        (forward-word))))))

(use-package flyspell-correct-ivy
  :after flyspell
  :custom (flyspell-correct-interface 'flyspell-correct-ivy)
  :general
  (dang/text/def
    "s" '(dang/hydra-spelling/body :wk "spellcheck-menu"))
  :config
  (defhydra dang/hydra-spelling nil
  "
  ^
  ^Spelling^              ^Errors^            ^Checker^
  ^────────^──────────────^──────^────────────^───────^───────
  _q_ quit                _<_ previous        _c_ correction
  _f_ flyspell-mode       _>_ next            _d_ dictionary
  _p_ flyspell-prog-mode  _n_ goto-next       _b_ check-buffer
  ^^                      _N_ goto-previous   ^^
  "
  ("q" nil)
  ("<" flyspell-correct-previous)
  (">" flyspell-correct-next)
  ("n" flyspell-goto-next-error)
  ("N" dang/flyspell-goto-previous-error)
  ("c" flyspell-correct-at-point)
  ("d" ispell-change-dictionary)
  ("f" flyspell-mode)
  ("p" flyspell-prog-mode)
  ("b" flyspell-buffer)))
