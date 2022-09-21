;; Launching magit. Dipatch is also possible by h after launching magit-status.
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

;; Automatic magit updates when files are modified
;; (add-hook 'after-save-hook 'magit-after-save-refresh-status t)

;; ;; Using evil-magit: https://github.com/emacs-evil/evil-magit (not used anymore)
;; (setq evil-magit-state 'motion)
;; (require 'evil-magit)
