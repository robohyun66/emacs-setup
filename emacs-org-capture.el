(setq org-default-notes-file "~/Dropbox/Documents/orglife/general-notes.org")

;; From https://emacs.stackexchange.com/questions/53151/org-capture-prepend-after-org-header-config-block
(setq org-capture-templates
      '(("t" "todo" entry (file+headline  "~/Dropbox/Documents/orglife/todo.org" "Uncategorized")
         "* TODO %?\n  %i\n  %a"
	 :prepend t)
	 ))

;; (setq org-refile-targets
;;       '(("~/Dropbox/Documents/orglife/research-ideas.org" :maxlevel . 1))
;;       '(("~/Dropbox/Documents/orglife/todo.org" :maxlevel . 1)))

(setq org-refile-targets (quote (("todo.org" :maxlevel . 2)
                                 ("research-ideas.org" :level . 2)
                                 ("todo-reading.org" :level . 3)
                                 ;; ("someday.org" :level . 2)
				 )))


;; (find-if
;;   (lambda (refloc) (string-match "./research-ideas*"
;;                                  (car refloc)))
;;   (org-refile-get-targets))
