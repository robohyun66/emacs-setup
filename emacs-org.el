;; Default settings
(with-eval-after-load 'org
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook 'visual-line-mode))


;; ;; Making latex fragements bigger
(require 'ox)
(plist-put org-format-latex-options :scale 3) ;3 for QHD monitor, 5 for laptop
(setq org-latex-create-formula-image-program 'dvipng)
;; (setq org-latex-create-formula-image-program 'imagemagick)


;; Pre-setting the image width of a figure to nil so I can manually change it in
;; each org document.
(setq org-image-actual-width nil)
(setq org-latex-image-default-width "5cm")

;; (setq org-image-actual-width (list 400))

;; Include html image.
(defun org-html--format-image (source attributes info)
  (progn
    (setq source (replace-in-string "%20" " " source))
    (format "<img src=\"data:image/%s;base64,%s\"%s />"
            (or (file-name-extension source) "")
            (base64-encode-string
             (with-temp-buffer
               (insert-file-contents-literally source)
              (buffer-string)))
            (file-name-nondirectory source))))

;; Enable org md export via C-c C-e m m
(require 'ox-md nil t)

;; Hide emphasis markers
(setq org-hide-emphasis-markers t)

;; highlighting of latex in org mode
(eval-after-load 'org
  '(setf org-highlight-latex-and-related '(latex)))
(eval-after-load 'org
  '(setf org-highlight-latex-fragments-and-specials t))


;; All todo files go here; only the html output goes in the actual folders.
(setq org-directory "/home/shyun/Dropbox/Documents/orglife")
;; The archived things all go here.
(setq org-archive-location "/home/shyun/Dropbox/Documents/orglife/archive.org::* From %s")
(setq org-agenda-files (list "/home/shyun/Dropbox/Documents/orglife/todo.org"
			     ;; This is the master todo file for everything
			     ;; "/home/shyun/Dropbox/Documents/orglife/todo-binseginf.org"
			     ;; This is the todo file for the binseginf paper.
			     ))
;; (format "%s/%s" org-base-path "notes.org")
;; (concat ORG "::")

;; keybindings for org mode agenda
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)

;;; In org agenda -- leave in emacs mode but add j & k
;; (define-key org-agenda-mode-map "j" 'evil-next-line)
;; (define-key org-agenda-mode-map "k" 'evil-previous-line)



;;; org agenda -- leave in emacs mode but add j & k

;; (define-key org-agenda-mode-map "j" 'evil-next-line)
;; (define-key org-agenda-mode-map "k" 'evil-previous-line)



;; Several things required for org-ref
(require 'cl)

;; Basic setup of org-ref
(setq org-ref-bibliography-notes "~/Dropbox/papers/notes.org"
      org-ref-default-bibliography '("~/Dropbox/papers/references.bib")
      org-ref-pdf-directory "~/Dropbox/papers/bibtex-pdfs/")

(unless (file-exists-p org-ref-pdf-directory)
  (make-directory org-ref-pdf-directory t))

;; Some org-mode customization
(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-src-preserve-indentation t)

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))

(org-babel-do-load-languages
  'org-babel-load-languages '((R . t)))

(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -interaction nonstopmode -output-directory %o %f"
	"pdflatex -interaction nonstopmode -output-directory %o %f"))

(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(require 'dash)
(setq org-latex-default-packages-alist
      (-remove-item
       '("" "hyperref" nil)
       org-latex-default-packages-alist))

;; Append new packages
(add-to-list 'org-latex-default-packages-alist '("" "natbib" "") t)
(add-to-list 'org-latex-default-packages-alist
	     '("linktocpage,pdfstartview=FitH,colorlinks,
linkcolor=blue,anchorcolor=blue,
citecolor=blue,filecolor=blue,menucolor=blue,urlcolor=blue"
	       "hyperref" nil)
	     t)

;; some requires for basic org-ref usage
(require 'org-ref)
(require 'org-ref-pdf)
(require 'org-ref-url-utils)


;; Make org-pdfview default
(eval-after-load 'org '(require 'org-pdfview))
;; If you want, you can also configure the org-mode default open PDF file function.
(add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))

;; PDFs visited in Org-mode are opened in Evince (and not in the default choice)
;; https://stackoverflow.com/a/8836108/789593
(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))


;; ;; linum clashes with pdf-tools, so only enabling it during prog-mode.
;; (add-hook 'prog-mode-hook 'linum-on)

;; Enabling github-style org html export
(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; Showing emacs what program (i.e. pand to use for markdown
(setq markdown-command "/usr/bin/pandoc")


(require 'ox-s5)

(setq org-s5-ui-url "/home/shyun/s5-files/ui")


;; Org mode projects
(setq org-publish-project-alist
      '(

  ("org-website"
          ;; Path to your org files.
          :base-directory "~/Dropbox/Documents/orglife/"
          :base-extension "org"

          ;; Path to your Jekyll project.
          :publishing-directory "~/Dropbox/Documents/orglife/website"
          :recursive t
          ;; :publishing-function org-publish-org-to-html
	  :publishing-function org-html-publish-to-html

          :headline-levels 4
          :html-extension "html"
          :body-only t ;; Only export section between <body> </body>
    )


    ("org-static-website"
          :base-directory "~/Dropbox/Documents/orglife/"
          :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
          :base-directory "~/Dropbox/website/temp/"
          :recursive t
          :publishing-function org-publish-attachment)

    ("website" :components ("org-website" "org-static-website"))

))



;; Org download and drag-and-drop
(require 'org-download)

;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

;; Allow helm-bibtex search by keyword.
(setq bibtex-completion-additional-search-fields '(keywords))


;; For website (org-page)
(setq op/repository-directory "~/repos/website")
(setq op/site-domain "http://www.stat.cmu.edu/~shyun/")

;; ;;; for commenting, you can choose either disqus, duoshuo or hashover
;; (setq op/personal-disqus-shortname "your_disqus_shortname")
;; (setq op/personal-duoshuo-shortname "your_duoshuo_shortname")
;; (setq op/hashover-comments t)

;;; the configuration below are optional
;; (setq op/personal-google-analytics-id "your_google_analytics_id")


(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; (require 'org-super-agenda)
(use-package org-super-agenda
  :ensure t
  ;; :disabled t
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '(
          (:name "Important tasks "
                 :priority "A")
          (:name "You promised"
		 :tag ("commitment"))
          (:name "USC"  ; Optionally specify section name
                 :tag ("usc"))
          (:name "Errands"  ; Optionally specify section name
                 :tag ("errands"))
          (:auto-category t)
          ))
  (org-agenda nil "a")
  )



;; Pretty bullets
;; (use-package org-bullets
;;   :config
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))



;; Agenda view html export
(setq org-agenda-exporter-settings
      '((ps-number-of-columns 2)
        (ps-landscape-mode t)
        (org-agenda-add-entry-text-maxlines 5)
        (htmlize-output-type 'css)))


;; Mixed pitch for org mode.
(add-hook 'org-mode-hook 'mixed-pitch-mode)

;; Settings for when and how to do
(setq org-log-into-drawer t)		;Insert into drawer.
(setq org-log-done 'time) 		;Track time of when item is done.
(setq org-todo-keywords
      '((sequence "TODO(t)" "INPROGRESS(i!)" "|" "DONE(d!)" "CANCELED(c@)" "STANDBY(s@/!)")))

;; ;; Setting Cycles
;; (setq org-todo-keywords
;;        '((sequence "TODO" "INPROGRESS" "DONE" )))


(setq org-default-notes-file (concat org-directory "todo.org"))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 2))))
(setq org-completion-use-helm nil)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path 'file)
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-deadline-warning-days 2)
