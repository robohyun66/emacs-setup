;; Really good Rmd + emacs tips from https://plantarum.ca/2021/10/03/emacs-tutorial-rmarkdown/

;; This is also helpful https://jblevins.org/projects/markdown-mode/

(require 'poly-R)

;; associate the new polymode to Rmd files:
(add-to-list 'auto-mode-alist
             '("\\.[rR]md\\'" . poly-gfm+r-mode))

;; uses braces around code block language strings:
(setq markdown-code-block-braces t)
