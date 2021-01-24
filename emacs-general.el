;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;Miscellaneous ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq byte-compile-warnings '(cl-functions)) ;; cl is deprecated



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    ;;;;;;;;;;;;
;; All about packages ;;;;;;;;;;;;
;;                    ;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Packages in emacs
(require 'package)
(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa"        . "https://melpa.org/packages/")
	("gnu"          . "https://elpa.gnu.org/packages/")
	("org"          . "http://orgmode.org/elpa/")
	))

;; initialize built-in package management
(package-initialize)

;; update packages list if we are on a new install
(unless package-archive-contents
  (package-refresh-contents))

;; Auto-update packages
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Always ensure that packages are installed automatically
(require 'use-package-ensure)
(setq use-package-always-ensure t)

; list the packages you want
(setq package-list '(ace-window
		     company
		     dash-functional
		     dired-toggle-sudo
		     ess
		     evil
		     evil-easymotion
		     hideshow-org
		     julia-mode
		     mixed-pitch
		     org-download
		     org-ref
		     org-super-agenda
		     persistent-scratch
		     poporg
		     smart-mode-line
		     smooth-scroll
		     smooth-scrolling
		     use-package
		     ws-butler
		     zpresent
		     magit
		     poly-R
		     ))
;; undo-tree


;; USE-PACKAGE
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))



;; programmatically install/ensure installed
;; pkgs in your personal list
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting some paths ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Manually setting some variables to use. These are the same variables I'll use
;; throughout.
(setenv "DB" "/home/sangwonh/Dropbox")
(setenv "ORG" "/home/sangwonh/Dropbox/Documents/orglife")
(setenv "FL" "/home/sangwonh/Dropbox/Documents/research/usc/flow-cytometry/")
(setenv "USC" "/home/sangwonh/Dropbox/Documents/research/usc/")


;; Copy path and environment variables from shell environment.
;; (exec-path-from-shell-copy-env "DB") ;; These didn't work
;; (exec-path-from-shell-copy-env "ORG") ;; These didn't work
;; (when (memq window-system '(mac ns x))
;;   (exec-path-from-shell-initialize))

;; (let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
;;   (setenv "PATH" path)
;;   (setq exec-path
;;         (append
;;          (split-string-and-unquote path ":")
;;          exec-path)))

;;;;;;;;;;;;;;;;;;;;;;
;; Other settings  ;;;
;;;;;;;;;;;;;;;;;;;;;;

;; Enabling line numbering in text editors
(global-display-line-numbers-mode)

;; Show matching parentheses
(show-paren-mode 1)

;; Enabling line wrap by default
(setq line-move-visual nil)

;; Setting up Emacs as an edit server, so that it `listens' for external edit requests and acts accordingly.
(require 'server)
(unless (server-running-p)
    (server-start))

;; Enable ESS
 (use-package ess
  :ensure t
  :init (require 'ess-site))

;; ESS indentation
(defun myindent-ess-hook ()
  (setq ess-indent-level 2)
  (setq ess-offset-arguments-newline '(prev-line 2))
)
(add-hook 'ess-mode-hook 'myindent-ess-hook)


;; Changing backup behavior to save to ~/.saves
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)



;; Ensure environment variables inside Emacs look the same as in the user's shell.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;; Enabling access to list of recently edited files
(use-package recentf
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))
(setq recentf-keep '(file-remote-p file-readable-p))



; Allows colors in emacs M-x shell.
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)


;; hide the startup message
(setq inhibit-startup-message t)


;; Use ibuffer for better buffer management
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(setq ibuffer-default-sorting-mode 'major-mode) ;; I like to see my buffers
						;; sorted by major-mode, so I
						;; add this bit too:
(setq ibuffer-expert t) ;; prompt for confirmation only on modified buffers.


;; Coloring for shell?
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)



;; shell-mode
(defun sh ()
  (interactive)
  (ansi-term "/bin/zsh"))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; (use-package dired
;;   :ensure t
;;   :config
;;   ;; dired - reuse current buffer by pressing 'a'
;;   (put 'dired-find-alternate-file 'disabled nil)

;;   ;; ;; always delete and copy recursively
;;   ;; (setq dired-recursive-deletes 'always)
;;   ;; (setq dired-recursive-copies 'always)

;;   ;; ;; if there is a dired buffer displayed in the next window, use its
;;   ;; ;; current subdir, instead of the current subdir of this dired buffer
;;   ;; (setq dired-dwim-target t)

;;   ;; ;; enable some really cool extensions like C-x C-j(dired-jump)
;;   ;; (require 'dired-x)
;;   )
;; Allows switch to sudo user when browsing "dired" buffers
(require 'dired)
(require 'dired-toggle-sudo)



;; Make cutting and pasting use the X clipboard.
(setq x-select-enable-clipboard t)



;; Enabling Helm
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x .") 'helm-imenu-anywhere)

;; Enabling tab for completion in helm
(define-key helm-find-files-map "\t" 'helm-execute-persistent-action)




;; If the first two variables are set to something non-nil (e.g. t)
;; any space character in your search string will match any sequence
;; matched by the regular expression defined by the
;; search-whitespace-regexp variable.

;; A space character in your query will now match any space, tab, or linebreak any number of times.

;; To match words across line breaks do this
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace t)
(setq search-whitespace-regexp "[ \t\r\n]+")




;; Better window navigation
(global-set-key (kbd "M-o") 'ace-window)


;; ;; Enabling in-window editing for /google chrome/ (not used now)
;; (use-package 'edit-server)
;; (edit-server-start)


;; Helper to just swap the two windows
(defun win-swap () "Swap windows using buffer-move.el" (interactive) (if (null (windmove-find-other-window 'right)) (buf-move-left) (buf-move-right)))


;; ;; (add-hook 'org-mode-hook 'variable-pitch-mode)
;; (use-package mixed-pitch
;;   :hook
;;   ;; If you want it in all text modes:
;;   (org-mode . mixed-pitch-mode))


;; Turning off toolbar and menu bar
(tool-bar-mode -1)
(menu-bar-mode -1)


;; Allowing renaming of current buffer file, on the spot
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let* ((name (buffer-name))
        (filename (buffer-file-name))
        (basename (file-name-nondirectory filename)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " (file-name-directory filename) basename nil basename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))


;; Allows for embedded html images in EMACS
(defun replace-in-string (what with in)
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))


;; Use xdg-open on X. Not sure why it was needed.
(defun counsel-locate-action-extern (x)
  "Use xdg-open shell command on X."
  (call-process shell-file-name nil
                nil nil
                shell-command-switch
                (format "%s %s"
                        (if (eq system-type 'darwin)
                            "open"
                          "xdg-open")
                        (shell-quote-argument x))))



;; (setq-default line-spacing 1)
(defun toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
URL `http://ergoemacs.org/emacs/emacs_toggle_line_spacing.html'
Version 2017-06-02"
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.5))
  (redraw-frame (selected-frame)))


;; Require ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)


;; Loads evil mode.
(require 'evil)
(evil-mode 1)

;; Enabling evil easy motion (easy ways to navigate)
(evilem-default-keybindings "SPC")


;; ;; Make the line number to alway show with same size, regardless of zoom in/out.
;; (set-face-attribute 'linum nil :height 120)
;; ((add-hook 'org-mode-hook (lambda () (display-line-number-mode 0)))
;; ((add-hook 'org-mode-hook (lambda () (linum-mode 1)))


;; Open html files in browser
(defun open-html()
  "Get the HTML file path & open it"
  (interactive)
  (let (html-file-path)
    (setq html-file-path (buffer-file-name))
    (shell-command (format "xdg-open '%s'" html-file-path)))
)

;; ;; Enables vim-type number increment/decrement in Evil
;; (define-key evil-normal-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)
;; (define-key evil-normal-state-map (kbd "C-c -") 'evil-numbers/dec-at-pt)

;; (define-key evil-visual-state-map (kbd "C-c +") 'evil-numbers/inc-at-pt)


;; (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)


;; Scrolling without moving point (cursor)
(defun gcm-scroll-down ()
  (interactive)
  (scroll-up 2))

(defun gcm-scroll-up ()
  (interactive)
  (scroll-down 2))

(global-set-key [(control down)] 'gcm-scroll-down)
(global-set-key [(control up)]   'gcm-scroll-up)

;; Current line highlighting!! This is great for latex editing, not so much for
;; org mode since various other things like checkboxes and coloring are masked.
;; (global-hl-line-mode 1)


;; Best search feature
(global-set-key (kbd "C-c h o") 'helm-occur)

;; Setting python3 to be default
;; (setq py-python-command "python3")
(setq py-python-command "/usr/bin/python3")

(defcustom python-shell-interpreter "python3"
  "Default Python interpreter for shell."
  :type 'string
  :group 'python)

 (setq python-shell-interpreter "/usr/bin/python3")


;; Putting the current buffer's file name on clipboard
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
 (message "Copied buffer file name '%s' to the clipboard." filename))))


;; Making the scratch buffer consistent
(persistent-scratch-setup-default)
(setq initial-major-mode 'org-mode)


;; Allows for code folding (i.e. "hide-show")
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'R-mode-common-hook #'hs-minor-mode)
(add-hook 'ess-mode-hook 'hs-minor-mode)

(global-set-key (kbd "C-+") 'hs-toggle-hiding)

;; Org-like hideshow behavior
(add-to-list 'load-path "/home/sangwonh/repos/emacs-setup")
(require 'hideshow-org)


;; Make hide-show work in the middle of code blocks
(defun toggle-fold ()
  (interactive)
  (save-excursion
    (end-of-line)
    (hs-toggle-hiding)))


;; Makes buffer cycling via C-x <right> and C-x <left> *repeatable* i.e. C-x
;; <right> <right> <right> works.

;; The misc-cmds.el file is here:
;; https://www.emacswiki.org/emacs/download/misc-cmds.el
(load-library "/home/sangwonh/repos/emacs-setup/misc-cmds.el")
(global-set-key [remap previous-buffer] 'previous-buffer-repeat)
(global-set-key [remap next-buffer]     'next-buffer-repeat)

;; ;; Remote file will be kept without testing if they still exists. Added in an
;; ;; attempt to make emacs open/quit faster.
;; (setq recentf-keep '(file-remote-p file-readable-p))

;; ;; Automatically save recent files every 5 minutes.
;; (run-at-time (current-time) 300 'recentf-save-list)


;; ;; Trying to make emacs startup faster
;; ;; Per: https://github.com/bbatsov/prelude/issues/896
;; (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))


(setq remote-file-name-inhibit-cache nil)



;; Trying out origami mode: https://github.com/gregsexton/origami.el
;; (use-package 'origami)
;; (use-package origami
;;   :ensure t
;;   :commands (origami-toggle-node)
;;   :bind* (("M-m -" . origami-toggle-node)))
;; (add-to-list 'origami-parser-alist '(R . origami-c-style-parser))


(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))


;; Set default
;; (set-frame-font "Inconsolata 12" nil t)
;; (set-frame-font "Liberation Mono-12:antialias=1")
;; (set-face-attribute 'default nil :font "Ubuntu Mono"
;; (set-face-attribute 'default nil
;; 		    :font "Liberation Mono-12:antialias=1"
;; 		    :extend t)
;; (set-frame-font "Liberation Mono-12:antialias=1",
;; 		:extend t
;; 		nil t)


(set-frame-font "Fantasque Sans Mono-12:antialias=1")



;;(set-frame-font "Inconsolata-12")



;; (custom-theme-set-faces
;;  'user
;;  '(variable-pitch ((t (:family "Open Sans" :height 180 :weight Regular))))
;;  '(fixed-pitch ((t ( :family "Inconsolata" :slant normal :weight normal :height 1.0 :width normal)))))

;; revive this!!!!
;; (set-frame-font "Inconsolata-12")

;; ;;Another way to do it.
;; (set-face-attribute 'default nil
;;                     :family "Inconsolata"
;;                     :height 120
;; 		    :width (quote condensed))
		    ;; :width (quote condensed))

;; (set-face-attribute 'default nil
;;                     :family "Ubuntu Mono"
;;                     :height 120
;; 		    :width (quote ExtraCondensed))




;; Smart Mode Line is a sexy mode-line for Emacs. It aims to be easy to read
;; from small to large monitors by using colors, a prefix feature, and smart
;; truncation.
(use-package smart-mode-line
  :config
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  (sml/setup))




;; In tramp, sometimes files cannot be run.
(setq tramp-inline-compress-start-size 1000000)


;; KILL current buffer file and close
;; based on http://emacsredux.com/blog/2013/04/03/delete-file-and-buffer/
(defun delete-current-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))


;; Increment number at selection
(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))


;; Remove all newlines in region
(defun remove-newlines-in-region ()
  "Removes all newlines in the region."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (mark))
    (goto-char (point-min))
    (while (search-forward "\n" nil t) (replace-match "" nil t))))
;; (global-set-key [f8] 'remove-newlines-in-region) ;; Not doing this



;; Experimental; trying to make typing/scrolling smoother
(setq redisplay-dont-pause t)
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)



;; Tramp and SSH
(setq tramp-default-method "ssh")
(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))
;; (Old) Tramp password is saved in
;; ~/.authinfo.gpg

;; Use Python3 by default
(setq py-python-command "python3")
(put 'narrow-to-region 'disabled nil)


;(setq python-shell-interpreter "python3"
;      python-shell-interpreter-args "-i")
;(elpy-enable)


;; Load theme
;; (add-to-list 'custom-theme-load-path "~/repos/emacs-setup/themes/emacs-leuven-theme")
(load-theme 'leuven t)		                  ; For Emacs 24+.
;; (load-theme 'leuven-dark t)
;; (load-theme 'misterioso)
;; (use-package 'apropospriate)
;; (load-theme 'apropospriate-light t)
;; (load-theme 'apropospriate-dark t)


;; The vanilla undo/redo was not working well with evil mode, so this is a
;; workaround. undo-tree used to work out of the box, but now it doesn't.
(use-package undo-fu
  :config
  (global-undo-tree-mode -1)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))
