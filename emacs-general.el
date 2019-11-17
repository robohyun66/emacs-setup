;; Manually setting some variables to use. These are the same variables I'll use
;; throughout.
(setenv "DB" "/home/shyun/Dropbox")
(setenv "ORG" "/home/shyun/Dropbox/Documents/orglife")
(setenv "FL" "/home/shyun/Dropbox/Documents/research/usc/flow-cytometry/")
(setenv "USC" "/home/shyun/Dropbox/Documents/research/usc/")


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



;; ;; Enabling line numbering in text editors
;; (global-linum-mode 1)
(global-display-line-numbers-mode)


;; Show matching parentheses
(show-paren-mode 1)


;; Enabling line wrap by default
(setq line-move-visual nil)


;; Setting up Emacs as an edit server, so that it `listens' for external edit requests and acts accordingly.
(require 'server)
(unless (server-running-p)
    (server-start))
;; (or (server-running-p)
;;     (server-start))


;; Enable ESS
(require 'ess-site)

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
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode +1)


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

;; Allows switch to sudo user when browsing "dired" buffers
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


;; Load theme
(load-theme 'leuven t)


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
;; (require 'edit-server)
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


(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)


;; Enabling evil easy motion (easy ways to navigate)
(evilem-default-keybindings "SPC")


;; ;; Make the line number to alway show with same size, regardless of zoom in/out.
;; (set-face-attribute 'linum nil :height 120)


;; Changing time stamps
(setq org-time-stamp-custom-formats
      '("<%d %b %Y>" . "<%d/%m/%y %a %H:%M>"))


;; Disabling certain commands because they are accidentally used sometimes and
;; it is super annoying.
(put 'org-columns 'disabled
     "You asked to go into org column view, but I'd bet it was a mistake!\n")

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


;; Wrapping region with a character (this doesn't seem to work.ve)
(require 'wrap-region)
(add-hook 'org-mode-hook #'wrap-region-mode)
(wrap-region-add-wrapper "/" "/" nil 'org-mode) ; select region, hit / then region -> /region/ in org-mode
(wrap-region-add-wrapper "=" "=" nil 'org-mode) ; select region, hit = then region -> =region= in org-mode
(wrap-region-add-wrapper "*" "*" nil 'org-mode) ; select region, hit * then region -> *region* in org-mode
(wrap-region-add-wrapper "_" "_" nil 'org-mode) ; select region, hit _ then region -> _region_ in org-mode
(wrap-region-add-wrapper "+" "+" nil 'org-mode) ;
(wrap-region-add-wrapper "~" "~" nil 'org-mode) ;



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
