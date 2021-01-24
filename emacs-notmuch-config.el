
;; setup the mail address and use name
(setq mail-user-agent 'message-user-agent)
(setq user-mail-address "robohyun66@gmail.com"
      user-full-name "robohyun66")
;; smtp config
(setq smtpmail-smtp-server "smtp.gmail.com"
      message-send-mail-function 'message-smtpmail-send-it)

;; report problems with the smtp server
(setq smtpmail-debug-info t)
;; add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")
;; postponed message is put in the following draft directory
(setq message-auto-save-directory "~/mail/draft")
(setq message-kill-buffer-on-exit t)
;; change the directory to store the sent mail
(setq message-directory "~/mail/")



;; (require 'smtpmail)
;; (setq send-mail-function 'smtpmail-send-it)
;; (setq message-send-mail-function 'smtpmail-send-it)
;; (require 'starttls)
