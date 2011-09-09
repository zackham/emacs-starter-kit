(setq dotfiles-dir (file-name-directory
                   (or (buffer-file-name) load-file-name)))

;; Load up ELPA, the package manager

(add-to-list 'load-path dotfiles-dir)

;; get textmate-mode running
(add-to-list 'load-path "~/.emacs.d/textmate.el")
(require 'textmate)
(textmate-mode)

;; (require 'vimpulse)
;;(require 'color-theme-zenburn)
;;(color-theme-zenburn)
(color-theme-sunburst)

(provide 'zack-init)

(require 'notmuch)
(require 'org-notmuch)
(global-set-key "\M-n" 'notmuch-search)

;; Org-mode settings
;; use newer org 7.7
(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(global-font-lock-mode 1)

;; http://orgmode.org/manual/Clocking-work-time.html#Clocking-work-time
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
; global Effort estimate values
; not sure if this is working...
(setq org-global-properties (quote (("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:00"))))

;; make sure we're using habits
(add-to-list 'org-modules 'org-habit)
(require 'org-habit)

(setq org-startup-indented t)
(setq org-odd-levels-only nil)

(setq org-agenda-files '("/media/truecrypt1/org"))
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@/!)")))

;; any headline with level <= 2 is a target
(setq org-refile-targets '((nil :maxlevel . 2)
                                ; all top-level headlines in the
                                ; current buffer are used (first) as a
                                ; refile target
                           (org-agenda-files :maxlevel . 2)))

;; provide refile targets as paths, including the file name
;; (without directory) as level 1 of the path
(setq org-refile-use-outline-path 'file)

;; allow to create new nodes (must be confirmed by the user) as
;; refile targets
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; refile only within the current buffer
(defun my/org-refile-within-current-buffer ()
  "Move the entry at point to another heading in the current buffer."
  (interactive)
  (let ((org-refile-targets '((nil :maxlevel . 5))))
    (org-refile)))

;; set up my capture templates
(setq org-default-notes-file "/media/truecrypt1/org/refile.org")
(global-set-key "\C-cc" 'org-capture)
;; Capture templates for: TODO tasks, Notes, appointments, phone calls, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "/media/truecrypt1/org/refile.org")
               "* TODO %?\n%U\n%a\n  %i" :clock-in t :clock-resume t)
              ("n" "note" entry (file "/media/truecrypt1/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n  %i" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "/media/truecrypt1/org/diary.org")
               "* %?\n%U\n  %i" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "/media/truecrypt1/org/refile.org")
               "* TODO Review %c\n%U\n  %i" :immediate-finish t)
              ("p" "Phone call" entry (file "/media/truecrypt1/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "/media/truecrypt1/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %t .+1d/3d\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n  %i"))))

;; sending email
(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)) ; Must be set BEFORE loading smtpmail 
      smtpmail-auth-credentials (expand-file-name "~/.authinfo") 
      smtpmail-default-smtp-server "smtp.gmail.com" 
      smtpmail-smtp-server "smtp.gmail.com" 
      smtpmail-smtp-service 587 
      smtpmail-debug-info t ; change to nil once it works 
      smtpmail-debug-verb t) 
(require 'smtpmail) 
(setq message-send-mail-function 'smtpmail-send-it) 
(require 'starttls)
