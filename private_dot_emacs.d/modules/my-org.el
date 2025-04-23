;;; Package --- my-org.el
;;; Commentary:
; Org-Mode functions and declarations

;;; Code:
(use-package org
  :init
  :bind
  :config
  (setq org-startup-folded t)
  (setq org-todo-keywords '((type "TODO(t)" "NEXT(n)" "STRT(s)" "HOLD(h@/!)" "PROJ(p)" "|" "DONE(d!)" "CANX(c@)")))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 2 )))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-return-follows-link t)
 ;; (setq org-agenda-files (list "~/org/tasks"))
  (setq org-agenda-window-setup 'current-window)
  (setq org-hide-leading-stars t)
  (setq org-adapt-indentation t)     ; indent content under headers
  (setq org-hide-emphasis-markers t) ; hide text format markers

  (defvar cl/base-agenda-files '("~/org/tasks")
	"The base agenda files that will always be included.")
  
  (setq org-agenda-custom-commands
	`(("d" "Daily Agenda and High Priority Tasks"
	   ((tags-todo "+PRIORITY=\"A\""
               ((org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                (org-agenda-block-separator nil)
                (org-agenda-overriding-header "Important tasks without a date\n")))
	    ; show overdue items separately
	    (agenda "" ((org-agenda-overriding-header "\nOverdue Tasks")
	     (org-agenda-start-day "-1d")
             (org-agenda-time-grid nil)
             (org-agenda-start-on-weekday nil)
             (org-agenda-show-all-dates nil)
             (org-agenda-format-date "")  ;; Skip the date
             (org-agenda-span 1)
             (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
             (org-agenda-entry-types '(:deadline :scheduled))
             (org-scheduled-past-days 999)
             (org-deadline-past-days 999)
	     (org-agenda-include-diary nil)
	     (org-agenda-block-separator nil)
             (org-deadline-warning-days 0)))
	    
	     ; following section from Prot's config - https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-lisp/prot-org.el
	     ; and https://protesilaos.com/codelog/2021-12-09-emacs-org-block-agenda/
	     (agenda "" ((org-agenda-span 1)      ; today only
                (org-deadline-warning-days 0)     ; remove any deadline warnings
                (org-agenda-block-separator nil)  ; remove separators between sections
                (org-scheduled-past-days 0)       ; remove any scheduled in the past
                (org-agenda-day-face-function (lambda (date) 'org-agenda-date))
                (org-agenda-format-date "%A %-e %B %Y")
                (org-agenda-overriding-header "\nToday's agenda\n")))
	     (agenda "" ((org-agenda-start-on-weekday nil)
                (org-agenda-start-day nil)
                (org-agenda-start-day "+1d")
                (org-agenda-span 3)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "\nNext three days\n")))
	     (agenda "" ((org-agenda-time-grid nil)
                (org-agenda-start-on-weekday nil)
                ;; We don't want to replicate the previous section's
                ;; three days, so we start counting from the day after.
                (org-agenda-start-day "+4d")
                (org-agenda-span 14)
                (org-agenda-show-all-dates nil)
                (org-deadline-warning-days 0)
                (org-agenda-block-separator nil)
                (org-agenda-entry-types '(:deadline))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                (org-agenda-overriding-header "\nUpcoming deadlines (+14d)\n")))
	    (todo "HOLD"
		  ((org-agenda-overriding-header "Blocked Tasks")
		   (org-agenda-max-todos nil)))
	    ))))
 
  (setq org-capture-templates
	`(("t" "todo" entry (file+headline ,(concat org-directory "/tasks/inbox.org") "Capture")
	   "** TODO %?\n" :empty-lines 1)
	  ("l" "daily bookmarks" entry
	   (file+olp+datetree, (concat org-directory "/notes/bookmarks/bookmarks.org") "Capture")
	   "** %(org-cliplink-capture)%?\n" :unnarrowed t)
	  ("m" "meeting" entry
	   (file+headline, (concat org-directory "/tasks/inbox.org") "Capture")
	   "** Meeting: %^{SUBJECT}%? \n%^T\n*** Attendees\n*** Notes\n\n" :empty-lines 1)
	  ("p" "phone call" entry
	   (file+headline, (concat org-directory "/tasks/inbox.org") "Capture")
	   "** Phone %^{person} \n%U\n*** Notes\n\n" :empty-lines 1)))
  
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c w") (lambda () (interactive) (find-file "~/org/tasks/work.org")))
  (global-set-key (kbd "C-c q") (lambda () (interactive) (find-file "~/org/tasks/home.org"))))
  
; set the default org path
(add-to-list 'load-path (expand-file-name "~/org"))

; set mode for org filetypes
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; set default apps
;;(setq org-file-apps '(("\\.ods\\'" \.system) (auto-mode . emacs)))

(use-package gnuplot
  :ensure t)

(use-package gnuplot-mode
  :ensure t)

;; add a hook to regenerate images after executing some org-babel
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; include org-babel gnuplot and shell tangling
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (gnuplot . t)
   (shell . t)
   )
 )

(setq org-attach-id-dir "/home/col/data/org-attach"
	  org-attach-directory "/home/col/data/org-attach"
	  org-attach-auto-tag "ATTACH")

; paste org link with description
; https://github.com/rexim/org-cliplink
(use-package org-cliplink
  :ensure t
  :bind ( :map global-map ( "C-x p i" . org-cliplink)))

;; -- Source Code Blocks
(setq org-src-preserve-indentation nil)   ;; allow indentation
(setq org-edit-src-content-indentation 0) ;; relative indent to #+begin_src

;; ;;
;; ;; --- org-journal ---
;; (use-package org-journal
;;   :straight (org-journal :type git
;;                   :host github
;;                   :repo "bastibe/org-journal")

;;   :init
;;   ;; Change default prefix key; needs to be set before loading org-journal
;;   (setq org-journal-prefix-key "C-c j")
;;   :config
;;   (global-set-key (kbd "C-c j") 'org-journal-new-entry)
;;   (setq org-journal-dir "~/org/journal/"
;;     org-journal-file-format "%Y%m%d.org"
;;     org-journal-date-format "%A, %d %B %Y")
;;     org-journal-file-type 'daily)

;; ;; custom function to return the current journal's location
;; ;; Open today's journal, but specify a non-nil prefix argument in order to
;; ;; inhibit inserting the heading; org-capture will insert the heading.
;; (defun org-journal-find-location ()
;;   (org-journal-new-entry t)
;;   (unless (eq org-journal-file-type 'daily)
;;     (org-narrow-to-subtree))
;;   (goto-char (point-max)))

;; (defun col/org-replace-link-by-link-description ()
;;   "Replace an org link by its description or if empty its address"
;;   (interactive)
;;   (if (org-in-regexp org-bracket-link-regexp 1)
;;       (save-excursion
;;         (let ((remove (list (match-beginning 0) (match-end 0)))
;;               (description (if (match-end 3)
;;                                (org-match-string-no-properties 3)
;;                              (org-match-string-no-properties 1))))
;;           (apply 'delete-region remove)
;;           (insert description)))))

;; bookmarks
;;set the default bookmark file
(setq bookmark-default-file "~/org/notes/bookmarks/bookmarks")
;;autosave bookmark file on change
(setq bookmark-save-flag 1)

(provide 'my-org)
;;; my-org.el ends here
