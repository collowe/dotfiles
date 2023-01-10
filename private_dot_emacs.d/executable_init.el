(require 'org)

;; Setup Repos
(require 'package)
(setq package-archives
      '(
	("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
	)
      )

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq package-enable-at-startup nil)

;; Use-Package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Interface
;; remove startup and splash screens
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
;; hide menus
(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-visual-line-mode 1) ;; line wrap
(electric-pair-mode 1) ;; insert matching delims
(setq org-startup-folded t) ;; startup folded
(add-hook 'org-mode-hook 'org-indent-mode) ;; always use org-indent mode
;;(setq org-adapt-indentation t) ;; indent content under headers
;;(setq split-height-threshold nil) ;; horizontal split by default
;;(setq split-width-threshold 0) ;; horizontal split by default

;; set the theme using ef-themes
(use-package ef-themes
  :ensure t)
;; disable any active themes
(mapc #'disable-theme custom-enabled-themes)
(load-theme 'ef-autumn :no-confirm)

;; change bullets to be pretty
(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
)

;; hide the markup/emphasis markers
(setq org-hide-emphasis-markers t)

;; refresh buffer if file changes on disk
(setq global-auto-revert-mode t)

;; backups - get backup files out of the way
(setq auto-save-list-file-prefix nil)
(setq backup-directory-alist
      `(("*.*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `(("*.*" ,temporary-file-directory t)))

;; which-key
(use-package which-key
  :ensure t
  :init (which-key-mode 1)
)

;; diary config
(setq org-agenda-include-diary t)       ;; include diary entries in the agenda
(setq diary-file "~/org/diary/diary")   ;; set the calendar file
(setq calendar-latitude 53.842178)      ;; calendar location - lat
(setq calendar-longitude -1.636099)     ;; calendar location - long
(setq calendar-week-start-day 1)        ;; set calendar to start on Monday
(setq mark-diary-entries-in-calendar t) ;; mark diary entries in calendar by default

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(custom-safe-themes
   '("e6df46d5085fde0ad56a46ef69ebb388193080cc9819e2d6024c9c6e27388ba9" default))
 '(org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
 '(org-agenda-span 'day)
 '(package-selected-packages
   '(ef-themes elpy plantuml-mode nord-theme alarm-clock 0blayout ox-hugo which-key org-superstar org-bars counsel ivy org-roam-ui simple-httpd websocket org-journal org-roam deft zenburn-theme try use-package magit))
 '(webjump-sites
   '(("Emacs Home Page" . "www.gnu.org/software/emacs/emacs.html")
     ("Emacs Wiki" .
      [simple-query "www.emacswiki.org" "www.emacswiki.org/cgi-bin/wiki/" ""])
     ("DuckDuckGo" .
      [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
     ("Google" .
      [simple-query "www.google.com" "www.google.com/search?q=" ""])
     ("Google Groups" .
      [simple-query "groups.google.com" "groups.google.com/groups?q=" ""])
     ("Wikipedia" .
      [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
     ("Dashboard" . "http://192.168.1.67:5005/"))))
   
;; set the agenda files
(setq org-agenda-files (list "~/org/tasks"
			     "~/org/journal"))

;; custom shortcut key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)
;; shortcut for work org file
(global-set-key (kbd "C-c w") 
                (lambda () (interactive) (find-file "~/org/tasks/work.org")))
;; Shortcut key for capture
(global-set-key (kbd "C-c c") 'org-capture)
;; shortcut key for journal
(global-set-key (kbd "C-c j") 'org-journal-new-entry)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'load-path (expand-file-name "~/Org"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; add a CLOSED timestamp for clocktable purposes
(setq org-log-done 'time)

;; TODO Keywords
(setq org-todo-keywords '((type "TODO(t)" "NEXT(n)" "STRT(s)" "HOLD(h@/!)" "PROJ(p)" "|" "DONE(d!)" "CANX(c@)")))

;; Refile
(setq org-refile-targets '((org-agenda-files :maxlevel . 6 )))

;; status and timestamps into drawer
(setq org-log-into-drawer t)

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

;; Capture templates
(setq org-capture-templates
      `(("t" "todo" entry (file ,(concat org-directory "/refile.org"))
	     "* TODO %?\n")
 	("r" "respond" entry (file ,(concat org-directory "/refile.org"))
	     "* TODO Respond to %^{person} on %^{subject}\n%U")
	("n" "note"  entry (file+headline, (concat org-directory "/refile.org") "Notes")
              "* %? :NOTE:\n%U")
        ("m" "meeting" entry (file+headline, (concat org-directory "/refile.org") "Meetings")
             "** %? :MEETING: \n %^T \n %^{Attendees}\n")
        ("p" "phone call"  entry (file+headline, (concat org-directory "/refile.org") "Phone Calls")
	 "* PHONE %^{person} :PHONE:\n%U\n")	    
        ("j" "Journal Entries")
        ("je" "General Entry" entry
         (function org-journal-find-location)
         "** %<%I:%M %p> - %^{Title} \n\n%?\n\n"
         :tree-type day
         :clock-in :clock-resume
         :empty-lines 1)
        ("jj" "Journal" plain
         (function org-journal-find-location)
         "\n** %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
         :tree-type day
         :clock-in :clock-resume
         :empty-lines 1)
        ("jt" "Task Entry" plain
          (function org-journal-find-location)
          "\n** %<%I:%M %p> - Task Notes: %a\n\n%?\n\n"
          :tree-type day
          :clock-in :clock-resume
          :empty-lines 1)
	("jd" "TODO" plain
          (function org-journal-find-location)
          "\n** TODO %?\n\n"
          :tree-type day
          :empty-lines 1)	 
	("jm" "Meeting" plain
	 (function org-journal-find-location)
          "** TODO %<%Y%m%d> - Meeting: %^{SUBJECT}%? %(org-set-tags \"WORK\") \n%^T\n\n*** Attendees\n\n"
          :tree-type day
          :empty-lines 1)
	("jp" "Phone Call" plain
	 (function org-journal-find-location)
          "\n** Phone Call: %^{PERSON}%?\n\n"
          :tree-type day
          :clock-in :clock-resume
          :empty-lines 1)
	)
 )

;; ivy completion framework
(use-package ivy
  :ensure t
  :diminish (ivy-mode . "")
  :init (ivy-mode 1)
  :config
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 20)
  ;; does not count candidates
  (setq ivy-count-format "(%d/%d) ")
)

;; counsel (uses ivy) - use automatically
(use-package counsel :ensure t
  :bind*                           ; load counsel when pressed
  (("M-x"     . counsel-M-x)       ; M-x use counsel
   ("C-x C-f" . counsel-find-file) ; C-x C-f use counsel-find-file
   ("C-x C-r" . counsel-recentf)   ; search recently edited files
  )
)

;; swiper search
(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
  )
)

;; org-journal
(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j")
  :config
  (setq org-journal-dir "~/org/journal/" 
	org-journal-file-format "%Y%m%d.org"
        org-journal-date-format "%A, %d %B %Y")
        org-journal-file-type 'daily)

;; org-roam
(use-package org-roam
      :ensure t
      :init
      (setq org-roam-v2-ack t)
      (setq org-roam-node-display-template "${tags:10} ${title:100}")
      :custom
      (org-roam-directory "~/org/notes")
      (org-roam-completion-everywhere t)
      :bind (("C-c n f" . org-roam-node-find)
             ("C-c n g" . org-roam-graph)
             ("C-c n r" . org-roam-node-random)	
	     (:map org-mode-map
		   (("C-M-i"   . completion-at-point)
		    ("C-c n i" . org-roam-node-insert)
		    ("C-c n I" . org-roam-node-insert-immediate)
		    ("C-c n o" . org-id-get-create)
		    ("C-c n t" . org-roam-tag-add)
		    ("C-c n a" . org-roam-alias-add)
		    ("C-c n l" . org-roam-buffer-toggle))))
      :config
      (org-roam-setup)
)


(setq org-roam-capture-templates
      '(
	("d" "default" plain "%?"
	 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			    "#+title: ${title}\n")
	 :unnarrowed t)
	("l" "web-page-link" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n"
           :unnarrowed t)
	)      
      )

;; Bind this to C-c n I
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; org-roam-ui
;;(add-to-list 'load-path "~/.emacs.d/private/org-roam-ui")
;;(load-library "org-roam-ui")

;; deft
(use-package deft
  :ensure t
  :bind ("C-c n d" . deft)
  :commands (deft)
  :config (setq deft-directory "~/org/notes"
                deft-extensions '("md" "org" "txt")
		deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
		deft-use-filename-as-title t
		deft-recursive t))

;; Custom agenda command definitions

(setq org-agenda-window-setup 'current-window)
(setq org-agenda-custom-commands
      (quote (
               ("d" "Dashboard"
		(
		 (tags "+WORK+PRIORITY={A}"
			((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("TODO" "DONE" "CANX")))
			 (org-agenda-overriding-header "High-priority unfinished tasks (WIP):")))
		 (agenda "" nil)
		 (todo "HOLD"
		       ((org-agenda-overriding-header "Blocked Tasks")
			(org-agenda-max-todos nil )))
 
		 (todo "TODO"
                       ((org-agenda-overriding-header "Unprocessed Journal Tasks")
			(org-agenda-files '("~/org/journal")))
                       (org-agenda-text-search-extra-files nil)))
		)
	       )
	     )
      )
     
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; beancount
(add-to-list 'load-path "~/finances/beancount-mode-main/")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
(add-hook 'beancount-mode-hook #'outline-minor-mode)
(define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
(define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)

;; Org Roam Protocol (needs to be after org-roam or errors occur)
;; server-start is required to stand up emacs as a server
;;(require 'org-roam-protocol)
;;(server-start)

;; ox-hugo
;;(use-package ox-hugo
;;  :ensure t   ;Auto-install the package from Melpa
;;  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
;;  :after ox)

;; plantUML
;;(setq org-plantuml-jar-path (expand-file-name "/home/col/PlantUML/plantuml.jar"))
;;(setq plantuml-default-exec-mode 'jar)
;;(setq plantuml-default-exec-mode 'executable)
;;(setq plantuml-server-url "http://192.168.1.129:8010/plantuml")
;;(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;;(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

;; gnus - newsgroups
;;(setq user-full-name '"bikeflip")
;;(setq user-mail-address '"yourname@email.invalid")
;;(setq gnus-select-method '(nntp "news.eweka.nl"))

;; load the work org file and agenda and set to daily view
(find-file "~/org/tasks/work.org")
(org-agenda nil "d") 

;; added some comments - test
