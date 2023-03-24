(require 'org)

;; Setup Repos
(setq package-archives
      '(
	("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu" )
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
;; set default font
(set-frame-font "Iosevka 11" nil t)

;; change bullets to be pretty
(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
)

;; hide the markup/emphasis markers
(setq org-hide-emphasis-markers t)

;; --- Dired ---
;; set the default listing switches to be more compact, sorted by size
(setq dired-listing-switches "-lGghaS")

;; function to get the combined file size of selected files in dired
(defun dired-get-size ()
(interactive)
(let ((files (dired-get-marked-files)))
  (with-temp-buffer
    (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
    (message "Size of all marked files: %s"
             (progn
               (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
               (match-string 1))))))

;; call get size function after dired loaded
(eval-after-load "dired" '(progn
  (define-key dired-mode-map (kbd "?") 'dired-get-size) )) 

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

;; magit - chezmoi has dependency on magit
(use-package magit
  :ensure t
  )

;; chezmoi
(use-package chezmoi
  :ensure t
)

;; --- Diary ---
(setq org-agenda-include-diary t)       ;; include diary entries in the agenda
(setq diary-file "~/org/diary/diary")   ;; set the calendar file
(setq calendar-latitude 53.842178)      ;; calendar location - lat
(setq calendar-longitude -1.636099)     ;; calendar location - long
(setq calendar-week-start-day 1)        ;; set calendar to start on Monday
(setq mark-diary-entries-in-calendar t) ;; mark diary entries in calendar by default

;; --- Agenda ---
;; set the agenda files
(setq org-agenda-files (list "~/org/tasks"
			     "~/org/journal"
			     "~/org/notes/project"))

(setq org-agenda-hide-tags-regexp ".")
(setq org-agenda-prefix-format
      '((agenda . " %i %-20:c%?-12t% s") ;; '%-10' sets 10 char field width
        (tags   . " %i %-12:c")
        (todo   . " %i %-12:c")   
        (search . " %i %-12:c")))

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
   '(chezmoi ef-themes elpy plantuml-mode nord-theme alarm-clock 0blayout ox-hugo which-key org-superstar org-bars counsel ivy org-roam-ui simple-httpd websocket org-journal org-roam deft zenburn-theme try use-package magit))
 '(webjump-sites
   '(("Emacs Home Page" . "www.gnu.org/software/emacs/emacs.html")
     ("Emacs Wiki" .
      [simple-query "www.emacswiki.org" "www.emacswiki.org/cgi-bin/wiki/" ""])
     ("DuckDuckGo" .
      [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
     ("Wikipedia" .
      [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
     ("Dashboard" . "http://dashboard.home/"))))

;; --- org-agenda ---
;; custom shortcut key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)

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

;; Shortcut key for capture
(global-set-key (kbd "C-c c") 'org-capture)

;; --- org-journal ---
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

;; shortcut key for journal
(global-set-key (kbd "C-c j") 'org-journal-new-entry)

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

;; --- org ---
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'load-path (expand-file-name "~/Org"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; shortcut for work org file
(global-set-key (kbd "C-c w") (lambda () (interactive) (find-file "~/org/tasks/work.org")))

;; org TODO Keywords
(setq org-todo-keywords '((type "TODO(t)" "NEXT(n)" "STRT(s)" "HOLD(h@/!)" "PROJ(p)" "|" "DONE(d!)" "CANX(c@)")))

;; refile
(setq org-refile-targets '((org-agenda-files :maxlevel . 6 )))

;; add a CLOSED timestamp for clocktable purposes
(setq org-log-done 'time)

;; status and timestamps into drawer
(setq org-log-into-drawer t)

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

;; --- org-roam ---
(use-package org-roam
      :after org
      :ensure t
      :init
      (setq org-roam-v2-ack t)
      (setq org-roam-node-display-template "${title:*} ${tags:32}")
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
		    ("C-c n l" . org-roam-buffer-toggle)
		    ("C-c n c" . org-roam-capture))))
      :config
      (org-roam-setup)
)

(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
	 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
			    "#+title: ${title}\n")
	 :unnarrowed t)
	("h" "home project" plain "%?" :if-new
	 (file+head "~/org/notes/project/%<%Y%m>-${slug}.org"
                  "#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n#+FILETAGS: home-project\n\n")
         :unnarrowed t)
	("w" "work project" plain "%?" :if-new
	 (file+head "~/org/notes/project/%<%Y%m>-${slug}.org"
                  "#+title: ${title}\n#+created: %U\n#+filetags: work-project\n")
         :unnarrowed t)
	("m" "recurring meeting" plain "%?" :if-new
          (file+head "~/org/notes/meeting/%<%Y%m>-${slug}.org" "#+title: ${title}\n#+created: %U\n#+filetags: :meeting:\n#+startup: overview\n")
          :unnarrowed t)))      
  
;; Bind this to C-c n I
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; --- ivy completion framework ---
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

;; --- counsel --- (uses ivy) - use automatically
(use-package counsel :ensure t
  :bind*                           ; load counsel when pressed
  (("M-x"     . counsel-M-x)       ; M-x use counsel
   ("C-x C-f" . counsel-find-file) ; C-x C-f use counsel-find-file
   ("C-x C-r" . counsel-recentf)   ; search recently edited files
  )
)

;; --- swiper ---
(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-use-selectable-prompt t) ;; allow choose own option
    (global-set-key "\C-s" 'swiper)
  )
)

;; --- deft ---
(use-package deft
  :ensure t
  :bind ("C-c n d" . deft)
  :commands (deft)
  :config (setq deft-directory "~/org/notes"
                deft-extensions '("md" "org" "txt")
		deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
		deft-use-filename-as-title t
		deft-recursive t))
     
;; --- beancount ---
(add-to-list 'load-path "~/finances/beancount-mode-main/")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
(add-hook 'beancount-mode-hook #'outline-minor-mode)
(define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
(define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)

;; --- EMMS ---
;; EMMS basic configuration
(require 'emms-setup)
(emms-all)
(emms-default-players)
(setq emms-source-file-default-directory "/mnt/music/") ;; Change to your music folder

;; --- Elfeed ---
(use-package elfeed
  :ensure t
  :config
  ;(setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
  (setq elfeed-show-entry-switch 'display-buffer)
  
  (setq elfeed-db-directory "~/.elfeed")
  (setq elfeed-enclosure-default-dir (expand-file-name "~/Downloads"))
  (elfeed-set-timeout 36000)
  (setq elfeed-search-title-max-width 100)
  :bind
  ("C-x w" . elfeed ))

;; Configure Elfeed with org mode
(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/elfeed.org")))

;; --- elpy ---
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  )

;; use the standard python interpreter
(setq python-shell-interpreter "python3"
      python-shell-interpreter-args "-i")
(setq elpy-rpc-virtualenv-path 'current)
(setq elpy-rpc-python-command "python3")
(setenv "PYTHONPATH" "/usr/bin/python3")

;; --- flycheck --- (syntax highlighting, uses pylint installed using pip)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; load the work org file 
(find-file "~/org/tasks/work.org")

