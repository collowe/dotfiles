;; https://github.com/raxod502/straight.el/issues/757#issuecomment-839764260
;;(defvar comp-deferred-compilation-deny-list ())

;; https://github.com/raxod502/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package org :straight (:type built-in)
  :init
  :bind 
  :config
  (setq org-startup-folded t) 
  (setq org-hide-emphasis-markers t)
  (setq org-todo-keywords '((type "TODO(t)" "NEXT(n)" "STRT(s)" "HOLD(h@/!)" "PROJ(p)" "|" "DONE(d!)" "CANX(c@)")))
  (setq org-refile-targets '((org-agenda-files :maxlevel . 6 )))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-return-follows-link t)
  
  (setq org-agenda-include-diary t)
  (setq org-agenda-files (list "~/org/tasks"
			       "~/org/journal"
			       "~/org/notes/project"))

  (setq org-agenda-hide-tags-regexp ".")
  (setq org-agenda-prefix-format
      '((agenda . " %i %-20:c%?-12t% s") ;; '%-10' sets 10 char field width
        (tags   . " %i %-12:c")
        (todo   . " %i %-12:c")   
        (search . " %i %-12:c")))

  (setq org-agenda-window-setup 'current-window)
  (setq org-agenda-custom-commands
      (quote
       (
	("d" "Dashboard"
	 (
	  (tags "+WORK+PRIORITY={A}"
		(
		 (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("TODO" "DONE" "CANX")))
		 (org-agenda-overriding-header "High-priority unfinished tasks (WIP):")))
		 (agenda "" nil)
		 (todo "HOLD"
		       (
			(org-agenda-overriding-header "Blocked Tasks")
			(org-agenda-max-todos nil)
			)
		       )
		 (todo "TODO"
                       (
			(org-agenda-overriding-header "Unprocessed Journal Tasks")
			(org-agenda-files '("~/org/journal"))
			)
                       (org-agenda-text-search-extra-files nil)))))))
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
          :empty-lines 1)))
  
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c w") (lambda () (interactive) (find-file "~/org/tasks/work.org"))))

;;  '(org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
;;  '(org-agenda-span 'day)

;; --- org ---
(add-to-list 'load-path (expand-file-name "~/org")) ; default path
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
;; set default apps
;;(setq org-file-apps '(("\\.ods\\'" \.system) (auto-mode . emacs)))

;; Interface
;; remove startup and splash screens
(setq inhibit-startup-screen t)    ; no startup screen
(setq inhibit-startup-message t)   ; no startup message
(setq inhibit-splash-screen t)     ; no splash screen
(defalias 'yes-or-no-p 'y-or-n-p)  ; y or n rather than yes or no
(setq make-pointer-invisible t)    ; remove mouse pointer when typing
;; hide menus
(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-visual-line-mode 1)       ; line wrap
(global-hl-line-mode 1)           ; highlight current line
(electric-pair-mode 1)            ; insert matching delims
(add-hook 'org-mode-hook 'org-indent-mode) ; always use org-indent mode
(recentf-mode 1)                  ; minor mode to remember recent files
(save-place-mode nil)             ; save last place visited in file
;;(setq org-adapt-indentation t) ;; indent content under headers
;;(setq split-height-threshold nil) ;; horizontal split by default
;;(setq split-width-threshold 0) ;; horizontal split by default

;; refresh buffer if file changes on disk
(setq global-auto-revert-mode t)

(use-package all-the-icons
  :if (display-graphic-p)
  :straight t)

(use-package doom-modeline
  :straight t
  ;; :if (not (display-graphic-p))
  :init
  (setq doom-modeline-env-enable-python nil)
  (setq doom-modeline-env-enable-go nil)
  (setq doom-modeline-buffer-encoding 'nondefault)
  (setq doom-modeline-hud t)
  (setq doom-modeline-persp-icon nil)
  (setq doom-modeline-persp-name nil)
  (setq doom-modeline-display-misc-in-all-mode-lines nil)
  :config
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-irc nil)
  (setq doom-modeline-buffer-state-icon nil)
  (doom-modeline-mode 1))

;; prevent backups (files ending ~)set backup directory
(setq backup-directory-alist '(("." . "~/.emacs-backups/backups/"))) 
(setq auto-save-file-name-transforms '((".*" ,"~/.emacs-backups/saves/" t)))

(setq backup-by-copying t    ; Don't delink hardlinks
      delete-old-versions t  ; Clean up the backups
      version-control t      ; Use version numbers on backups,
      kept-new-versions 5    ; keep some new versions
      kept-old-versions 2)   ; and some old ones, too

(use-package helpful
  :straight t
  :commands (helpful-callable
	     helpful-variable
	     helpful-key
	     helpful-macro
	     helpful-function
	     helpful-command))

;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
(global-set-key (kbd "C-h f") #'helpful-callable)
(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)
(global-set-key (kbd "C-h x") #'helpful-command)
;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Look up *F*unctions (excludes macros).
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

; https://github.com/minad/vertico
(use-package vertico
  :straight (vertico :type git
                     :host github
                     :repo "minad/vertico")
  :init
  (vertico-mode)
  :config
)

;; https://github.com/minad/consult
(use-package consult
  :straight (consult :type git
                     :host github
                     :repo "minad/consult")
  :bind
  (
   ;; C-c bindings (mode-specific-map)
   ("C-c h" . consult-history)
   ("C-c m" . consult-mode-command)
   ("C-c k" . consult-kmacro)

   ;; C-x bindings (ctl-x-map)
   ("C-x M-:" . consult-complex-command)     ; orig. repeat-complex-command
   ("C-x b"   . consult-buffer)              ; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark)            ; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer)      ; orig. project-switch-to-buffer

   ;; M-s bindings (search-map)
   ("M-s d" . consult-find)
   ("M-s D" . consult-locate)
   ("M-s g" . consult-grep)
   ("M-s G" . consult-git-grep)
   ("M-s r" . consult-ripgrep)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s m" . consult-multi-occur)
   ("M-s k" . consult-keep-lines)
   ("M-s u" . consult-focus-lines)
  )

  :config
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                   #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
)

;; set the theme using ef-themes
;; (use-package ef-themes
;;   :ensure t)
;; ;; disable any active themes
;; (mapc #'disable-theme custom-enabled-themes)
;; (load-theme 'ef-autumn :no-confirm)

;; set default font
(set-frame-font "Iosevka 11" nil t)

;; https://github.com/protesilaos/ef-themes
(use-package ef-themes
  :straight (ef-themes :type git
		       :host gitlab
		       :repo "protesilaos/ef-themes")
   :config
   ;; disable any active themes
   (mapc #'disable-theme custom-enabled-themes)
   (load-theme 'ef-autumn :no-confirm))
 
;; https://github.com/integral-dw/org-superstar-mode
(use-package org-superstar
  :straight (org-superstar :type git
                           :host github
                           :repo "integral-dw/org-superstar-mode")
  :after org
  :hook (org-mode . org-superstar-mode)
)

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :straight (which-key :type git
                       :host github
                       :repo "justbur/emacs-which-key")
  :init
  (which-key-mode 1)
)

;; --- Dired ---
;; set the default listing switches to be more compact, sorted by size
;;(setq dired-listing-switches "-lGghaS")

;; backups - get backup files out of the way
(setq auto-save-list-file-prefix nil)
(setq backup-directory-alist
      `(("*.*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `(("*.*" ,temporary-file-directory t)))

;; --- Dired Additions ---
(use-package diredfl
  :straight t
  :after (dired)
  :config
  (diredfl-global-mode 1))

; allow filtering
(use-package dired-filter
  :straight t
  :after (dired))

; show icons in dired
(use-package all-the-icons-dired
  :straight t
  ;;:if (not (or my/slow-ssh (not (display-graphic-p))))
  :hook (dired-mode . (lambda ()
			(unless (string-match-p "/gnu/store" default-directory)
			  (all-the-icons-dired-mode))))
  :config)

; show directory sizes
(use-package dired-du
  :straight t
  :commands (dired-du-mode)
  :config
  (setq dired-du-size-format t))

;; https://github.com/magit/magit.git
(use-package magit
  :straight (magit :type git
                   :host github
                   :repo "magit/magit")
)

;; https://github.com/tuh8888/chezmoi.el
(use-package chezmoi
  :straight (chezmoi :type git
                     :host nil
                     :repo "https://github.com/tuh8888/chezmoi.el")
)

;; --- Diary ---
(setq diary-file "~/org/diary/diary")   ;; set the calendar file
(setq calendar-latitude 53.842178)      ;; calendar location - lat
(setq calendar-longitude -1.636099)     ;; calendar location - long
(setq calendar-week-start-day 1)        ;; set calendar to start on Monday
(setq mark-diary-entries-in-calendar t) ;; mark diary entries in calendar by default

;; --- org-journal ---
(use-package org-journal
  :straight (org-journal :type git
                  :host github
                  :repo "bastibe/org-journal")

  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j")
  :config
  (global-set-key (kbd "C-c j") 'org-journal-new-entry)
  (setq org-journal-dir "~/org/journal/" 
    org-journal-file-format "%Y%m%d.org"
    org-journal-date-format "%A, %d %B %Y")
    org-journal-file-type 'daily)

;; custom function to return the current journal's location
;; Open today's journal, but specify a non-nil prefix argument in order to
;; inhibit inserting the heading; org-capture will insert the heading.
(defun org-journal-find-location ()
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))

(use-package emacsql-sqlite
  :defer t
  :straight (:type built-in))

;; https://github.com/org-roam/org-roam
(use-package org-roam
  :straight (org-roam :type git
                      :host github
                      :repo "org-roam/org-roam")
  :after org
  :init
  (setq org-roam-v2-ack t
        org-roam-node-display-template "${title:*} ${tags:32}"
        org-roam-file-extensions '("org" "md")
        org-roam-db-location (file-truename "~/org/notes/org-roam.db"))
   (let ((directory (file-truename "~/org/notes/")))
     (make-directory directory t)
     (setq org-roam-directory directory
     ;; Define a directory that does not change along with the Org-Roam folder
     col-org-roam-root-directory directory))
  :config
  (org-roam-setup)
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
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
	 ("C-c n I" . org-roam-node-insert-immediate)
         (:map org-mode-map
	    ("C-c n o" . org-id-get-create)
	    ("C-c n t" . org-roam-tag-add)
	    ("C-c n a" . org-roam-alias-add))))
  
;; Insert a new org roam file and link without opening in a new bufferBound to C-c n I
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

;; --- org-roam-ui ---
(use-package org-roam-ui
  :straight (org-roam-ui :type git
                         :host github
                         :repo "org-roam/org-roam-ui")
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
	org-roam-ui-open-on-start nil))

;; ; https://org-roam.discourse.group/t/using-consult-ripgrep-with-org-roam-for-searching-notes/1226/8
;; (defun bms/org-roam-rg-search ()
;;   "Search org-roam directory using consult-ripgrep. With live-preview."
;;   (interactive)
;;   (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
;;     (consult-ripgrep org-roam-directory)))
;; (global-set-key (kbd "C-c rr") 'bms/org-roam-rg-search)

; https://github.com/jgru/consult-org-roam
(use-package consult-org-roam
   :straight (consult-org-roam :type git
                               :host github
		               :repo "jgru/consult-org-roam"
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Activate the minor mode
   (consult-org-roam-mode 1)
   :custom
   ;; Use `ripgrep' for searching with `consult-org-roam-search'
   (consult-org-roam-grep-func #'consult-ripgrep)
   ;; Configure a custom narrow key for `consult-buffer'
   (consult-org-roam-buffer-narrow-key ?r)
   ;; Display org-roam buffers right after non-org-roam buffers
   ;; in consult-buffer (and not down at the bottom)
   (consult-org-roam-buffer-after-buffers t)
   :config
   ;; Eventually suppress previewing for certain functions
   (consult-customize
    consult-org-roam-forward-links
    :preview-key (kbd "M-."))
   :bind
   ;; Define some convenient keybindings as an addition
   ("C-c n e" . consult-org-roam-file-find)
   ("C-c n b" . consult-org-roam-backlinks)
   ("C-c n l" . consult-org-roam-forward-links)
   ("C-c n r" . consult-org-roam-search)))

;; --- deft ---
;; (use-package deft
;;   :straight (deft :type git
;;                   :host github
;;                   :repo "jrblevin/deft")
;;   :bind
;;     ("C-c n d" . deft)
;;   :commands
;;     (deft)
;;   :config
;;     (setq deft-directory "~/org/notes"
;;           deft-extensions '("md" "org" "txt")
;;           deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
;;           l-use-filename-as-title t
;;           deft-recursive t))
     
;; ;; --- beancount ---
;; (add-to-list 'load-path "~/finances/beancount-mode-main/")
;; (require 'beancount)
;; (add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
;; (add-hook 'beancount-mode-hook #'outline-minor-mode)
;; (define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
;; (define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)

;; ;; --- EMMS ---
;; ;; EMMS basic configuration
;; (require 'emms-setup)
;; (emms-all)
;; (emms-default-players)
;; (setq emms-source-file-default-directory "/mnt/music/") ;; Change to your music folder

;; https://github.com/remyhonig/elfeed-org
 (use-package elfeed-org
   :straight (elfeed-org   :type git
                           :host github
                           :repo "remyhonig/elfeed-org")
   :init
   :config
     (elfeed-org)
     (setq rmh-elfeed-org-files (list "~/org/feeds/elfeed.org")))

;; https://github.com/skeeto/elfeed
(use-package elfeed
  :straight (elfeed  :type git
		     :host github
		     :repo "skeeto/elfeed")
   :bind
   ("C-x w" . elfeed)
   :config
   ;(setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
   (setq elfeed-show-entry-switch 'display-buffer
         elfeed-db-directory "~/.elfeed"
         elfeed-enclosure-default-dir (expand-file-name "~/Downloads")
	 elfeed-search-title-max-width 150
         elfeed-search-trailing-width 30)
         (elfeed-set-timeout 36000))

;; ; play YouTube videos with MPV
;; (defun elfeed-play-with-mpv ()
;;   "Play entry link with mpv."
;;   (interactive)
;;   (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
;;         (quality-arg "")
;;         )
;;     (message "Opening %s with mpv..." (elfeed-entry-link entry))
;;     (start-process "elfeed-mpv" nil "mpv" (elfeed-entry-link entry))))

;; (defvar elfeed-mpv-patterns
;;   '("youtu\\.?be")
;;   "List of regexp to match against elfeed entry link to know whether to use mpv to visit the link.")

;; ; Open in mpv when o is pressed:
;; (eval-after-load 'elfeed-search
;;  '(define-key elfeed-search-mode-map (kbd "o") 'elfeed-play-with-mpv))

;; (use-package pdf-tools
;;   ;;:defer t
;;   ;; stop pdf-tools being automatically updated when I update the
;;   ;; rest of my packages, since it would need the installation command and restart
;;   ;; each time it updated.
;;   :ensure t
;;   ;;:pin manual
;;   :mode  ("\\.pdf\\'" . pdf-view-mode)
;;   :config
;;   ;;(pdf-loader-install)
;;   (setq-default pdf-view-display-size 'fit-height)
;;   (setq pdf-view-continuous nil) ;; Makes it so scrolling down to the bottom/top of a page doesn't switch to the next page
;;   ;;(setq pdf-view-midnight-colors '("#ffffff" . "#121212" )) ;; I use midnight mode as dark mode, dark mode doesn't seem to work
;;   ;; :general
;;   ;; (general-define-key :states 'motion :keymaps 'pdf-view-mode-map
;;   ;;                     "j" 'pdf-view-next-page
;;   ;;                     "k" 'pdf-view-previous-page

;;   ;;                     "C-j" 'pdf-view-next-line-or-next-page
;;   ;;                     "C-k" 'pdf-view-previous-line-or-previous-page

;;   ;;                     ;; Arrows for movement as well
;;   ;;                     (kbd "<down>") 'pdf-view-next-line-or-next-page
;;   ;;                     (kbd "<up>") 'pdf-view-previous-line-or-previous-page

;;   ;;                     (kbd "<down>") 'pdf-view-next-line-or-next-page
;;   ;;                     (kbd "<up>") 'pdf-view-previous-line-or-previous-page

;;   ;;                     (kbd "<left>") 'image-backward-hscroll
;;   ;;                     (kbd "<right>") 'image-forward-hscroll

;;   ;;                     "H" 'pdf-view-fit-height-to-window
;;   ;;                     "0" 'pdf-view-fit-height-to-window
;;   ;;                     "W" 'pdf-view-fit-width-to-window
;;   ;;                     "=" 'pdf-view-enlarge
;;   ;;                     "-" 'pdf-view-shrink

;;   ;;                     "q" 'quit-window
;;   ;;                     "Q" 'kill-this-buffer
;;   ;;                     "g" 'revert-buffer

;;   ;;                     "C-s" 'isearch-forward
;;   ;;                    )
;;   )

;; ;; --- elpy ---
;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable)
;;   )

;; ;; use the standard python interpreter
;; (setq python-shell-interpreter "python3"
;;       python-shell-interpreter-args "-i")
;; (setq elpy-rpc-virtualenv-path 'current)
;; (setq elpy-rpc-python-command "python3")
;; (setenv "PYTHONPATH" "/usr/bin/python3")

;; ;; --- flycheck --- (syntax highlighting, uses pylint installed using pip)
;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))

;; ;; --- testing nov.el ---
;; (use-package nov
;;   :ensure t
;;   :config
;;   (setq nov-text-width 80)
;;   )
;; (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; ;; load the work org file 
;; (find-file "~/org/tasks/work.org")
