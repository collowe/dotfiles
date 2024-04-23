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

;; Configure cache-dir
(defvar cache-dir (expand-file-name "cache" user-emacs-directory)
  "Main cache directory which packages should be configured to use.")

; create cache directory if it doesn't exist
(unless (file-exists-p cache-dir)
  (make-directory cache-dir))

(defun cache-dir (name)
  "Return absolute path to sub-directory NAME under cache-dir."
  (expand-file-name name cache-dir))

(let ((backup-dir (cache-dir "backup/")))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir)))

;; Backup files
(setq backup-by-copying t           ; Don't clobber symlinks
      delete-old-versions t         ; Delete excess backup files silently
      kept-new-versions 10          ; Number of new versions to keep
      kept-old-versions 0           ; Number of old versions to keep
      vc-make-backup-files t        ; No backup of files under version control
      version-control t             ; Version numbers for backup files
      delete-by-moving-to-trash t   ; delete files to trash
      backup-directory-alist `((".*" . ,(cache-dir "backup"))))  ; backup dir

;; Auto-save files
 (let ((auto-save-dir (cache-dir "autosave/")))
   (unless (file-exists-p auto-save-dir)
     (make-directory auto-save-dir))
   (setq auto-save-interval 200
	 auto-save-timeout 20
         auto-save-file-name-transforms
         `((".*" ,auto-save-dir t))))

;; European calendar please
(eval-after-load "calendar"
  '(setq calendar-date-style 'european
	 diary-date-forms diary-iso-date-forms))

;; bookmarks
; set the default bookmark file
(setq bookmark-default-file "~/org/notes/bookmarks/bookmarks")
; autosave bookmark file on change
(setq bookmark-save-flag 1)

(use-package org
  :straight (:type built-in)
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
  (setq org-agenda-files (list "~/org/tasks"))

  ;(setq org-agenda-hide-tags-regexp ".")
  ;(setq org-agenda-prefix-format
  ;    '((agenda . " %i %-20:c%?-12t% s") ;; '%-10' sets 10 char field width
  ;      (tags   . " %i %-12:c")
  ;      (todo   . " %i %-12:c")   
  ;      (search . " %i %-12:c")))

  (setq org-agenda-window-setup 'current-window)
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
(setq sentence-end-double-space nil) ; single space sentences
(delete-selection-mode t)         ; copy paste over existing text
(electric-pair-mode 1)            ; insert matching delims
(add-hook 'org-mode-hook 'org-indent-mode) ; always use org-indent mode
(recentf-mode 1)                  ; minor mode to remember recent files
(save-place-mode nil)             ; save last place visited in file
;;(setq org-adapt-indentation t)  ; indent content under headers
(setq split-width-threshold nil)  ; vertical split by default
(setq split-height-threshold 0)   ; vertical split by default
(setq org-roam-buffer-position 'bottom) ; org roam buffers open horizontally
(setq use-short-answers t)        ; answer using y or n
(setq isearch-lazy-count t)       ; enable hit count in isearch
(setq lazy-count-prefix-format nil) ; better formatting in lazy search count
(setq lazy-count-suffix-format "   (%s/%s)") ; better formatting in lazy search count

(setq custom-file (locate-user-emacs-file "custom-vars.el")) ; use custom file location to keep init.el clean
(load custom-file 'noerror 'nomessage)

;; refresh buffer if file changes on disk
(setq global-auto-revert-mode t)

(use-package all-the-icons
  :if (display-graphic-p)
  :straight t)

; icons in autocompletion (vertico)
(use-package all-the-icons-completion
  :config
  (all-the-icons-completion-mode))

;; doesn't play nice with eglot
;; (use-package doom-modeline
;;   :straight t
;;   ;; :if (not (display-graphic-p))
;;   :init
;;   (setq doom-modeline-env-enable-python nil)
;;   (setq doom-modeline-env-enable-go nil)
;;   (setq doom-modeline-buffer-encoding 'nondefault)
;;   (setq doom-modeline-hud t)
;;   (setq doom-modeline-persp-icon nil)
;;   (setq doom-modeline-persp-name nil)
;;   (setq doom-modeline-display-misc-in-all-mode-lines nil)
;;   :config
;;   (setq doom-modeline-minor-modes nil)
;;   (setq doom-modeline-irc nil)
;;   (setq doom-modeline-buffer-state-icon nil)
;;   (doom-modeline-mode 1))

; --- helpful ---
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

;; org-babel
(org-babel-do-load-languages
 'org-babel-load-languages '((shell . t)))

; https://github.com/minad/vertico
(use-package vertico
  :straight (vertico :type git
                     :host github
                     :repo "minad/vertico")
  :init
  (vertico-mode)
  :config
)

; fuzzy matching in completion
(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  (completion-styles '(orderless))
  (completion-category-overrides '((file (styles partial-completion)))))

; adds detail to completions
(use-package marginalia
  :straight t
  :config
  (marginalia-mode))

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

; in region completions with Corfu
(use-package corfu
  :straight t
  :custom
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  (corfu-preselect-first t)
  (corfu-on-exact-match t)
  (corfu-echo-documentation nil)
  (corfu-popupinfo-mode 1)
  ;:config
  ;(set-face-attribute 'corfu-default nil
  ;                    :background (nord-color "polar-night-0")
  ;                    :foreground (nord-color "aurora-3"))
  ;(set-face-attribute 'corfu-current nil
  ;                    :background (nord-color "frost-3")
  ;                    :foreground (nord-color "snow-storm-1"))
  ;(set-face-attribute 'corfu-annotations nil
  ;                    :foreground (nord-color "snow-storm-0"))
  :init
  (global-corfu-mode))

(use-package kind-icon
  :if (display-graphic-p)
  :straight t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; set the theme using ef-themes
;; (use-package ef-themes
;;   :ensure t)
;; ;; disable any active themes
;; (mapc #'disable-theme custom-enabled-themes)
;; (load-theme 'ef-autumn :no-confirm)

;; set default font
(set-frame-font "Iosevka Light 16" nil t)

;; csv-mode
(use-package csv-mode
  :straight (csv-mode :type git
		      :host github
		      :repo "emacs-straight/csv-mode"
		      :files ("*" (:exclude ".git"))))

;; https://github.com/protesilaos/ef-themes
(use-package ef-themes
  :straight (ef-themes :type git
		       :host gitlab
		       :repo "protesilaos/ef-themes")
   :config
   ;; disable any active themes
   (mapc #'disable-theme custom-enabled-themes)
   (load-theme 'ef-autumn :no-confirm))

;; set the theme to be dracula
;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;(mapc #'disable-theme custom-enabled-themes)
;(load-theme 'dracula t)

;; https://github.com/integral-dw/org-superstar-mode
(use-package org-superstar
  :straight (org-superstar :type git
                           :host github
                           :repo "integral-dw/org-superstar-mode")
  :after org
  :hook (org-mode . org-superstar-mode)
)

; paste link with description
(use-package org-cliplink)
(global-set-key (kbd "C-x p i") 'org-cliplink)

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
(setq dired-auto-revert-buffer t) ; auto-update the dired buffer on revisit
(setf dired-kill-when-opening-new-dired-buffer t) ; don't open a new buffer, use the existing one

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
		     :flavor melpa
		     :host github
                     :repo "tuh8888/chezmoi.el"
		     :files (:defaults "extensions/chezmoi-ediff.el")))
(require 'chezmoi-ediff)

;; --- Diary ---
(setq diary-file "~/org/diary/diary")     ;; set the calendar file
(setq calendar-latitude 53.842178)        ;; calendar location - lat
(setq calendar-longitude -1.636099)       ;; calendar location - long
(setq calendar-week-start-day 1)          ;; set calendar to start on Monday
(setq calendar-mark-diary-entries-flag t) ;; mark diary entries in calendar by default
;;
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
  :defer
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
;; (use-package org-roam-ui
;;   :straight (org-roam-ui :type git
;;                          :host github
;;                          :repo "org-roam/org-roam-ui")
;;   :after org-roam
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;; 	org-roam-ui
;;         org-roam-ui-update-on-save t
;; 	org-roam-ui-open-on-start nil))

; https://org-roam.discourse.group/t/using-consult-ripgrep-with-org-roam-for-searching-notes/1226/8
(defun col/org-roam-rg-search ()
  "Search org-roam directory using consult-ripgrep. With live-preview."
  (interactive)
  (let ((consult-ripgrep-command "rg --null --ignore-case --type org --line-buffered --color=always --max-columns=500 --no-heading --line-number . -e ARG OPTS"))
    (consult-ripgrep org-roam-directory)))
(global-set-key (kbd "C-c rr") 'bms/org-roam-rg-search)

; chat-gpt - doesn't work
(defun col/kill-line-minus-markup ()
  "Kill the current line minus the markup characters at either end."
  (interactive)
  (let ((line-start (line-beginning-position))
        (line-end (line-end-position))
        (markup-regexp "^\\(\\*+\\|/\\|_\\|\\+\\|=\\)\\(.*?\\)\\1$"))
    (save-excursion
      (goto-char line-start)
      (when (looking-at markup-regexp)
        (goto-char (match-end 2))
        (delete-region line-end (point)))
      (kill-line))))

; https://github.com/jgru/consult-org-roam
(use-package consult-org-roam
   :straight (consult-org-roam :type git
                               :host github
		               :repo "jgru/consult-org-roam")
   :after org-roam
   :init
   (require 'consult-org-roam)
   ;; Acbtivate the minor mode
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
   ("C-c n r" . consult-org-roam-search))

(defun col/org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description (if (match-end 3) 
                               (org-match-string-no-properties 3)
                             (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))

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
     
;; --- beancount ---
(add-to-list 'load-path "~/.emacs.d/beancount-mode/")
(require 'beancount)
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-mode))
(add-hook 'beancount-mode-hook #'outline-minor-mode)
(define-key beancount-mode-map (kbd "C-c C-n") #'outline-next-visible-heading)
(define-key beancount-mode-map (kbd "C-c C-p") #'outline-previous-visible-heading)

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
	 elfeed-search-title-max-width 100
         elfeed-search-trailing-width 30
         elfeed-search-remain-on-entry t)
         (elfeed-set-timeout 36000))

;; https://github.com/karthink/elfeed-tube
(use-package elfeed-tube
  :straight t
  :after elfeed
  :demand t
  :config
  ;; (setq elfeed-tube-auto-save-p nil) ; default value
  ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
  (elfeed-tube-setup)

  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)))

;; elfeed browser open functions
;; from https://noonker.github.io/posts/2020-04-22-elfeed/
(defun elfeed-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (eww-browse-url it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun elfeed-firefox-open (&optional use-generic-p)
  "open with firefox"
  (interactive "P")
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (browse-url-firefox it))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(define-key elfeed-search-mode-map (kbd "w") 'elfeed-eww-open)
(define-key elfeed-search-mode-map (kbd "f") 'elfeed-firefox-open)

;; (use-package elfeed-tube-mpv
;;   :straight t
;;   :bind (:map elfeed-show-mode-map
;;               ("C-c C-f" . elfeed-tube-mpv-follow-mode)
;;               ("C-c C-w" . elfeed-tube-mpv-where)))

 ; play YouTube videos with MPV
;; (defun elfeed-play-with-mpv ()
;;   "Play entry link with mpv."
;;   (interactive)
;;   (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
;;         (quality-arg "")
;;         )
;;      (message "Opening %s with mpv..." (elfeed-entry-link entry))
;;      (start-process "elfeed-mpv" nil "mpv" (elfeed-entry-link entry))))

;; (defvar elfeed-mpv-patterns
;;   '("youtu\\.?be")
;;   "List of regexp to match against elfeed entry link to know whether to use mpv to visit the link.")

;; ; Open in mpv when o is pressed:
;; (eval-after-load 'elfeed-search
;;  '(define-key elfeed-search-mode-map (kbd "o") 'elfeed-play-with-mpv))

; chat-gpt stuff - chatgpt-shell
(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el")))

;; lazy loaded api key (prevents unexpected passphrase prompt)
;; stored in .authinfo
(setq chatgpt-shell-openai-key
      (lambda ()
        (auth-source-pick-first-password :host "api.openai.com")))

;; weather
;; (use-package biome
;;   :straight (:host github :repo "SqrtMinusOne/biome"))

;; (setq biome-query-coords
;;       '(("Horsforth, England" 53.84260000 -1.63754000)
;;         ("Douglas, England" 55.5500000 -3.8500000)))

;;  Ement.
;; (use-package ement
;;   :straight (:host github :repo "alphapapa/ement.el"))

;; (setq biome-query-coords
;;       '(("Helsinki, Finland" 60.16952 24.93545)
;;         ("Berlin, Germany" 52.52437 13.41053)
;;         ("Dubai, UAE" 25.0657 55.17128)))

;; Special case for pdf-tools that has recently (2022) changed maintainer
(straight-use-package
 '(pdf-tools :type git :host github :repo "vedang/pdf-tools"))

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

;; --- elpy ---
;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable))

;; ;; use the standard python interpreter
;; (setq python-shell-interpreter "python3"
;;       python-shell-interpreter-args "-i")
;; (setq elpy-rpc-virtualenv-path 'current)
;; (setq elpy-rpc-python-command "python3")
;; (setenv "PYTHONPATH" "/usr/bin/python3")

;; Python
;;
;; Requires
;;  pip3 install jedi autopep8 flake8 ipython importmagic yapf
;; (use-package elpy
;;   :straight t
;;   :config
;;   (elpy-enable)
;;   (setq python-shell-interpreter "ipython"
;;         python-shell-interpreter-args "-i --simple-prompt"))

; eshell
(use-package eshell
  :bind ("s-n" . eshell)
  :demand t
  :after corfu
  :init (require 'eshell)
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell/alias "ff" "find-file $1")
              (eshell/alias "d" "dired $1")
              (eshell/alias "less" "find-file-read-only $1")))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode))))

(use-package pyvenv
  :ensure t)

;; eglot
(use-package eglot
  :ensure t
  :hook (python-base-mode-hook . eglot-ensure)
  :mode(("\\.py\\'" . python-mode))
  ;; :config
  ;; (add-to-list 'eglot-server-programs
  ;; 	       `(python-mode
  ;; 	       . ,(eglot-alternatives '(("pyright-langserver" "--stdio")
  ;;                                         "jedi-language-server"
  ;;                                        "pylsp")))))
)
  
;; --- flycheck --- (syntax highlighting, uses pylint installed using pip)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; (use-package mastodon
;;   :straight t
;;   :config (setq mastodon-instance-url "https://emacs.ch"
;; 		  mastodon-active-user "clowe"))

;; ;; --- testing nov.el ---
;; (use-package nov
;;   :ensure t
;;   :config
;;   (setq nov-text-width 80)
;;   )
;; (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; ;; load the work org file 
;; (find-file "~/org/tasks/work.org")
