;;; Package --- my-core.el
;;; Commentary:
; Core functions and declarations

;;; Code:

; machine-specific detection

(defun aws-instance-p-via-files ()
  "Check if the current machine is an AWS instance by looking for specific files."
  (let ((cloud-init-file "/var/lib/cloud/instance"))
    (file-exists-p cloud-init-file)))

(defvar my/aws-instance-p nil
  "Non-nil if the current machine is an AWS instance.")

(setq my/aws-instance-p (aws-instance-p-via-files))

;; (when my/aws-instance-p
;;   (message "This is an AWS instance, apply additional settings if needed.")
;;   ;; Additional AWS-specific configurations can go here
;; )

; no startup screen
(setq inhibit-startup-screen t)

; no startup message
(setq inhibit-startup-message t)

; no splash screen
(setq inhibit-splash-screen t)

; remove mouse pointer when typing
(setq make-pointer-invisible t)

; answer using y or n
(setq use-short-answers t)

;; hide menus
(tool-bar-mode 0)
(tooltip-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

; line wrap
(global-visual-line-mode 1)

; highlight current line
(global-hl-line-mode 1)

;; Enable electric-pair mode for automatic pairing of brackets and quotes
(electric-pair-mode 1)

;; Show matching parentheses
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Enable clipboard integration with system
(setq select-enable-clipboard t)

;; Set default tab width to 4 spaces
(setq-default tab-width 4)

;; Configure cache-dir within emacs directory
(defvar cache-dir (expand-file-name "cache" user-emacs-directory)
  "Main cache directory which packages should be configured to use.")

; create cache directory if it doesn't exist
(unless (file-exists-p cache-dir)
  (make-directory cache-dir))

; get the cache-dir
(defun cache-dir (name)
  "Return absolute path to sub-directory NAME under cache-dir."
  (expand-file-name name cache-dir))

; create backup directory if it doesn't exist
(let ((backup-dir (cache-dir "backup/")))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir)))

;; Setup backup
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

; single space sentences
(setq sentence-end-double-space nil)

; copy paste over existing text
(delete-selection-mode t)
 
; minor mode to remember recent files
(recentf-mode 1)

; save last place visited in file
(save-place-mode nil)

;; refresh buffer if file changes on disk
(setq global-auto-revert-mode t)

; custom-file settings
; use custom file location to keep init.el clean
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Ensure TRAMP is required
(require 'tramp)

;; Set TRAMP's default method to ssh
(setq tramp-default-method "ssh")

;; Configure dired to not prompt for confirmation and keep versions on remote systems
(setq dired-kept-versions 1)
(setq dired-recursive-deletes 'always)

;; Ensure dired uses the remote method for deletion
(setq delete-by-moving-to-trash nil)

(defun my-dired-do-delete-tramp (&optional arg)
  "Delete files using dired and tramp directly on remote."
  (interactive "P")
  (if (tramp-tramp-file-p (dired-get-filename))
      (let ((dired-recursive-deletes 'always)
            (delete-by-moving-to-trash nil))
        (dired-do-delete))
    (dired-do-delete arg)))

;; Bind the custom delete function to d
;(define-key dired-mode-map (kbd "d") 'my-dired-do-delete-tramp)

; --- helpful ---
;; (use-package helpful
;;   :commands (helpful-callable
;; 	     helpful-variable
;; 	     helpful-key
;; 	     helpful-macro
;; 	     helpful-function
;; 	     helpful-command))

;; Note that the built-in `describe-function' includes both functions
;; and macros. `helpful-function' is functions only, so we provide
;; `helpful-callable' as a drop-in replacement.
;; (global-set-key (kbd "C-h f") #'helpful-callable)
;; (global-set-key (kbd "C-h v") #'helpful-variable)
;; (global-set-key (kbd "C-h k") #'helpful-key)
;; (global-set-key (kbd "C-h x") #'helpful-command)
;; ;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; ;; for this in lisp modes.
;; (global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Look up *F*unctions (excludes macros).
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
;; (global-set-key (kbd "C-h F") #'helpful-function)

;; ;; multiple cursors
;; (use-package multiple-cursors
;;   :straight (multiple-cursors
;; 	     :type git
;; 	     :host github
;; 	     :repo "magnars/multiple-cursors.el"))

;; mpv
(setq mpv-default-options '("--screenshot-directory=~/temp/mpv" "--screenshot-template=%F-[%P]v%#01n"))

(provide 'my-core)
;;; my-core.el ends here
