;;; Package --- my-dired.el
;;; Commentary:
; Dired functions and declarations

;;; Code:
;; ;; set the default listing switches to be more compact, sorted by size

; auto-update the dired buffer on revisit
(setq dired-auto-revert-buffer t)

; don't open a new buffer, use the existing one
(setf dired-kill-when-opening-new-dired-buffer t) 

;; set the default listing switches - directories first
(setq dired-listing-switches "-agho --group-directories-first")

;; font lock rules for a more colourful dired
(use-package diredfl
  :ensure t
  :after (dired)
  :config
  (diredfl-global-mode 1))

;; allow dired filtering - provides the dired-filter functions
(use-package dired-filter
  :ensure t
  :after (dired))

;; show directory sizes
(use-package dired-du
  :ensure t
  :commands (dired-du-mode)
  :config 
  (setq dired-du-size-format t))

(provide 'my-dired)
;;; my-dired.el ends here
