;;; Package --- my-elfeed.el
;;; Commentary:
; Elfeed functions and declarations

;;; Code:
;; https://github.com/skeeto/elfeed
(use-package elfeed
  :bind (
		 :map global-map
			  ( "C-x w" . elfeed)
		 :map elfeed-search-mode-map
			  ("w" . 'elfeed-eww-open)
			  ("f" . 'elfeed-firefox-open))
  :config
  (setq elfeed-show-entry-switch 'display-buffer
		elfeed-db-directory "~/.elfeed"
		elfeed-enclosure-default-dir (expand-file-name "~/Downloads")
		elfeed-search-title-max-width 100
		elfeed-search-trailing-width 30
		elfeed-search-remain-on-entry t
		elfeed-set-timeout 36000))

;; https://github.com/remyhonig/elfeed-org
(use-package elfeed-org
  :ensure t
  :after elfeed
  :init
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/feeds/elfeed.org")))

;; https://github.com/karthink/elfeed-tube
;; (use-package elfeed-tube
;;   :after elfeed
;;   :demand t
;;   :config
;;   ;; (setq elfeed-tube-auto-save-p nil) ; default value
;;   ;; (setq elfeed-tube-auto-fetch-p t)  ; default value
;;   (elfeed-tube-setup)

;;   :bind (:map elfeed-show-mode-map
;;          ("F" . elfeed-tube-fetch)
;;          ([remap save-buffer] . elfeed-tube-save)
;;          :map elfeed-search-mode-map
;;          ("F" . elfeed-tube-fetch)
;;          ([remap save-buffer] . elfeed-tube-save)))

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

;; ;; (use-package elfeed-tube-mpv
;; ;;   :straight t
;; ;;   :bind (:map elfeed-show-mode-map
;; ;;               ("C-c C-f" . elfeed-tube-mpv-follow-mode)
;; ;;               ("C-c C-w" . elfeed-tube-mpv-where)))

;;  ; play YouTube videos with MPV
;; ;; (defun elfeed-play-with-mpv ()
;; ;;   "Play entry link with mpv."
;; ;;   (interactive)
;; ;;   (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
;; ;;         (quality-arg "")
;; ;;         )
;; ;;      (message "Opening %s with mpv..." (elfeed-entry-link entry))
;; ;;      (start-process "elfeed-mpv" nil "mpv" (elfeed-entry-link entry))))

;; ;; (defvar elfeed-mpv-patterns
;; ;;   '("youtu\\.?be")
;; ;;   "List of regexp to match against elfeed entry link to know whether to use mpv to visit the link.")

;; ;; ; Open in mpv when o is pressed:
;; ;; (eval-after-load 'elfeed-search
;; ;;  '(define-key elfeed-search-mode-map (kbd "o") 'elfeed-play-with-mpv))

(provide 'my-elfeed)
;;; my-elfeed.el ends here
