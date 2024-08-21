;;; Package --- my-social.el
;;; Commentary:
; social network functions and declarations

;;; Code:
;; --- Mastodon ---
(use-package mastodon
  :ensure t
  :config
  (setq mastodon-instance-url "https://emacs.ch"
		mastodon-active-user "clowe"))

;; ;;  Ement - matrix client.
;; ;; (use-package ement
;; ;;   :straight (:host github :repo "alphapapa/ement.el"))

(provide 'my-social)
;;; my-social.el ends here