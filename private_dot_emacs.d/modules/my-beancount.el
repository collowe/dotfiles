;;; Package --- my-beancount.el
;;; Commentary:
; Beancount functions and declarations

;;; Code:
;; --- beancount ---
(use-package beancount
  :ensure t
  :load-path "~/.emacs.d/beancount-mode/"
  :bind(
		:map beancount-mode-map
			 ("C-c C-n" . outline-next-visible-heading)
			 ("C-c C-p" . outline-previous-visible-heading))
  :hook (beancount-mode-hook . outline-minor-mode)
  :mode ("\\.beancount\\'" . beancount-mode))

;; csv-mode
(use-package csv-mode
  :ensure t)

(provide 'my-beancount)
;;; my-beancount.el ends here
