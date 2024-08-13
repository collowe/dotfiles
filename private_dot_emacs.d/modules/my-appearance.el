;;; Package --- my-appearance.el
;;; Commentary:
; Appearance functions and declarations

;;; Code:
;; set the theme using ef-themes
(use-package ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-elea-dark))

;; disable any active themes
;(mapc #'disable-theme custom-enabled-themes)
;(load-theme 'ef-autumn :no-confirm)

;; set default font
(set-frame-font "Iosevka Light 16" nil t)

;; https://github.com/integral-dw/org-superstar-mode
(use-package org-superstar
  :ensure t
  :after org
  :hook (org-mode . org-superstar-mode)
)

; load all the icons if using a graphical display
; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
  :demand t
  :if (display-graphic-p))

; show icons in dired
(use-package all-the-icons-dired
  :ensure t
  :demand t
  :hook (dired-mode . (lambda ()
			(unless (string-match-p "/gnu/store" default-directory)
			  (all-the-icons-dired-mode))))
  :config)

; icons in autocompletion (vertico)
; removed as it currently doesn't work
; https://github.com/iyefrat/all-the-icons-completion
;(use-package all-the-icons-completion
;  :ensure t
;  :demand t
;  :after (all-the-icons)
;  :init (all-the-icons-completion-mode))
;  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

; isearch variables
; enable hit count in isearch
(setq isearch-lazy-count t)
; better formatting in lazy search count
(setq lazy-count-prefix-format nil) 
(setq lazy-count-suffix-format "   (%s/%s)")

(provide 'my-appearance)
;;; my-appearance.el ends here
