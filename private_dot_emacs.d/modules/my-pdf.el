;;; Package --- my-pdf.el
;;; Commentary:
; pdf functions and declarations

;;; Code:
;; ;; Special case for pdf-tools that has recently (2022) changed maintainer
;; (straight-use-package
;;  '(pdf-tools :type git :host github :repo "vedang/pdf-tools"))

;; ;; (use-package pdf-tools
;; ;;   ;;:defer t
;; ;;   ;; stop pdf-tools being automatically updated when I update the
;; ;;   ;; rest of my packages, since it would need the installation command and restart
;; ;;   ;; each time it updated.
;; ;;   :ensure t
;; ;;   ;;:pin manual
;; ;;   :mode  ("\\.pdf\\'" . pdf-view-mode)
;; ;;   :config
;; ;;   ;;(pdf-loader-install)
;; ;;   (setq-default pdf-view-display-size 'fit-height)
;; ;;   (setq pdf-view-continuous nil) ;; Makes it so scrolling down to the bottom/top of a page doesn't switch to the next page
;; ;;   ;;(setq pdf-view-midnight-colors '("#ffffff" . "#121212" )) ;; I use midnight mode as dark mode, dark mode doesn't seem to work
;; ;;   ;; :general
;; ;;   ;; (general-define-key :states 'motion :keymaps 'pdf-view-mode-map
;; ;;   ;;                     "j" 'pdf-view-next-page
;; ;;   ;;                     "k" 'pdf-view-previous-page

;; ;;   ;;                     "C-j" 'pdf-view-next-line-or-next-page
;; ;;   ;;                     "C-k" 'pdf-view-previous-line-or-previous-page

;; ;;   ;;                     ;; Arrows for movement as well
;; ;;   ;;                     (kbd "<down>") 'pdf-view-next-line-or-next-page
;; ;;   ;;                     (kbd "<up>") 'pdf-view-previous-line-or-previous-page

;; ;;   ;;                     (kbd "<down>") 'pdf-view-next-line-or-next-page
;; ;;   ;;                     (kbd "<up>") 'pdf-view-previous-line-or-previous-page

;; ;;   ;;                     (kbd "<left>") 'image-backward-hscroll
;; ;;   ;;                     (kbd "<right>") 'image-forward-hscroll

;; ;;   ;;                     "H" 'pdf-view-fit-height-to-window
;; ;;   ;;                     "0" 'pdf-view-fit-height-to-window
;; ;;   ;;                     "W" 'pdf-view-fit-width-to-window
;; ;;   ;;                     "=" 'pdf-view-enlarge
;; ;;   ;;                     "-" 'pdf-view-shrink

;; ;;   ;;                     "q" 'quit-window
;; ;;   ;;                     "Q" 'kill-this-buffer
;; ;;   ;;                     "g" 'revert-buffer

;; ;;   ;;                     "C-s" 'isearch-forward
;; ;;   ;;                    )
;; ;;   )

(provide 'my-pdf)
;;; my-pdf.el ends here
