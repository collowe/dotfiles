;;; Package --- my-completion.el
;;; Commentary:
; Completion functions and declarations

;;; Code:
; https://github.com/minad/vertico
; Vertico provides a performant and minimalistic vertical completion UI based on the default completion system.
(use-package vertico
  :ensure t
  :init (vertico-mode))

; adds detail to completions
; https://github.com/minad/marginalia
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; built in completion preview
;; https://eshelyaron.com/posts/2023-11-17-completion-preview-in-emacs.html
(use-package completion-preview
  :ensure nil
  :bind
  (:map completion-preview-active-mode-map
			  ("M-n" . #'completion-preview-next-candidate)
			  ("M-p" . #'completion-preview-prev-candidate))
  :custom
  (completion-preview-minimum-symbol-length 2)
  :init
  (global-completion-preview-mode))

;; https://github.com/justbur/emacs-which-key
;; built in, just enable it
(use-package which-key
  :ensure nil
  :init
  (which-key-mode 1)
)

;; https://github.com/minad/consult
(use-package consult
  :ensure t
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
   :map consult-narrow-map
   ("?" . consult-narrow-help)
  )
  ;; The :init configuration is always executed (Not lazy)
  :init
)

(provide 'my-completion)
;;; my-completion.el ends here
