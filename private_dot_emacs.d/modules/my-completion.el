;;; Package --- my-completion.el
;;; Commentary:
; Completion functions and declarations

;;; Code:
; https://github.com/minad/vertico
; Vertico provides a performant and minimalistic vertical completion UI based on the default completion system.
(use-package vertico
  :ensure t
  :init (vertico-mode))

; fuzzy matching in completion
(use-package orderless
  :ensure t
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

; adds detail to completions
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

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
  )

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  ;:hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  
  :config
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                   #'consult-completion-in-region)
                   ;;#'completion--in-region)
                 args)))
)

; in region completions with Corfu
(use-package corfu
  :ensure t
  :custom
  ;; (corfu-min-width 80)
  ;; (corfu-max-width corfu-min-width)
  (corfu-count 5)
  ;; (corfu-scroll-margin 4)
  (corfu-cycle t)              ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)               ;; Enable auto completion
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  ;; (corfu-separator ?\s)
  ;; (corfu-quit-at-boundary nil)
  ;; (corfu-quit-no-match t)
  ;; (corfu-preview-current nil)
  ;; (corfu-preselect-first t)
  ;; (corfu-on-exact-match t)
  ;; (corfu-echo-documentation nil)
  ;; (corfu-popupinfo-mode 1)
  :init
  (global-corfu-mode)          ;; enable corfu globally
  (corfu-history-mode)         ;; enable corfu history mode
  :config
  (add-hook 'eshell-mode-hook
			(lambda() (setq-local corfu-quit-at-boundary t
								  corfu-quit-no-match t
								  corfu-auto nil)
			  (corfu-mode))
  nil
  t))

(use-package kind-icon
  :ensure t
  :if (display-graphic-p)
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :ensure t
  :init
  (which-key-mode 1)
)

(provide 'my-completion)
;;; my-completion.el ends here
