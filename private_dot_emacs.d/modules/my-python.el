;;; Package --- my-python.el
;;; Commentary:
; Python functions and declarations

;;; Code:
;; ;; --- elpy ---
;; ;; (use-package elpy
;; ;;   :ensure t
;; ;;   :init
;; ;;   (elpy-enable))

;; ;; ;; use the standard python interpreter
;; ;; (setq python-shell-interpreter "python3"
;; ;;       python-shell-interpreter-args "-i")
;; ;; (setq elpy-rpc-virtualenv-path 'current)
;; ;; (setq elpy-rpc-python-command "python3")
;; ;; (setenv "PYTHONPATH" "/usr/bin/python3")

;; ;; Python
;; ;;
;; ;; Requires
;; ;;  pip3 install jedi autopep8 flake8 ipython importmagic yapf
;; ;; (use-package elpy
;; ;;   :straight t
;; ;;   :config
;; ;;   (elpy-enable)
;; ;;   (setq python-shell-interpreter "ipython"
;; ;;         python-shell-interpreter-args "-i --simple-prompt"))

;; ;; --- elpy ---
;; ;; (use-package elpy
;; ;;   :ensure t
;; ;;   :init
;; ;;   (elpy-enable))

;; ;; ;; use the standard python interpreter
;; ;; (setq python-shell-interpreter "python3"
;; ;;       python-shell-interpreter-args "-i")
;; ;; (setq elpy-rpc-virtualenv-path 'current)
;; ;; (setq elpy-rpc-python-command "python3")
;; ;; (setenv "PYTHONPATH" "/usr/bin/python3")

;; ;; Python
;; ;;
;; ;; Requires
;; ;;  pip3 install jedi autopep8 flake8 ipython importmagic yapf
;; ;; (use-package elpy
;; ;;   :straight t
;; ;;   :config
;; ;;   (elpy-enable)
;; ;;   (setq python-shell-interpreter "ipython"
;; ;;         python-shell-interpreter-args "-i --simple-prompt"))

(use-package pyvenv
  :ensure t)

;; eglot
(use-package eglot
  :ensure t
;; hook and mode commented for now as they introduce a defect that prevents eglot loading
;;  :hook (python-base-mode-hook . eglot-ensure)
;;  :mode(("\\.py\\'" . python-mode))
  :config
  (add-to-list 'eglot-server-programs
   	       '((python-mode) . ("pyright-langserver" "--stdio"))))

;; Enable LSP support by default in programming buffers
(add-hook 'prog-mode-hook #'eglot-ensure)

;; set python mode for any file with a .py extension
;; this is here as a :mode command in eglot config essentially defers the loading and
;; introduces this issue - https://github.com/joaotavora/eglot/discussions/1436
;; also ref - https://www.reddit.com/r/emacs/comments/tda6qm/emacs_packages_loading_order/
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

;;Create a memorable alias for `eglot-ensure'.
(defalias 'start-lsp-server #'eglot)

;;--- flycheck --- (syntax highlighting, uses pylint installed using pip)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; --- YAML ---
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$")
  :bind (
		 :map yaml-mode-map ("C-m" . newline-and-indent)))

(provide 'my-python)
;;; my-python.el ends here
