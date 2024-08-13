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
  :hook (python-base-mode-hook . eglot-ensure)
  :mode(("\\.py\\'" . python-mode))
  :config
  (add-to-list 'eglot-server-programs
   	       '((python-mode) . ("pyright-langserver" "--stdio")))
  ;; 	       . ,(eglot-alternatives '(("pyright-langserver" "--stdio")
  ;;                                         "jedi-language-server"
  ;;                                        "pylsp")))))
)
  
;; Enable LSP support by default in programming buffers
;(add-hook 'prog-mode-hook #'eglot-ensure)

;; Create a memorable alias for `eglot-ensure'.
(defalias 'start-lsp-server #'eglot)

;; --- flycheck --- (syntax highlighting, uses pylint installed using pip)
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
