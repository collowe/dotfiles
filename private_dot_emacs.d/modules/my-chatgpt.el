;;; Package --- my-chatgpt.el
;;; Commentary:
; chat-gpt functions and declarations

;;; Code:
; chat-gpt stuff - chatgpt-shell
(use-package shell-maker
  :ensure t)

(use-package chatgpt-shell
  :ensure t
  :requires shell-maker)

;; lazy loaded api key (prevents unexpected passphrase prompt)
;; stored in .authinfo
(setq chatgpt-shell-openai-key
      (lambda ()
        (auth-source-pick-first-password :host "api.openai.com")))

;; https://github.com/xenodium/ob-chatgpt-shell
;; chatgpt in org-babel blocks
(use-package ob-chatgpt-shell
  :ensure t
  :config
  (ob-chatgpt-shell-setup))

(use-package gptel
  :ensure t
  :bind ("C-c RET" . gptel-send)
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-model 'claude-sonnet-4-5-20250929
		gptel-backend
		(gptel-make-bedrock "AWS"
		  :stream t
		  :region "eu-west-2"
		  :models '(claude-sonnet-4-5-20250929)
		  :aws-profile "maritime"
		  :model-region 'eu)))


(provide 'my-chatgpt)
;;; my-chatgpt.el ends here
