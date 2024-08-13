;;; Package --- my-chatgpt.el
;;; Commentary:
; chat-gpt functions and declarations

;;; Code:
; chat-gpt stuff - chatgpt-shell
(use-package shell-maker
  :ensure t)
;  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :ensure t
  :requires shell-maker)
;  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el")))

;; lazy loaded api key (prevents unexpected passphrase prompt)
;; stored in .authinfo
(setq chatgpt-shell-openai-key
      (lambda ()
        (auth-source-pick-first-password :host "api.openai.com")))

(provide 'my-chatgpt)
;;; my-chatgpt.el ends here
