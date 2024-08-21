;;; Package --- my-shell.el
;;; Commentary:
; shell and eshell functions and declarations

;;; Code:
;; https://www.gnu.org/software/emacs/manual/html_mono/eshell.html
;; map key super-n (search key on chromebook)
(use-package eshell
 :ensure nil
 :bind ("s-n" . (lambda (exit)
				   "Bring up a full-screen eshell or restore previous config.
With a prefix argument, exit eshell before restoring previous config."
				   (interactive "P")
				   (if (string= "eshell-mode" major-mode)
					   (progn
                      (when exit
                        (insert "exit")
                        (eshell-send-input))
                      (jump-to-register :eshell-fullscreen))
                  (progn
                    (window-configuration-to-register :eshell-fullscreen)
                    (eshell)
                    (delete-other-windows)))))
 :demand t
;; :after corfu
 :init (require 'eshell)
 :config
  ;; (add-hook 'eshell-mode-hook
  ;;           (lambda ()
  ;;             (eshell/alias "ff" "find-file $1")
  ;;             (eshell/alias "d" "dired $1")
  ;;             (eshell/alias "less" "find-file-read-only $1")))

  ;; (add-hook 'eshell-mode-hook
  ;;           (lambda ()
  ;;             (setq-local corfu-auto nil)
  ;;             (corfu-mode))))
)
 
;; history autosuggestions
;;(use-package capf-autosuggest
;;  :ensure t
;;  :hook ((comint-mode eshell-mode) . capf-autosuggest-mode))
;; have to use this hook rather than :hooks in capf-autosuggest declaration
;;(add-hook 'eshell-mode-hook 'capf-autosuggest-mode)

(provide 'my-shell)
;;; my-shell.el ends here
