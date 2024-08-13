;;; Package --- my-shell.el
;;; Commentary:
; shell and eshell functions and declarations

;;; Code:
;; (use-package eshell
;;   :bind ("s-n" . eshell)
;;   :demand t
;;   :after corfu
;;   :init (require 'eshell)
;;   :config
;;   (add-hook 'eshell-mode-hook
;;             (lambda ()
;;               (eshell/alias "ff" "find-file $1")
;;               (eshell/alias "d" "dired $1")
;;               (eshell/alias "less" "find-file-read-only $1")))

;;   (add-hook 'eshell-mode-hook
;;             (lambda ()
;;               (setq-local corfu-auto nil)
;;               (corfu-mode))))

(provide 'my-shell)
;;; my-shell.el ends here
