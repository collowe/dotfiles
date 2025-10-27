;;; Package --- my-source.el
;;; Commentary:
; source control functions and declarations

;;; Code:
;; https://github.com/magit/magit.git
(use-package magit
  :ensure t)

;; https://github.com/tuh8888/chezmoi.el
(use-package chezmoi
  :ensure t)
;;(require 'chezmoi-ediff)

(provide 'my-source)
;;; my-source.el ends here
