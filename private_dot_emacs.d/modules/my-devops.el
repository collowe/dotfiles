;;; Package --- my-devops.el
;;; Commentary:
; Devops packages functions and declarations

;; Docker
;; https://github.com/Silex/docker.el
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package kubernetes
  :ensure t)

(provide 'my-devops)
;;; my-devops.el ends here
