;;; package --- init.el
;;; Commentary:
; Emacs init.el configuration file

;;; Code:

;; DEBUG
;(setq debug-on-error t)
;(setq debug-on-quit t)
;; (advice-add 'require :before (lambda (feature &optional filename noerror)
;; 			    (if (equal feature 'project)
;; 				(debug))))

; set package archive locations
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

; initialise the package management system
(package-initialize)

; ensure use-package is installed
; https://www.gnu.org/software/emacs/manual/html_node/use-package/index.html
; I use use-package to provide package install customisation
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; modules dir
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(message "loading modules")
;; Load my modules
(require 'my-core)
(require 'my-org)
(require 'my-python)
(require 'my-completion)
(require 'my-appearance)
(require 'my-dired)
(require 'my-denote)
(unless my/aws-instance-p
 (message "home instance, loading home config")
 (require 'my-elfeed)
 (require 'my-beancount)
 (require 'my-social))
(require 'my-chatgpt)
(require 'my-shell)
(require 'my-source)
(require 'my-pdf)
(require 'my-publish)
(when (my/aws-instance-p)
  (message "work instance, loading work config")
  (require 'my-devops))
(message "init complete")

(provide 'init)
;;; init.el ends here
