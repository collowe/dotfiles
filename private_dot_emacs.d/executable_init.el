;;; package --- init.el
;;; Commentary:
; Emacs init.el configuration file

;;; Code:

;; DEBUG
;(setq debug-on-error t)
;(setq debug-on-quit t)

(require 'package)

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
; I use Use-Package to provide package install customisation
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; modules dir
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load my modules
(require 'my-core)
(require 'my-org)
(require 'my-completion)
(require 'my-appearance)
(require 'my-dired)
(require 'my-elfeed)
(require 'my-denote)
(require 'my-python)
(require 'my-beancount)
(require 'my-chatgpt)
(require 'my-shell)
(require 'my-source)
(require 'my-social)
(require 'my-pdf)

(provide 'init)
;;; init.el ends here
