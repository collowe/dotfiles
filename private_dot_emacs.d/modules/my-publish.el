;;; Package --- my-publish.el
;;; Commentary:
; publishing functions and declarations

(require 'ox-publish)

(defvar my/denote-publish-dir "/var/www/notes")
(defvar my/denote-assets-dir (expand-file-name "assets" my/denote-publish-dir))

(setq org-html-html5-fancy t)
(setq org-html-validation-link nil)
(setq org-export-with-toc t)
(setq org-export-with-section-numbers t)
(setq org-export-babel-evaluate nil)

(setq org-publish-project-alist
      `(
        ;; 1) Org → HTML
        ("denote-org"
         :base-directory ,denote-directory
		 :exclude "/home/colin.lowe/org/notes/archive/.*"
         :base-extension "org"
         :publishing-directory ,my/denote-publish-dir
         :recursive t
         :publishing-function org-html-publish-to-html
         :with-author nil
         :with-creator nil
         :with-toc t
         :section-numbers nil
         :time-stamp-file nil
		 :with-sub-superscript nil
         :auto-sitemap t
         :sitemap-title "Notes"
		 :sitemap-filename "index.org"
         :html-preamble t
         :html-postamble t
		 :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"assets/stylesheet.css\" />"
         :html-head-include-default-style nil
         :html-head-include-scripts nil
		 
		 )

;        2) Static assets (images, css, etc.)
        ("denote-static"
         :base-directory ,my/denote-assets-dir
         :base-extension "png\\|jpg\\|jpeg\\|gif\\|svg\\|pdf\\|css\\|js\\|webp\\|woff\\|woff2\\|ttf\\|eot"
         :publishing-directory ,my/denote-publish-dir
         :recursive t
         :publishing-function 'org-publish-attachment)

        ;; 3) Meta project
;;        ("denote-site" :components ("denote-org"))
        ("denote-site" :components ("denote-org" "denote-static"))		
        ))

;; Convenience command to publish quickly
(defun cl/publish-denote-site ()
  "Publish Denote-backed Org notes to HTML."
  (interactive)
  (org-publish "denote-site" t))

(provide 'my-publish)
;;; my-publish.el ends here
