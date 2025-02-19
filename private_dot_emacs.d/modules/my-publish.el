;;; Package --- my-publish.el
;;; Commentary:
; publishing functions and declarations

(require 'ox-publish)

(setq org-publish-project-alist
      `(("org-site:main"
         :base-directory "~/org/notes"
		 :exclude "~/org/notes/archive/*"
         :base-extension "org"
         :recursive nil
         :publishing-directory "~/www/"
         :publishing-function org-html-publish-to-html
		 :with-author nil
		 :with-toc t
		 :section-numbers nil
		 :time-stamp-file nil
		 :with-sub-superscript nil ;; important!!
		 ;; sitemap - list of notes
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "index"
		)))

(provide 'my-publish)
;;; my-publish.el ends here
