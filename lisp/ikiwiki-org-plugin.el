
(require 'org-exp)

(defun ikiwiki-org-correct-link (best-link destpage)
  ;; best-link is always the same -- something like "posts/processing"
  ;; destpage can change depending on whether the page is being
  ;; inlined.  It might be something like "index" or "posts/test".  If
  ;; it's the former, then we need to keep "posts/processing", but if
  ;; it's the latter, then it needs to be "../processing".  So the
  ;; strategy is to find the directories in destpage that are prefixes
  ;; of best-link.  These can be removed from both
  ;; (non-destructively).  The path that is returned is a number of
  ;; ".."s that is the number of directories that is different between
  ;; the new destpage and best-link followed by the new best-link.  So
  ;; if we had best-link = "foo/bar/baz" and destpage =
  ;; "foo/bar/quux/test" then we would reset best-link to "baz" and
  ;; destpage to "quux/test".  We would return "../../baz".
  (if (string= destpage "index")
      best-link
   (let* ((subdirs-match-index 0)
	  (best-link (concat "/" (replace-regexp-in-string "//" "/" best-link)))
	  (destpage (concat "/"  (replace-regexp-in-string "//" "/" destpage)))
	  (best-link-len (length best-link))
	  (destpage-len (length destpage))
	  (matching-subdirs-index 0)
	  (matching-subdirs-index
	   (progn
	     (while (and subdirs-match-index (< subdirs-match-index best-link-len) (< subdirs-match-index destpage-len)
			 (string= (substring best-link 0 subdirs-match-index) (substring destpage 0 subdirs-match-index)))
	       (setq matching-subdirs-index subdirs-match-index)
	       (setq subdirs-match-index (string-match "/" destpage (1+ subdirs-match-index))))
	     matching-subdirs-index))
	  (link-prefix "")
	  (subdirs-match-index matching-subdirs-index)
	  (link-prefix
	   (progn
	     (while (and subdirs-match-index (< subdirs-match-index destpage-len))
	       (setq subdirs-match-index (string-match "/" destpage (1+ subdirs-match-index)))
	       (setq link-prefix (concat "../" link-prefix)))
	     link-prefix)))
     (concat link-prefix (substring best-link (1+ matching-subdirs-index))))))

(defun ikiwiki-org-linkify (infile outfile destpage link-hash)
  (with-temp-buffer
    (insert-file-contents infile)
    (goto-char (point-min))
    (while (re-search-forward org-bracket-link-regexp (point-max) t)
      (let* ((url-part (match-string-no-properties 1))
	     (text-part (match-string-no-properties 3))
	     (best-link (gethash url-part link-hash))
	     (image? (save-match-data (string-match (org-image-file-name-regexp) url-part))))
	(message url-part)
	(when best-link (message best-link))
	(if best-link
	    ;; internal page
	    (let* ((corrected-link (save-match-data
				     (ikiwiki-org-correct-link best-link destpage))))
	      (replace-match (concat "[[./" corrected-link "][" (or (when image? corrected-link) text-part url-part) "]]") t t))
	  ;; external page -- put a slash in front if no text part
	  ;; otherwise, leave the same
	  (when (not text-part)
	    (replace-match (concat "\\[[" url-part "]]") t t)))))
    (append-to-file (point-min) (point-max) outfile)))

(defun ikiwiki-org-scan (infile outfile)
  (with-temp-buffer
    (insert-file-contents infile)
    (org-mode)
    (let ((org-info (org-infile-export-plist)))
      (when (plist-get org-info :title)
	(save-excursion
	  (with-temp-buffer
	   (insert "#+TITLE: ")
	   (insert (plist-get org-info :title))
	   (insert "\n")
	   (append-to-file (point-min) (point-max) outfile))))
      (when (plist-get org-info :author)
	(save-excursion
	  (with-temp-buffer
	    (insert "#+AUTHOR: ")
	    (insert (plist-get org-info :author))
	    (insert "\n")
	    (append-to-file (point-min) (point-max) outfile)))))
    (goto-char (point-min))
    (while (re-search-forward org-bracket-link-regexp (point-max) t)
      (let ((url (match-string-no-properties 1)))
       (save-excursion
	 (with-temp-buffer
	   (insert url)
	   (insert "\n")
	   (append-to-file (point-min) (point-max) outfile)))))))

(defun ikiwiki-org-htmlize (infile outfile)
  (let* ((org-export-html-preamble nil)
	 (org-export-html-postamble nil)
	 (org-export-with-sub-superscripts nil)
	 (org-export-with-TeX-macros nil) ; let mathjax take care of it
	 (org-export-with-LaTeX-fragments 'mathjax)
	 (org-export-babel-evaluate nil)
	 (org-export-with-toc nil)
	 (org-info
	  (with-temp-buffer
	    (insert-file-contents infile)
	    (org-mode)
	    (list (org-infile-export-plist)
		  (org-export-as-html 3 t nil 'string t))))
	 (ret-html (cadr org-info))
	 (title (plist-get (car org-info) :title)))
    (with-temp-buffer
      (insert ret-html)
      (append-to-file (point-min) (point-max) outfile))))

(provide 'ikiwiki-org-plugin)

