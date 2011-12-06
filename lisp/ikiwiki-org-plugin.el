
(require 'xml-rpc)
(require 'org-exp)

(defun xml-find-nodes-matching (node name)
  "Returns all children of `node' that have an `xml-node-name' equal to `name'."
  (if (or (eq node '()) (not (listp node)))
      '()
    (if (equal (xml-node-name node) name)
	(cons node (delq nil (mapcar (lambda (nd) (xml-find-nodes-matching nd name)) (xml-node-children node))))
      (delq nil (apply 'append (mapcar (lambda (nd) (xml-find-nodes-matching nd name)) (xml-node-children node)))))))

(defun xml-print-with-preamble (xml-list)
  (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n")
  (xml-print xml-list)
  (insert "\n"))

(defun xml-rpc-wait-for-response (buffer)
  (with-current-buffer buffer
    (while (not (re-search-forward "\\(</\\(methodResponse\\|methodCall\\)>\\)" (point-max) t))
      (revert-buffer t t)
      (narrow-to-region org-ikiwiki-region-start (point-max))
      (sleep-for 0.3)
      (when (not (file-exists-p (buffer-file-name)))
	(throw 'org-ikiwiki-input-file-gone nil)))
    (setq org-ikiwiki-region-end (match-end 0))))

(defun xml-rpc-method-call-stdout (method &rest params)
  (let* ((m-name (if (stringp method)
                     method
                   (symbol-name method)))
         (m-params (mapcar '(lambda (p)
                              `(param nil ,(car (xml-rpc-value-to-xml-list
                                                 p))))
			   params))
         (m-func-call `((methodCall nil (methodName nil ,m-name)
                                    ,(append '(params nil) m-params)))))
    (with-temp-buffer
      (xml-print-with-preamble m-func-call)
      (insert "\n")
      (append-to-file (point-min) (point-max) org-ikiwiki-output-file))))

(defun org-ikiwiki-hook (&rest params)
  (apply 'xml-rpc-method-call-stdout 'hook (butlast params))
  (funcall (car (last params))))

(defun org-ikiwiki-setstate (page id key value get-response-fn)
  (xml-rpc-method-call-stdout 'setstate page id key value)
  (funcall get-response-fn))

(defun org-ikiwiki-getstate (page id key get-response-fn)
  (xml-rpc-method-call-stdout 'getstate page id key)
  (funcall get-response-fn))

(defun org-ikiwiki-getvar (hash-name hash-key get-response-fn)
  (xml-rpc-method-call-stdout 'getvar hash-name hash-key)
  (funcall get-response-fn))

(defun org-ikiwiki-add-link (page text get-response-fn &optional link-type)
  (if link-type
      (xml-rpc-method-call-stdout 'add_link page text link-type)
    (xml-rpc-method-call-stdout 'add_link page text))
  (funcall get-response-fn))

(defun org-ikiwiki-pagename (file get-response-fn)
  (xml-rpc-method-call-stdout 'pagename file)
  (funcall get-response-fn))

(defun org-ikiwiki-bestlink (page link-text get-response-fn)
  (xml-rpc-method-call-stdout 'bestlink page link-text)
  (funcall get-response-fn))

(defun org-ikiwiki-import (get-response-fn params)
  (org-ikiwiki-hook "type" "htmlize" "id" "org" "call" "htmlize" get-response-fn)
  (org-ikiwiki-hook "type" "linkify" "id" "org" "call" "linkify" "first" t get-response-fn)
  (org-ikiwiki-hook "type" "scan" "id" "org" "call" "scan" get-response-fn)
  1
  )

(defun list->hash (l)
  (let* ((ret (make-hash-table :test 'equal)))
    (while l
      (puthash (car l) (cadr l) ret)
      (setq l (cddr l)))
    ret))

(defun org-ikiwiki-linkify (get-response-fn prms)
  (let* ((params (list->hash prms))
	 (content (gethash "content" params))
	 (page (gethash "page" params))
	 (destpage (gethash "destpage" params))
	 (page-file-name (org-ikiwiki-getvar "pagesources" page get-response-fn))
	 (ret (if (not (string-match "\\.org$" page-file-name))
		  content
		(with-temp-buffer
		  (insert content)
		  (goto-char (point-min))
		  (while (re-search-forward org-bracket-link-regexp (point-max) t)
		    (let* ((url-part (match-string-no-properties 1))
		  	   (text-part (match-string-no-properties 3))
			   (whole-link (match-string-no-properties 0))
		  	   (best-link (save-match-data (org-ikiwiki-bestlink page url-part get-response-fn))))
		      (if best-link
		  	  ;; internal page
		  	  (replace-match (concat "[[./" best-link "][" (or text-part url-part) "]]") t t)
		  	;; external page -- put a slash in front if no text part
		  	;; otherwise, leave the same
		  	(when (not text-part)
		  	  (replace-match (concat "\\[[" url-part "]]") t t)))))
		  (buffer-string)))))
    ret))

(defun org-ikiwiki-scan (get-response-fn prms)
  (let* ((params (list->hash prms))
	 (page (gethash "page" params))
	 (content (gethash "content" params))
	 (page-file-name (org-ikiwiki-getvar "pagesources" page get-response-fn)))
    (when (string-match "\\.org$" page-file-name)
      (with-temp-buffer
	(insert content)
	(goto-char (point-min))
	(while (re-search-forward org-any-link-re (point-max) t)
	  (org-ikiwiki-add-link page (match-string 0) get-response-fn "org"))))
    1))

(defun org-ikiwiki-htmlize (get-response-fn prms)
  (let* ((params (list->hash prms))
	 (content (gethash "content" params))
	 (page (gethash "page" params))
	 (org-export-html-preamble nil)
	 (org-export-html-postamble nil)
	 (org-export-with-sub-superscripts nil)
	 (org-export-with-TeX-macros nil) ; let mathjax take care of it
	 (org-export-with-LaTeX-fragments 'mathjax)
	 (org-export-babel-evaluate nil)
	 (org-export-with-toc nil)
	 (org-info
	  (with-temp-buffer
	    (insert content)
	    (org-mode)
	    (list (org-infile-export-plist)
		  (org-export-as-html 3 t nil 'string t))))
	 (ret-html (cadr org-info))
	 (title (plist-get (car org-info) :title)))
    (org-ikiwiki-setstate page "meta" "title" title get-response-fn)
    ret-html))

(defvar org-ikiwiki-methods 
  '(("import" org-ikiwiki-import)
    ("htmlize" org-ikiwiki-htmlize)
    ("linkify" org-ikiwiki-linkify)
    ("scan" org-ikiwiki-scan))
  "An alist of the methods that ikiwiki can call.")

(defvar org-ikiwiki-output-file nil)
(defvar org-ikiwiki-region-start 1)
(defvar org-ikiwiki-region-end 1)

(defun org-ikiwiki-make-rpc-xml-get-response-fn (input-buffer)
  (lambda ()
    (xml-rpc-wait-for-response input-buffer)
    (let* ((xml-rpc-debug 3)
	   (xml-list
	    (with-current-buffer input-buffer
	     (xml-rpc-clean (xml-parse-region org-ikiwiki-region-start org-ikiwiki-region-end))))
	   (method-or-response (car-safe (car-safe xml-list))))
      (with-current-buffer input-buffer
	(revert-buffer t t)
	(setq org-ikiwiki-region-start org-ikiwiki-region-end)
	(narrow-to-region org-ikiwiki-region-end org-ikiwiki-region-end))
      (if (eq method-or-response 'methodResponse)
	  (let ((valpart (cdr (cdaddr (caddar xml-list)))))
	    (xml-rpc-xml-list-to-value valpart))
	(error "Not a methodResponse")))))

(defmacro org-ikiwiki-with-no-supersession-questions (&rest forms)
  (let ((old-supersession-threat (gensym)))
   `(let ((,old-supersession-threat (symbol-function 'ask-user-about-supersession-threat)))
      (fset 'ask-user-about-supersession-threat (lambda (f)))
      (unwind-protect
	  (progn ,@forms)
	(fset 'ask-user-about-supersession-threat ,old-supersession-threat)))))

(defun org-ikiwiki-compile (input-file output-file)
  "Runs the ikiwiki compilation process."
  (setq org-ikiwiki-region-start 1
	org-ikiwiki-region-end 1)
  (let* ((input-buffer (find-file-noselect input-file t))
	 (methods org-ikiwiki-methods)
	 (org-ikiwiki-rpc-xml-get-response (org-ikiwiki-make-rpc-xml-get-response-fn input-buffer))
	 (auto-revert-mode nil))
    (setq org-ikiwiki-output-file output-file)
    (org-ikiwiki-with-no-supersession-questions
     (catch 'org-ikiwiki-input-file-gone
      (while (file-exists-p input-file)
	(xml-rpc-wait-for-response input-buffer)
	(let* ((xml-rpc-debug 3)
	       (xml-list
		(with-current-buffer input-buffer
		  (xml-rpc-clean (xml-parse-region org-ikiwiki-region-start org-ikiwiki-region-end))))
	       (method-or-response (car-safe (car-safe xml-list))))
	  (with-current-buffer input-buffer
	    (revert-buffer t t)
	    (setq org-ikiwiki-region-start org-ikiwiki-region-end)
	    (narrow-to-region org-ikiwiki-region-end org-ikiwiki-region-end))
	  (cond
	   ((eq method-or-response 'methodResponse)
	    (let ((valpart (cdr (cdaddr (caddar xml-list)))))
	      (xml-rpc-xml-list-to-value valpart)))
	   ((eq method-or-response 'methodCall)
	    (let* ((method-name (caddr (caddar xml-list)))
		   (params (mapcar
			    (lambda (node)
			      (xml-rpc-xml-list-to-value
			       (list node)))
			    (xml-find-nodes-matching (car xml-list) 'value)))
		   (method (cadr (assoc method-name methods)))
		   (result (funcall method org-ikiwiki-rpc-xml-get-response params))
		   (m-params (list `(param nil ,(car (xml-rpc-value-to-xml-list result)))))
		   (m-response `((methodResponse nil
						 ,(append '(params nil) m-params)))))
	      (with-temp-buffer 
		(xml-print-with-preamble m-response)
		(append-to-file (point-min) (point-max) org-ikiwiki-output-file))))
	   (t
	    (error "Not a methodCall or methodResponse")))))))
    (setq org-ikiwiki-region-start 1
	  org-ikiwiki-region-end 1)
    (message "done with that file!")))

(provide 'ikiwiki-org-plugin)
