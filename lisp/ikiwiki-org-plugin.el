
(require 'xml-rpc)

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
    (while (not (re-search-forward "</\\(methodResponse\\|methodCall\\)>" (point-max) t))
      (revert-buffer t t)
      (narrow-to-region org-ikiwiki-region-start (point-max)))))

(defun xml-rpc-method-call-stdout (method &rest params)
  "Call an XML-RPC method asynchronously at SERVER-URL named METHOD with \
PARAMS as parameters. When the method returns, ASYNC-CALLBACK-FUNC will be \
called with the result as parameter."
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
  (apply 'xml-rpc-method-call-stdout 'hook params))

(defun org-ikiwiki-setstate (page id key value)
  (xml-rpc-method-call-stdout 'setstate page id key value))

(defun org-ikiwiki-import (get-response-fn params)
  (org-ikiwiki-hook "type" "htmlize" "id" "org" "call" "htmlize")
  (funcall get-response-fn)
;  (org-ikiwiki-hook "type" "linkify" "id" "org" "call" "linkify")
;  (org-ikiwiki-hook "type" "scan" "id" "org" "call" "scan")
  "1"
  )

(defun list->hash (l)
  (let* ((ret (make-hash-table :test 'equal)))
    (while l
      (puthash (car l) (cadr l) ret)
      (setq l (cddr l)))
    ret))

(defun org-ikiwiki-htmlize (get-response-fn params)
  (let* ((content (gethash "content" params))
	 (page (gethash "page" params))
	 (org-export-html-preamble nil)
	 (org-export-html-postable nil)
	 (org-export-with-toc nil)
	 (ret-html
	  (with-temp-buffer
	    (insert content)
	    (org-mode)
	    (org-replace-region-by-html (point-min) (point-max))
	    (buffer-substring-no-properties (point-min) (point-max)))))
    ret-html))

(defvar org-ikiwiki-methods 
  '(("import" org-ikiwiki-import)
    ("htmlize" org-ikiwiki-htmlize)
    ("linkify" org-ikiwiki-linkify)
    ("scan" org-ikiwiki-scan))
  "An alist of the methods that ikiwiki can call.")

(defvar org-ikiwiki-output-file nil)
(defvar org-ikiwiki-region-start 1)

(defun org-ikiwiki-make-rpc-xml-get-response-fn (input-buffer)
  (lambda ()
    (xml-rpc-wait-for-response input-buffer)
    (let* ((xml-rpc-debug 3)
	   (xml-list
	    (with-current-buffer input-buffer
	     (xml-rpc-clean (xml-parse-region (point-min) (point-max)))))
	   (method-or-response (car-safe (car-safe xml-list))))
      (with-current-buffer input-buffer
	(revert-buffer t t)
	(setq org-ikiwiki-region-start (point-max))
	(narrow-to-region (point-max) (point-max)))
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
  (let* ((input-buffer (find-file-noselect input-file t))
	 (methods org-ikiwiki-methods)
	 (org-ikiwiki-rpc-xml-get-response (org-ikiwiki-make-rpc-xml-get-response-fn input-buffer))
	 (auto-revert-mode nil))
    (setq org-ikiwiki-output-file output-file)
    (org-ikiwiki-with-no-supersession-questions
     (while (file-exists-p input-file)
       (xml-rpc-wait-for-response input-buffer)
       (let* ((xml-rpc-debug 3)
	      (xml-list
	       (with-current-buffer input-buffer
		 (xml-rpc-clean (xml-parse-region (point-min) (point-max)))))
	      (method-or-response (car-safe (car-safe xml-list))))
	 (with-current-buffer input-buffer
	   (revert-buffer t t)
	   (setq org-ikiwiki-region-start (point-max))
	   (narrow-to-region (point-max) (point-max)))
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
		  (result (funcall method org-ikiwiki-rpc-xml-get-response (list->hash params)))
		  (m-params (list `(param nil ,(car (xml-rpc-value-to-xml-list result)))))
		  (m-response `((methodResponse nil
						,(append '(params nil) m-params)))))
	     (with-temp-buffer 
	       (xml-print-with-preamble m-response)
	       (append-to-file (point-min) (point-max) org-ikiwiki-output-file))))
	  (t
	   (error "Not a methodCall or methodResponse"))))))
    (message "done with that file!")))

(provide 'ikiwiki-org-plugin)
