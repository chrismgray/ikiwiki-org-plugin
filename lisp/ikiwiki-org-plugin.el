
(require 'xml-rpc)

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
    (print m-func-call (find-file-noselect "/dev/stdout"))))

(defun org-ikiwiki-hook (&rest params)
  (apply xml-rpc-method-call-stdout 'hook params))

(defun org-ikiwiki-import (&rest params)
  (org-ikiwiki-hook "type" "htmlize" "id" "org")
  (org-ikiwiki-hook "type" "linkify" "id" "org")
  (org-ikiwiki-hook "type" "scan" "id" "org"))

(defvar org-ikiwiki-methods 
  '(("import" org-ikiwiki-import)
    ("htmlize" org-ikiwiki-htmlize)
    ("linkify" org-ikiwiki-linkify)
    ("scan" org-ikiwiki-scan))
  "An alist of the methods that ikiwiki can call.")

(defun org-ikiwiki-make-rpc-xml-get-response-fn (input-buffer)
  (lambda ()
    (let* ((xml-rpc-debug 3)
	   (with-current-buffer input-buffer
	     (revert-buffer t t))
	   (xml-list (xml-rpc-request-process-buffer input-buffer))
	   (with-current-buffer input-buffer
	     (delete-region (point-min) (point-max)))
	   (method-or-response (car-safe (car-safe xml-list))))
      (if (eq method-or-response 'methodResponse)
	  (let ((valpart (cdr (cdaddr (caddar xml-list)))))
	    (xml-rpc-xml-list-to-value valpart))
	(error "Not a methodResponse")))))

(defun org-ikiwiki-compile (input-file)
  "Runs the ikiwiki compilation process."
  (let ((input-buffer (find-file-noselect input-file))
	(methods org-ikiwiki-methods)
	(org-ikiwiki-rpc-xml-get-response (org-ikiwiki-make-rpc-xml-get-response-fn input-buffer)))
    (while (file-exists-p input-file)
      (let* ((xml-rpc-debug 3)
	     (with-current-buffer input-buffer
	      (revert-buffer t t))
	     (xml-list (xml-rpc-request-process-buffer input-buffer))
	     (with-current-buffer input-buffer
	       (delete-region (point-min) (point-max)))
	     (method-or-response (car-safe (car-safe xml-list))))
	(cond
	 ((eq method-or-response 'methodResponse)
	  (let ((valpart (cdr (cdaddr (caddar xml-list)))))
	    (xml-rpc-xml-list-to-value valpart)))
	 ((eq method-or-response 'methodCall)
	  (let* ((method-name (caddr (caddar xml-list)))
		 (params (xml-rpc-clean (cadar (cdddar xml-list))))
		 (method (cadr (assoc method-name methods)))
		 (result (funcall method org-ikiwiki-rpc-xml-get-response params))
		 (m-params (mapcar '(lambda (p)
                              `(param nil ,(car (xml-rpc-value-to-xml-list
                                                 p))))
				   
				   params))
		 (m-response `((methodResponse nil
					       ,(append '(params nil) m-params)))))
	    (with-current-buffer (find-file-noselect "/dev/stdout")
	      (xml-print m-response))))
	 (t
	  (error "Not a methodCall or methodResponse")))))))

(cadar (cddar '((methodCall nil (methodName nil "import") (params nil)))))

(provide 'ikiwiki-org-plugin)
