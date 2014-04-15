
(require 'ikiwiki-org-plugin)

(require 'ert)

(ert-deftest test-org-generates-html ()
  "Simple test showing that org generates HTML"
  (let ((output
         (ikiwiki-org-htmlize-strings "#+TITLE: Foo\n\nBar")))
    (should (string= output "<p>\nBar</p>\n"))))

(ert-deftest test-more-complicated-org-generates-html ()
  ""
  (let ((output
         (ikiwiki-org-htmlize-strings "#+TITLE: Foo\n\n* Bar\n** Baz\nQuux")))
    (should (string= output "\n<div id=\"outline-container-sec-1\" class=\"outline-2\">\n<h2 id=\"sec-1\"><span class=\"section-number-2\">1</span> Bar</h2>\n<div class=\"outline-text-2\" id=\"text-1\">\n</div><div id=\"outline-container-sec-1-1\" class=\"outline-3\">\n<h3 id=\"sec-1-1\"><span class=\"section-number-3\">1.1</span> Baz</h3>\n<div class=\"outline-text-3\" id=\"text-1-1\">\n<p>\nQuux</p>\n</div>\n</div>\n</div>\n"))))
