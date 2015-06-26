#|
  This file is a part of <% @var name %> project.
  Copyright (c) <%= (local-time:timestamp-year (local-time:now)) %> Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#
<%
(when (getf env :description)
%>
#|
<%- @if description %>
  <% @var description %>
<%- @endif %>
  Author: Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#
<% ) %>
(in-package :cl-user)
(defpackage <% @var name %>-asd
  (:use :cl :asdf))
(in-package :<% @var name %>-asd)

(defsystem <% @var name %>
  :version "0.1"
  :author "Rudolph Miller"
  :license "MIT"
  <%- @if homepage %>
  :homepage "<% @var homepage %>"
  <%- @else %>
  :homepage "https://github.com/Rudolph-Miller/<% @var name %>"
  <%- @endif %>
  :depends-on (:cl-syntax
               :cl-syntax-annot
               <% (format t "~{:~(~A~)~^~%               ~}"
                          (getf env :depends-on)) %>)
  :components ((:module "src"
                :components
                ((:file "<% @var name %>"))))
  :description "<% @var description %>"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op <% @var name %>-test))))
