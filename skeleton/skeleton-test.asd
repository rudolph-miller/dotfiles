#|
  This file is a part of <% @var name %> project.
  Copyright (c) <%= (local-time:timestamp-year (local-time:now)) %> Rudolph Miller (chopsticks.tk.ppfm@gmail.com)
|#

(in-package :cl-user)
(defpackage <% @var name %>-test-asd
  (:use :cl :asdf))
(in-package :<% @var name %>-test-asd)

(defsystem <% @var name %>-test
  :author "Rudolph Miller"
  :license "MIT"
  <%- @if homepage %>
  :homepage "<% @var homepage %>"
  <%- @else %>
  :homepage "https://github.com/Rudolph-Miller/<% @var name %>"
  <%- @endif %>
  :depends-on (:<% @var name %>
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "<% @var name %>"))))
  :description "Test system for <% @var name %>"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
