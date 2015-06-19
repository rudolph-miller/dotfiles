(in-package :cl-user)
(defpackage <% @var name %>
  (:use :cl
        :annot.doc))
(in-package :<% @var name %>)

;; blah blah blah.

(syntax:use-syntax :annot)
