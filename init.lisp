#+:quicklisp
(ql:quickload '(:prove :qlot) :silent t)
#+:quicklisp
(progn
  (setf prove:*enable-colors* t)
  (ros:ignore-shebang)
  (setf sb-impl::*default-external-format* :utf-8)
  (setf sb-alien::*default-c-string-external-format* :utf-8))
