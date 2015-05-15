;; -*- Mode: lisp; -*-
(in-package "CL-USER")

;; load all LispWorks patches
(load-all-patches)

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init (merge-pathnames ".quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

;; to avoid printing circular references. See heap overflow example
;; http://paste.lisp.org/+31DI
;; Here, in the parent I have a reference to the children. In a child I have
;; a reference to the parent.  Therefore when printing I get infinite
;; loops.
;; With *print-circle* the print algorithm takes care of that, and uses the
;; #= ## syntax to denote shared structures including circles.
(setf *print-circle* t)

;; do not produce error when unbalanced right paren inserted
(setf system:*right-paren-whitespace* :warn)

;; Indent as in Emacs
(editor:setup-indent "if" 2 4 4)

;; Run GUI inspect when called from REPL
(setf *inspect-through-gui* t)

;; default external file format
 (setf stream::*default-external-format* '(:utf-8 :eol-style :lf))

(ql:quickload "cl-fad")

(defun load-config-file (filename)
  (let ((file-full-path (cl-fad:merge-pathnames-as-file (cl-fad:pathname-directory-pathname *load-truename*) filename)))
    (load file-full-path)))
  
(require "delete-selection")
(editor:delete-selection-mode-command)

(load-config-file "editor-extensions.lisp")
(load-config-file "dvorak-binds.lisp")
(load-config-file "other-binds.lisp")
(load-config-file "lw-editor-color-theme/editor-color-theme")
(load-config-file "darkula-theme.lisp")
(editor-color-theme:color-theme "darkula")

;; turn off backup files
(setf (editor:variable-value `editor:backups-wanted) nil)

(format *standard-output* "~%Press Cmd+F1 to show Hyperspec for symbol~%")
(format *standard-output* "Press Alt+F1 to show Documentation for symbol~%~%")

