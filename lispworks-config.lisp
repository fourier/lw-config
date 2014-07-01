;; -*- Mode: lisp; -*-
(in-package "CL-USER")
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

;; Bind Ctrl-Up/Down to previous/next commands in Listener
(editor:bind-key "History Next" "Control-Down" :mode "Pc Execute")
(editor:bind-key "History Previous" "Control-Up" :mode "Pc Execute")
;; Same for Cmd-UP/Down 
(editor:bind-key "History Next" "Hyper-Down" :mode "Pc Execute")
(editor:bind-key "History Previous" "Hyper-Up" :mode "Pc Execute")

;; Enter will indent new line
(editor:bind-key "Indent New Line" #\Return :mode "Lisp")

;; Indent as in Emacs
(editor:setup-indent "if" 2 4 4)

;; per-word movements
(editor:bind-key "Forward Word" "Control-Right" :global)
(editor:bind-key "Backward Word" "Control-Left" :global)

(editor:bind-key "Forward Word" "Hyper-Right" :global)
(editor:bind-key "Backward Word" "Hyper-Left" :global)

;; Cmd-x as extended command ?? doesn't work
(editor:bind-key "Extended Command" "Hyper-x" :global :mac)

;; Run GUI inspect when called from REPL
(setf *inspect-through-gui* t)

(editor:bind-key "Function Documentation" "Hyper-F1" :global)
(editor:bind-key "Show Documentation" "Meta-F1" :global)
;;(editor:bind-key "Function Arglist" "Meta-=" :global)
;;(editor:bind-key "Function Argument List" "Meta-A" :global)

(ql:quickload "cl-fad")

(let* ((ctrl-u-binds-file "ctrl-u-mac-binds.lisp")
       (ctrl-u-binds-path (cl-fad:merge-pathnames-as-file (cl-fad:pathname-directory-pathname *load-truename*) ctrl-u-binds-file)))
  (load ctrl-u-binds-path))
                            
;(load "ctrl-u-mac-binds.lisp")


(format *standard-output* "~%Press Cmd+F1 to show Hyperspec for symbol~%")
(format *standard-output* "Press Alt+F1 to show Documentation for symbol~%~%")

