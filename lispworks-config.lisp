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

;; maximum editor windows
#+lw-editor
(setf editor:*maximum-ordinary-windows* nil)

;; turn off editor coloring of parenthesis
(editor::set-parenthesis-colours nil)

(require "delete-selection")
(editor:delete-selection-mode-command)

(load-config-file "editor-extensions.lisp")
(load-config-file "dvorak-binds.lisp")
(load-config-file "other-binds.lisp")
(load-config-file "lw-editor-color-theme/editor-color-theme")
(load-config-file "darkula-theme.lisp")
;; TODO: isolate echo area colors, listener colors and add them to editor-color-theme
(load-config-file "colors.lisp")


;; Set the IDEA-style color theme
(editor-color-theme:color-theme "darkula")
;; Change the background colors of LispWorks' In-place completion and
;; 'Function Arglist Displayer' windows:
;(setf capi::*editor-pane-in-place-background* :black)
;(setf capi-toolkit::*arglist-displayer-background* :black)


;; turn off backup files
(setf (editor:variable-value `editor:backups-wanted) nil)

;; do not highlight found source and show found definition at 4th line
(setf editor:*source-found-action* '(4 nil))

;; aliases for upcase/downcase region commands
(editor:define-command-synonym "Upcase Region" "Uppercase Region")
(editor:define-command-synonym "Downcase Region" "Lowercase Region")

;; add Sources/ directory to quicklisp local directories
(let ((sources-dir (cl-fad:merge-pathnames-as-directory (cl-fad:pathname-directory-pathname "~/") "Sources/")))
  (push sources-dir ql:*local-project-directories*))

;; update list of QuickLisp projects
(ql:register-local-projects)

(format *standard-output* "~%Press Cmd+F1 to show Hyperspec for symbol~%")
(format *standard-output* "Press Alt+F1 to show Documentation for symbol~%~%")

