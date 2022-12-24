; -*- Mode: lisp; -*-
(in-package "CL-USER")

;; load all LispWorks patches

#-:LISPWORKS-PERSONAL-EDITION (load-all-patches)
#+(and LISPWORKS-PERSONAL-EDITION MSWINDOWS) (load "C:/apps/asdf/asdf.lisp")

;;; The following lines added by ql:add-to-init-file:
#-quicklisp
(let ((quicklisp-init #+:MSWINDOWS "C:/apps/quicklisp/setup.lisp"
					  #-:MSWINDOWS (merge-pathnames ".quicklisp/setup.lisp"
                                                                        (user-homedir-pathname))))
  (format *standard-output* "ql init: ~s" quicklisp-init)
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))


#+(and LISPWORKS7.0 :MSWINDOWS)
(scm:require-private-patch "C:/Sources/lisp/lw-config/shell-open-command.ofasl" :hqn-web)


;; add Sources/ directory to quicklisp local directories
(push (pathname #+:MSWINDOWS "C:/Sources/lisp" #-:MSWINDOWS "~/Sources/lisp") ql:*local-project-directories*)

;; update list of QuickLisp projects
(ql:register-local-projects)


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

;; When recompiling a file with an existing defpackage, do not warn
;; when the defpackage is modified
;; Default value: (:WARN :MODIFY)
;; See http://www.lispworks.com/documentation/lw61/LW/html/lw-803.htm
;; for details
(setf hcl:*handle-existing-defpackage* '(:MODIFY))

;; Fix for OSX El Captain 
#+cocoa(remhash "NSGlyphStorage" objc::*interned-protocols*)

;; default external file format
(setf stream::*default-external-format* '(:utf-8 :eol-style :lf))

;; editor file format
(setf (editor:variable-value 'editor:output-format-default
                             :global) '(:utf-8 :eol-style :lf))

;; maximum editor windows
#+lw-editor
(setf editor:*maximum-ordinary-windows* nil)

;; turn off editor coloring of parenthesis
(editor::set-parenthesis-colours nil)

(require "delete-selection")
(editor:delete-selection-mode-command t)

;; turn off backup files
(setf (editor:variable-value `editor:backups-wanted) nil)

;; do not highlight found source and show found definition at 4th line
(setf editor:*source-found-action* '(4 nil))

;; Add Esc as a interrupt key together with Ctrl-g
(editor:set-interrupt-keys '("Escape" "Control-g"))


;; aliases for upcase/downcase region commands
(editor:define-command-synonym "Upcase Region" "Uppercase Region")
(editor:define-command-synonym "Downcase Region" "Lowercase Region")

;; the following two forms make sure the "Find Source" command works
;; with the editor source
#-:lispworks-personal-edition
(load-logical-pathname-translations "EDITOR-SRC")

#-:lispworks-personal-edition
(setf dspec:*active-finders*
        (append dspec:*active-finders*
                (list "EDITOR-SRC:editor-tags-db")))


(flet ((load-config-file (filename)
         (compile-file (lw:current-pathname filename) :load t)))
  (load-config-file "editor-extensions.lisp")
  (load-config-file "dvorak-binds.lisp")
  (load-config-file "other-binds.lisp")
  (load-config-file "lw-editor-color-theme/editor-color-theme.lisp")
  (load-config-file "darkula-theme.lisp")
  ;; TODO: isolate echo area colors, listener colors and add them to editor-color-theme
  (load-config-file "colors.lisp")
  )

;; configuration of the cl-project
(setf cl-project:*skeleton-directory*
      (merge-pathnames "Sources/lisp/skeleton/"
                       #+:MSWINDOWS "C:/" #-:MSWINDOWS(user-homedir-pathname)))

(let ((lw-project
	   (merge-pathnames "Sources/lisp/lw-project/lw-project.lisp"
						#+:MSWINDOWS "C:/" #-:MSWINDOWS (user-homedir-pathname))))
  (when (fad:file-exists-p lw-project)
	(load lw-project)))

;; Set the IDEA-style color theme

#+(and (not lispworks-personal-edition) windows) (editor-color-theme:color-theme "default")
#+(or cocoa linux) (editor-color-theme:color-theme "darkula")

;; Change the background colors of LispWorks' In-place completion and
;; 'Function Arglist Displayer' windows:
;; (setf capi::*editor-pane-in-place-background* :black)
;; (setf capi-toolkit::*arglist-displayer-background* :black)


;; start the Editor after the startup
;;(define-action "Initialize LispWorks Tools" "Ensure an Editor"
;;  (lambda (&optional screen) (capi:find-interface 'lw-tools:editor :screen screen)))


;; start the System Browser after the startup
;;(define-action "Initialize LispWorks Tools" "Ensure System Browser"
;;  (lambda (&optional screen) (capi:find-interface 'lw-tools:system-browser :screen screen)))

;; downcase symbols then printing
(setf *print-case* :downcase)

;; set extensions for asd same as for lisp
(editor:define-file-type-hook ("asd" "system") (buffer type)
  (declare (ignore type))
  (setf (editor:buffer-major-mode buffer) "Lisp"))



