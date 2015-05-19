(in-package "CL-USER")

(defun make-rgb (red green blue &optional alpha)
  (color:make-rgb (/ red 255s0)
                  (/ green 255s0)
                  (/ blue 255s0)
                  (and alpha (/ alpha 255s0))))

(defvar *darkula-color-table*
  '(:darkula-background  (57 57 57)
    :darkula-foreground  (169 183 198)
    :darkula-selection-background (33 66 131)
    :darkula-function-name (255 198 109)
    :darkula-comment (128 128 128)
    :darkula-type (170 73 38)
    :darkula-variable-name (152 118 170)
    :darkula-string (106 135 89)
    :darkula-keyword (204 120 50)
    :darkula-caret (187 187 187)
    :darkula-unknown-symbol (188 63 60)
    :darkula-warning-background (82 80 58)
    :darkula-warning-foreground (190 145 23)
    :darkula-matched-brace-foreground (255 239 40)
    :darkula-matched-brace-background (59 81 77)
    ))
    

(loop for list on *darkula-color-table* by #'cddr
      for name = (first list)
      for rgb = (second list)
      do
      (color:define-color-alias
       name
       (apply #'make-rgb rgb)))


(editor-color-theme:define-color-theme "darkula" ()
  :foreground :darkula-foreground
  :background :darkula-background
  :region '(:background :darkula-selection-background)
  :highlight '(:background :darkula-background)
  :font-lock-function-name-face '(:foreground :darkula-function-name)
  :font-lock-comment-face '(:foreground :darkula-comment)
  :font-lock-type-face '(:foreground :darkula-type)
  :font-lock-variable-name-face '(:foreground :darkula-variable-name :italic-p t)
  :font-lock-string-face '(:foreground :darkula-string)
  :font-lock-keyword-face '(:foreground :darkula-keyword)
  :font-lock-builtin-face '(:foreground :darkula-keyword :italic-p t)
  
  :compiler-note-highlight '(:foreground :magenta
                             :bold-p t)
  :compiler-warning-highlight '(:foreground :darkula-warning-foreground
                                :background :darkula-warning-background
                                :bold-p t)
  :compiler-error-highlight '(:foreground :darkula-unknown-symbol
                              :inverse-p t)
  :show-point-face '(:foreground :darkula-matched-brace-foreground
                     :background :darkula-matched-brace-background
                     :bold-p t)
  :interactive-input-face '(:foreground :cyan)
  :non-focus-complete-face '(:background :darkula-background))
