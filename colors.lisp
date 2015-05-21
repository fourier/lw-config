#|

;; Change Background Color for Lispworks Editor.
(defun set-listener-pane-colors (x)
  (typecase x
    ;; messages in message area color
    (capi:echo-area-pane
      (setf (capi:simple-pane-foreground x) :cyan)
      (setf (capi:simple-pane-background x) :cyan))
    ;; should be output
    (capi:collector-pane
     (setf (capi:simple-pane-background x) (color:make-rgb .8 1.0 .8)
           (capi:simple-pane-foreground x) :black))
    ;; listener foreground and background
    (capi:listener-pane
     (setf (capi:simple-pane-background x) (color:make-rgb .8 .8 1.0)
           (capi::simple-pane-foreground x) :black))
    
    (capi:output-pane
     (setf (capi:simple-pane-background x) :black
           (capi::simple-pane-foreground x) :white))
    ))


(defun set-editor-pane-colors (x)
  (typecase x
    ;; messages in message area color
    (capi:echo-area-pane
      (setf (capi:simple-pane-foreground x) :green)
      (setf (capi:simple-pane-background x) :cyan))
    
    ;(capi:collector-pane
    ; (setf (capi:simple-pane-background x) (color:make-rgb .8 1.0 .8)
    ;       (capi:simple-pane-background x) :black))
    ;; listener foreground and background
    (capi:editor-pane
     (setf (capi:simple-pane-background x) :black
           (capi::simple-pane-foreground x) :white))
    ))


(let ((*HANDLE-WARN-ON-REDEFINITION* :warn)
      (*redefinition-action* :warn))
  (defmethod capi:interface-display :before ((self lw-tools:listener))
    (capi:map-pane-descendant-children
     self 'set-listener-pane-colors))
  (defmethod capi:interface-display :before ((self lw-tools:editor))
    (capi:map-pane-descendant-children
     self 'set-editor-pane-colors))

  ;;;   (defmethod capi:interface-display :before ((self lw-tools::help-interface))
;;;     (capi:map-pane-descendant-children
;;;      self 'set-pane-background-colors))

;;;    (defmethod capi:interface-display :before ((self lw-tools:output-browser))
;;;      (capi:map-pane-descendant-children
;;;       self 'set-pane-background-colors))
;;;   (defmethod capi:interface-display :before ((self lw-tools:shell))
;;;     (capi:map-pane-descendant-children
;;;      self 'set-pane-background-colors))
;;;   (defmethod capi:interface-display :before ((self lw-tools:inspector))
;;;     (capi:map-pane-descendant-children
;;;      self 'set-pane-background-colors))
  )
|#
