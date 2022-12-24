;;(in-package "EDITOR")
(defpackage #:lw-config.editor-commands
  (:use #:cl #:editor)
  (:add-use-defaults t))


(in-package lw-config.editor-commands)

(defun region-commented-p ()
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (with-point
       ((start  (buffer-point buffer))
        (end (buffer-mark buffer)))
      (when (point> start end)
        (rotatef start end)) ; so point<= start end
      (loop with line-is-comment = nil
            while (point< start end)
            do
            (unless (start-line-p start)               ; ensure at start of line
              (line-start start))
            (with-point ((end1 start))
              ;; skip starting spaces
              (loop while (eq (character-at end1 0) #\Space)
                    do (character-offset end1 1))
              ;; move a point to end of command prefix
              (setf line-is-comment nil)
              (loop while (eq (character-at end1 0) #\;)
                    do
                    (character-offset end1 1)
                    (setf line-is-comment t)))
            (line-offset start 1)            
            when (not line-is-comment)
            return line-is-comment
            finally (return line-is-comment)))))                   ; move to next line


(editor:defcommand "Comment Do What I Mean" (p) 
     "Similar to Emacs comment-dwim. If the region is selected,
comment it; otherwise add comment at the end of line" ""
  (let ((b (current-buffer)))
    (with-buffer-locked (b :for-modification t)
      (cond ((and (editor::region-highlighted-p b)
                  (region-commented-p))
             (uncomment-region-command p))
            ((editor::region-highlighted-p b)
             (editor::comment-region-command p))
            (t 
             (indent-for-comment-command p))))))


;; bug: removed the syntax coloring, remains the comment colors
;; http://permalink.gmane.org/gmane.lisp.lispworks.general/671
(editor:defcommand "Uncomment Region" (p)
     "Delete all sequences of ; at start of each line in region"
     "Delete comment prefixes"
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (with-point
       ((start  (buffer-point buffer)) (end (buffer-mark buffer)))
      (when (point> start end)
        (rotatef start end)) ; so point<= start end
      (collect-undo buffer
        (loop while (point< start end)
              do
              (unless (start-line-p start)               ; ensure at start of line
                (line-start start))
              (with-point ((end1 start))
                ;; skip starting spaces
                (loop while (eq (character-at end1 0) #\Space)
                      do (character-offset end1 1))
                ;; move a point to end of command prefix
                (loop while (eq (character-at end1 0) #\;)
                      do (character-offset end1 1))
                (unless (point= end1 start)
                  (when (eq (character-at end1 0) #\Space) ; remove separating space
                    (character-offset end1 1)))          ; if any
                (delete-between-points start end1))      ; delete the comment prefix
              (line-offset start 1))))                   ; move to next line
    (editor::font-lock-fontify-buffer buffer)))               

#+:mswindows
(fli:define-c-typedef fli-hwnd
  (:unsigned :long))

#+:mswindows
(fli:define-foreign-function (shell-execute "ShellExecute" :dbcs)
    ((hwnd fli-hwnd)
     (lp-operation :pointer)
     (lp-file :pointer)
     (lp-parameters :pointer)
     (lp-directory :pointer)
     (n-show-cmd :int32))
  :result-type (:unsigned :long)
  :documentation "Performs an operation on a specified file.")

(defun open-in-dash (name)
  (let ((url (concatenate 'string "dash://" name)))
    #+:mswindows
    (let ((null-ptr (fli:make-pointer :address 0 :type :void)))
      (fli:with-foreign-string (text element-count byte-count 
                                     :external-format :unicode)
          url
        (declare (ignore element-count byte-count))
        (shell-execute 0 null-ptr text  null-ptr null-ptr 5)))
    #+:cocoa
    (objc:invoke (objc:invoke "NSWorkspace" "sharedWorkspace") "openURL:" (objc:invoke "NSURL" "URLWithString:" url))))


(editor:defcommand "Search In Dash" (p &optional name)
     "Search current word in Dash"
     "Search current word in Dash"
  (let* ((name (format nil "~a" (editor:get-symbol-from-point (editor:current-point)))))
    (open-in-dash name)))


(editor:defcommand "Save File and Compile Defun" (p)
     "Save current file and compile current function"
     "Save current file and compile current function"
  (editor:save-file-command p)
  (editor:compile-defun-command p))
;;(capi:find-interface 'lw-tools::system-browser)
#|
(editor:defcommand "Browse Parent System" (p)
     "Browse Parent System"
     "Browse Parent System"
  (lw-tools::find-parent-system "/Users/alexeyv/Sources/lisp/git-api/src/pack.lisp")
   ;;(buffer-pathname (current-buffer))))
|#
;; (lispworks-tools::make-tools-menu-callback 'lispworks-tools:system-browser)
;;(capi:find-interface 'lw-tools:system-browser)

;;; (defcommand "Tools Editor" (p)
;;;      "Invokes the Editor"
;;;   (declare (ignore p))
;;;   (lispworks-tools::make-tools-menu-callback (capi:find-interface 'lw-tools:listener)));;'lispworks-tools:editor nil))
;;;     
;;; (bind-key "Tools Editor" #\meta-control-\e)

;;; (defcommand "Tools Listener" (p)
;;;      "Acts like menu Works > Tools > Listener"
;;;   (declare (ignore p))
;;;   (capi:find-interface 'lw-tools:listener))
;;;   ;(lispworks-tools::make-tools-menu-callback 'lispworks-tools:listener))
;;;     
;;; (bind-key "Tools Listener" #\meta-control-\l)


(editor:defcommand "Focus On Buffers Search" (p)
     "When 'Show Buffers List' is enabled, move focus to the filter edit of the buffers list"
     "When 'Show Buffers List' is enabled, move focus to the filter edit of the buffers list"
  ;; capi::layouts -> main layout -> panes [capi::pane-geometry] ->capi::object = capi::tab-layout ->
  (declare (ignore p))
  (when-let* ((editor-iface (capi:top-level-interface (editor::current-editor-pane)))
              (layouts (slot-value editor-iface 'capi::layouts))
              (text-layout (find-if (lambda (pane) (eq (capi:capi-object-name pane) 'lispworks-tools::text-layout)) layouts))
              (text-layout-panes (slot-value text-layout 'capi::panes))
              (filter-layout-pane (find-if (lambda (pane)
                                             (typep (slot-value pane 'capi::object) 'capi::filter-layout)) text-layout-panes))
              (filter-layout (slot-value filter-layout-pane 'capi::object))
              (descr (slot-value filter-layout 'capi:filtering-layout))
              (filter (slot-value descr 'capi:text-input-pane)))
   (capi:set-pane-focus filter)))


(defcommand "Tools Editor" (p)
     "Acts like menu Works > Tools > Editor"
  (declare (ignore p))
  ;;(lispworks-tools::make-tools-menu-callback 'lispworks-tools:editor))
  (capi:find-interface 'lw-tools:editor))
    
;;(bind-key "Tools Editor" #\meta-control-\e)

(defcommand "Tools Listener" (p)
     "Acts like menu Works > Tools > Listener"
  (declare (ignore p))
  (capi:find-interface 'lw-tools:listener))
  ;(lispworks-tools::make-tools-menu-callback 'lispworks-tools:listener))

(defcommand "Tools Object Clipboard" (p)
     "Acts like menu Works > Tools > Object Clipboard"
  (declare (ignore p))
  (capi:find-interface 'lw-tools:object-clipboard))


(defcommand "Values Clip" (p)
     "Acts like menu Values > Clip"
  (declare (ignore p))
  ;; get the current editor window
  (when-let (cw (current-window))
    ;; extract the capi text pane out of it
    (when (slot-boundp cw 'editor::text-pane)
      ;; capi text pane could be used to get the top level interface -
      ;; the Listener window itself
      (when-let (iface (capi:top-level-interface (slot-value cw 'editor::text-pane)))
        ;; now try to find the "Values" toplevel menu
        (when-let (values-menu (find-if (lambda (m)
                                          (and (slot-boundp m 'capi::title)
                                               (string= "Values"
                                                        (slot-value m 'capi::title))))
                                        (capi:interface-menu-bar-items iface)))
          (let* ((component (elt (capi:menu-items values-menu) 0))
                 (item (capi:get-collection-item component 0))
                 (menu-item (capi:get-collection-item item 2)))
            (when (capi:menu-object-enabled item)
              (funcall (capi:callbacks-selection-callback menu-item)
                       iface))))))))
;;;                        (symbol-value 'editor::*)))))))))
;;; ;;          (inspect values-menu))))))
;;        (inspect iface)))))

;;(bind-key "Tools Listener" #\meta-control-\l)

(defun open-file-from-file (&optional (filename "/tmp/edfile"))
  "Read and open a file listed in first line of text file FILENAME.
Tries to reuse existing Editor window. If second line of the file
FILENAME is the number, move the line with this number"
  (with-open-file (stream filename :direction :input)
    ;; read the first line with the file name to open
    (when-let (edit-filename (probe-file (ignore-errors (read-line stream))))
      (let ((line-number
             ;; read second line of the file - should be a line number
             (or (ignore-errors (parse-integer (read-line stream)))
                 1)))
        ;; find existing Editor or create a new one
        (if-let (editor-interface (capi:locate-interface 'lispworks-tools:editor))
            (capi:execute-with-interface
             editor-interface
             (lambda ()
               (when-let (window (editor:current-window))
                 (editor:find-line-in-file  edit-filename line-number window))))
          (let ((editor-buffer 
                 (ed edit-filename)))
            ;; TODO: Doesn't work currently.
            (editor:goto-line editor-buffer line-number)))))))
