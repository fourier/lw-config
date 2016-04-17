;;; This a stripped-down version of the code SIMPLE-SYMBOL-BROWSER 
;;; example in the LispWorks IDE. 

(defpackage #:package-browser
  (:documentation "Simple browser of all symbols in package")
  (:use #:cl #:capi #:alexandria)
  ;; these names should be from alexandria rather than lispworks
  (:shadowing-import-from #:alexandria if-let removef when-let* appendf copy-file with-unique-names nconcf when-let)
  (:export show-package-browser)
  (:add-use-defaults t))

(in-package #:package-browser)

(defvar *default-type-filter* :all)
(defvar *symbol-types*
    '(:all :functions :classes :structures
      :variables :constants :keywords :others))


;;; These two are used when sorting the symbols by the
;;; package (click on the Home-Package header)

(defun symbol-lessp (symbol1 symbol2)
  (declare (optimize (safety 0)))
  (let ((p1 (symbol-package symbol1))
        (p2 (symbol-package symbol2)))
    (if p1
        (if p2
            (if (eq p1 p2)
                (string-lessp (symbol-name symbol1) (symbol-name symbol2))
              (string-lessp (package-name p1) (package-name p2)))
          t)
      (if p2
          nil
        (string-lessp (symbol-name symbol1) (symbol-name symbol2))))))
          
(defun symbol-greaterp (symbol1 symbol2)
  (declare (optimize (safety 0)))
  (let ((p1 (symbol-package symbol1))
        (p2 (symbol-package symbol2)))
    (if p1
        (if p2
            (if (eq p1 p2)
                (string-greaterp (symbol-name symbol1) (symbol-name symbol2))
              (string-greaterp (package-name p1) (package-name p2)))
          t)
      (If p2
          nil
        (string-greaterp (symbol-name symbol1) (symbol-name symbol2))))))

;;; These are the sorting types for the symbol pane
;;; (selected by clicking on the headers). 

(defvar *package-browser-sorting-types*
  (list
   (capi:make-sorting-description :type :Home-Package
                             :sort 'symbol-lessp
                             :reverse-sort 'symbol-greaterp)
   (capi:make-sorting-description :type :Name
                             :sort 'string-lessp
                             :reverse-sort 'string-greaterp)
  
   ))


(defmacro package-browser-accessibility (self)
  `(capi:choice-selected-item (slot-value ,self 'accessibility-pane)))

;;; LispWorks 5 does not have exported interface to the filter
;;; LispWorks 6 has an exported interface. 

(defmacro package-browser-filter (self)
  #+lispworks5 `(capi::filtered-pane-filter (slot-value ,self 'symbols-pane))
  #-lispworks5 `(capi:list-panel-filter-state (slot-value ,self 'symbols-pane)))

;;----------------------------------------------------------------------------
;; The symbol browser itself
;;----------------------------------------------------------------------------


(capi:define-interface package-browser ()
  ((visible-symbols :initform nil :accessor package-browser-visible-symbols)

   (display-type-filter :initform *default-type-filter*
                        :initarg :display-type-filter
                        :accessor package-browser-display-type-filter)
   (list-up-to-date :initform nil)
   (state :initform nil :accessor package-browser-state))
  (:panes
   (matches-pane
     capi:display-pane
    :title "Total Matches:"
    :title-position :left
    :title-adjust :center
    :visible-min-width '(character 6)
    :visible-max-width t)
   (symbols-pane
     capi:multi-column-list-panel
    :filter t
    :color-function 'color-symbol-if-defined
    :visible-min-height '(:character 6)
    :interaction :extended-selection
    :columns '((:title :Home-Package) (:title :Name))
    :column-function #'(lambda (symbol)
                         (let ((p (symbol-package symbol)))
                           (list (and p (package-name p))
                                 (symbol-name symbol))))
    :sort-descriptions *package-browser-sorting-types*
    :header-args (list :print-function 'string-capitalize
                       :selection-callback #'(lambda (interface data)
                                               (let ((slist (slot-value interface 'symbols-pane)))
                                                 (capi:sorted-object-sort-by slist data))))
   
    :print-function #'(lambda (symbol)
                        (package-browser-print-symbol capi:interface symbol))
    :selection-callback 'package-browser-display-symbol-description
    :retract-callback #'(lambda (ignore self)
                          (declare (ignore ignore))
                          (package-browser-display-symbol-description nil self)
                          (capi:redisplay-menu-bar self))
    :action-callback 'package-browser-edit-source
    :callback-type :data-interface)
  
   (description-pane
     capi:multi-column-list-panel
    :visible-max-width nil
    :columns '((:title "Attribute" :visible-min-width 50) 
               (:title "value" :visible-min-width 50))
    :visible-min-height '(character 2))
   (package-options
    capi:option-pane
    :items (mapcar #'package-name (list-all-packages))
;;;    :print-function 'string-capitalize
    :interaction :single-selection
    :callback-type :data-interface
    :selected-item (package-name (find-package "COMMON-LISP"))
    :selection-callback #'on-select-package
    :visible-max-width t
    :title "Package:"
    :title-position :left
    )
   (symbol-types
    capi:option-pane
    :items *symbol-types*
    :print-function 'string-capitalize
    :interaction :single-selection
    :callback-type :data-interface
    :selected-item (package-browser-display-type-filter capi:interface)
    :selection-callback #'(setf package-browser-display-type-filter)
    :visible-max-width t
    :title "Type:"
    :title-position :left
    )
   (accessibility-pane
     capi:option-pane
    :print-function 'string-capitalize
    :items '(:all :present :externals-only :internals-only)
    :callback-type :interface
    :selection-callback 'package-browser-update-visible-symbols 
    :title "Accessibility:"
    :title-position :left
    )
   )
  
  (:layouts
   (options-layout
     capi:row-layout
    '(show-layout nil ;matches-pane
                  )
    :y-adjust :center
    )
   (show-layout
     capi:row-layout
    '(symbol-types accessibility-pane)
    :title "Show"
    :title-position :frame)
   (text-layout
    capi:column-layout
    '(symbols-pane))
   (select-layout
     capi:column-layout
    '(package-options options-layout)
    :title-position :frame
    :title "Search Settings")
    
   (main-layout
     capi:column-layout
    '(select-layout text-layout :divider  description-pane )
    :ratios '(  3 9 nil 1))
   )
  (:extra-initargs '(:accessibility))

  (:default-initargs
   :layout 'main-layout
   :title "Package Browser"
   :best-height 600
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Colouring of the items in the list-panel. Works only on Windows and GTK. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *sb-fbound-and-special-color* :green)
(defvar *sb-fbound-and-class-color* :purple)
(defvar *sb-fbound-color* :red)
(defvar *sb-specialp-color* :blue)
(defvar *sb-specialp-and-class-color* :white)
(defvar *sb-class-color* :orange)

;;; This is the :COLOR-FUNCTION of the symbols pane.
;;; See the doc for COLOR-FUNCTION in the manual entry for CAPI:LIST-PANEL

(defun color-symbol-if-defined (lp symbol state)
  (declare (ignore lp))
  (when (eq state :normal)
    (let ((fboundp (fboundp symbol))
          (specialp (sys:declared-special-p symbol))
          (classp (find-class symbol nil)))
      (cond (fboundp
             (cond (specialp *sb-fbound-and-special-color*)
                   (classp *sb-fbound-and-class-color*)
                   (t *sb-fbound-color*)))
            (specialp (if classp 
                          *sb-specialp-and-class-color*
                        *sb-specialp-color*))
            (classp *sb-class-color*)
            (t nil)))))
          

;;----------------------------------------------------------------------------
;; package-browser-state
;;----------------------------------------------------------------------------

;;; The PACKAGE-BROWSER-STATE object keeps the current state of the interface. 
;;; The IDE tool keeps a record of these to implement the History. 

(defstruct (package-browser-state
            (:print-function print-package-browser-state))
  search-string
  (symbols :unknown))

(defun print-package-browser-state (self stream level)
  (declare (ignore level))
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~S" (package-browser-state-search-string self))))

;;; Accessors that access values on the state. 

(defun package-browser-search-string (self)
  (when-let (state (package-browser-state self))
    (package-browser-state-search-string state)))

(defun package-browser-symbols (self)
  (when-let (state (package-browser-state self))
    (let ((symbols (package-browser-state-symbols state)))
      (when (listp symbols)
        symbols))))

(defun (setf package-browser-symbols) (symbols self)
  (when-let (state (package-browser-state self))
    (setf (package-browser-state-symbols state) symbols)
    (package-browser-update-visible-symbols self)))


;;----------------------------------------------------------------------------


(defmethod initialize-instance :after  ((self package-browser)
                                   &key
                                   (regexp nil regexpp)
                                   (accessibility nil accessibility-p)
                                   filter
                                   (display-type-filter *default-type-filter*
                                                        display-type-filter-p)
                                   &allow-other-keys)
  (when accessibility-p
    (setf (package-browser-accessibility self)
          accessibility))
  (when filter
    (setf (package-browser-filter self) filter))
  (when display-type-filter-p
    (setf (package-browser-display-type-filter self) display-type-filter))
  (when regexpp
    (setf (package-browser-state self) 
          (package-browser-coerce-to-state regexp self))))


;;; The IDE has a mechanism to keep a history of states. Here
;;; interface-history is just a dummy. 

(defun interface-history (self)
  (declare (ignore self))
  nil)

;;; Convert to object to a package-browser-state, which
;;; keeps the state of the interface. 
(defun package-browser-coerce-to-state (object self)
  (typecase object
    (package-browser-state object)
    (string
     (let ((old-state (find object (interface-history self)
                            :test 'string=
                            :key 'package-browser-state-search-string)))
       (if old-state
           (progn
             ;; Make it redo the regexp-find-symbols when given a string.
             (setf (package-browser-state-symbols old-state) :recompute)
             old-state)
         (make-package-browser-state
          :search-string object))))))

;;; Dummy. The IDE has a more complex mechanism to take various
;;; settings into account.

(defun package-browser-print-symbol (self symbol)
  (declare (ignore self)) 
  (prin1-to-string symbol))


;;; Update the description pane at the bottom.

(defun update-description-pane (desc-pane symbol)
  (setf (capi:collection-items desc-pane)
        (list (list "Function" (when (fboundp symbol) (symbol-function symbol)))
              (list "Value" (when (boundp symbol) (symbol-value symbol))))))

;;; This is the selection callback in the symbols pane,
;;; and also used elsewhere to update the description pane. 

(defun package-browser-display-symbol-description (symbol self)
  (with-slots (description-pane) self
    (update-description-pane description-pane symbol)))


(defun package-browser-update-current-view (self)
  nil)
;;;   (with-slots (apropos-pane) self
;;;     (let ((search-string (package-browser-search-string self)))
;;;       (setf (capi:text-input-pane-text apropos-pane) 
;;;             search-string)
;;;       (package-browser-update-visible-symbols self))))


;;; This computes which of the symbols that matches the
;;; regexp also fits the "accessibility" and the "type"

(defmethod package-browser-compute-visible-symbols ((self package-browser))
  (format t "called package-browser-compute-visible-symbols~%")
  (let* ((current-package (find-package (choice-selected-item (slot-value self 'package-options))))
         (type (package-browser-display-type-filter self))
         (accessibility (package-browser-accessibility self)))
    (loop for symbol in (package-browser-symbols self)
      for name = (symbol-name symbol)
      for package = (symbol-package symbol)
      when (and
            (eq package current-package)
            (symbol-of-type symbol type)
                (or (eq accessibility :all)
                    (or (eq accessibility :present)
                        (if (eq (nth-value 1 (find-symbol name current-package))
                                :external)
                            (eq accessibility :externals-only)
                            (eq accessibility :internals-only)))))
      collect symbol)))

;;; The "visible symbols" are the symbols that match the 
;;; "accessibility" and "type" settings. 

(defun package-browser-update-visible-symbols (self )
  (with-slots (matches-pane list-up-to-date) self
    (let ((visible-symbols (package-browser-compute-visible-symbols self)))
      (setf (package-browser-visible-symbols self) visible-symbols)
      (setf (capi:display-pane-text matches-pane)
            (format nil "~D" (length visible-symbols)))
      (setf  list-up-to-date nil)
      (update-symbols-list-pane self t)
      (package-browser-display-symbol-description (car visible-symbols) self)
      (capi:redisplay-menu-bar self))))


;;; Setters that also need to do updating 

(defmethod (setf package-browser-display-type-filter)
     :after ((options t) (self package-browser))
  (package-browser-update-visible-symbols self))


;;; Update functions. 
(defmethod interface-update (self)
  nil)
;;;   (with-slots (apropos-pane) self
;;;     (let ((search-string (package-browser-search-string self)))
;;;       (setf (capi:text-input-pane-text apropos-pane) 
;;;             search-string)
;;;       (when (> (length search-string) 0)
;;;         (package-browser-update-symbols self)))))


(defmethod (setf package-browser-state) :after ((string t)
                                           (self package-browser))
  (let ((state (package-browser-state self)))
    (if (and state
             (listp (package-browser-state-symbols state)))
        (package-browser-update-current-view self)
      (interface-update self))))



(defun symbol-of-type (symbol type)
  (or (eq type :all)
      (case type
        (:keywords
         (keywordp symbol))
	(:classes
	 (find-class symbol nil))
	(:functions
	 (fboundp symbol))
        (:constants
         (and (boundp symbol)
              (constantp symbol)
              (not (keywordp symbol))))
	(:variables
         (and (boundp symbol)
              (not (constantp symbol))))
	(:structures
	 (when-let (class (find-class symbol nil))
	   (typep class 'structure-class)))
	(t
	 (not (or (find-class symbol nil)
		  (fboundp symbol)
		  (boundp symbol)))))))




(defun update-symbols-list-pane (self update)
  (when (or update
            (not (slot-value self 'list-up-to-date)))
    (with-slots (symbols-pane) self
      (let* ((visible-symbols (package-browser-visible-symbols self))
             (old-selection (capi:choice-selected-items symbols-pane))
             (new-list visible-symbols))
        (setf (capi:collection-items symbols-pane) new-list)
        (setf (capi:choice-selected-items symbols-pane) old-selection)
        (setf (slot-value self 'list-up-to-date) t)))))


;;; Dummies for implementing editing the source.  Rely on 
;;; having the LispWorks IDE, in particular on having an interface of type 
;;; lw-tools::editor that capi:call-editor can be called with it. 

(defun my-find-interface  () (capi:find-interface 'lw-tools::editor))
(defun my-call-function (interface data)
  (capi:call-editor interface (list 'editor:find-source-command
                                    nil data)))

;;; The action callback of the list of symbols

(defun package-browser-edit-source  (data self)  
  (declare (ignore self))
  (when-let (interface (my-find-interface))
    (capi:execute-with-interface interface
                                 'my-call-function interface
                                 data)))


(defmethod on-select-package (selected (self package-browser))
  (format t "select package: ~a~%" selected)
  (let (symbols)
    (do-symbols (symb (find-package selected))
      (push symb symbols))
  (capi:with-busy-interface (self)
    (setf (package-browser-symbols self)
          symbols))))

;;; Entry point

(defun show-package-browser ()
  (capi:display  (make-instance 'package-browser 
                                :regexp "DEFAULT")))
