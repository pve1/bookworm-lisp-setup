(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package '#:script)
    (make-package '#:script :use '(#:cl))))

(in-package #:script)

;;; Useful functions for CL-USER.

(export '(core-script-file
          load-unless
          load-once
          as-keyword-or-integer-or-string
          parse-args-to-kw-int-str
          *script-package*
          *script-default-command*))

(use-package :script :cl-user)

(defvar *script-package*)
(defvar *script-default-command* nil)

(defun core-script-file (name)
  (merge-pathnames 
   (merge-pathnames name (make-pathname :type "lisp"))
   sb-ext:*core-pathname*))

(defun as-keyword-or-integer-or-string (object)
  (cond ((eql (mismatch ":" object) 1)
         (read-from-string object))
        ((find (aref object 0) "0123456789-+")
         (let ((num? (read-from-string object)))
           (if (numberp num?)
               num?
               object)))
        (t object)))

(defun parse-args-to-kw-int-str (args)
  (mapcar #'as-keyword-or-integer-or-string args))

(defun thing-exists-p (name exist-fn &optional package &aux sym)
  (unless package
    (setf package *package*))
  (setf name (string name))
  (and (find-package package)
       (setf sym (find-symbol name package))
       (funcall exist-fn sym)))
  
(defun function-exists-p (name &optional package)
  (thing-exists-p name #'fboundp package))

(defun variable-exists-p (name &optional package)
  (thing-exists-p name #'boundp package))

(defun class-exists-p (name &optional package)
  (thing-exists-p name (lambda (x)
                         (find-class x nil))
                  package)) 

(defun %load-unless (file &key package 
                               function
                               variable
                               class)
  (when (and function (symbolp function))
    (setf function (string function)))
  (when (and variable (symbolp variable))
    (setf variable (string variable)))
  (cond ((and function
              (function-exists-p function package))
         (return-from %load-unless nil))
        ((and variable
              (variable-exists-p variable package))
         (return-from %load-unless nil))
        ((and class
              (class-exists-p class package))
         (return-from %load-unless nil))
        ((and package
              (null function)
              (null variable)
              (null class)
              (find-package package))
         (return-from %load-unless nil))
        (t (let ((*script-package* *package*)
                 (*script-default-command* *script-default-command*))
             (values (load file) t)))))

(defmacro load-unless (file &key package 
                                 function
                                 variable
                                 class)
  `(eval-when (:execute)
     (%load-unless ,file
                   :package ,package 
                   :function ,function
                   :variable ,variable
                   :class ,class)))

(defvar *load-once-loaded*)

(defun %load-once (&rest files)
  (flet ((doit ()
           (dolist (file files)
             (unless (gethash file *load-once-loaded*)
               (load file)
               (setf (gethash file *load-once-loaded*) t)))))
    (if (boundp '*load-once-loaded*)
        (doit)
        (let ((*load-once-loaded* (make-hash-table :test 'equal))
              (*script-package* *package*)
              (*script-default-command* *script-default-command*))
          (doit)))))

(defmacro load-once (&rest files)
  `(eval-when (:execute)
     (%load-once ,@files)))

(defun prefixp (prefix string &aux mismatch)
  (setf mismatch (mismatch prefix string))
  (or (null mismatch)
      (= (length prefix) mismatch)))

(defun detect-script-type (script)
  (cond ((prefixp "sys:" script)
         (values :system (subseq script 4)))
        ((prefixp "core:" script)
         (values :core-script (subseq script 5)))
        ((equal "lisp" (pathname-type script))
         (values :lisp-file script))
        (t (values :unknown script))))

;; Three modes:
;; 
;; 1. load - load specified file
;; 2. load script - load file from core directory
;; 3. load system - load system

(defun parse-command-line (argv)
  (let ((argv-length (length argv)))
    (cond ((= 1 argv-length)
           '(sb-impl::toplevel-repl nil))
          ((<= 2 argv-length)
           (destructuring-bind (core script &rest rest)
               argv
             (declare (ignore core))
             (cond ((member script '("--help" "-h" ":help") :test #'equal)
                    `(progn (format *error-output* 
                                    "~&Usage: ~A SCRIPT ARGS~%"
                                    ,(file-namestring (first argv)))
                            (sb-ext:exit :code 1)))
                   ;; Shorthand
                   (t `(load-lisp ,script ',rest))))))))
  
(defun run-command-and-exit (args &key (print t))
  (let* ((command (or *script-default-command*
                      (first args)))
         (command-args (if *script-default-command*
                           args
                           (rest args)))
         (*package* (find-package *script-package*))
         (command-symbol
           (etypecase command
             (symbol command)
             (string (read-from-string command)))))
    (let ((result (apply command-symbol command-args)))
      (when (and result print)
        (fresh-line)
        (princ result)
        (terpri))
      (sb-ext:exit :code 0))))

(defun load-lisp (script args)
  (let* ((*package* (find-package :cl-user))
         (*script-package* *package*)
         (*script-default-command* nil))
    (handler-bind ((asdf:bad-system-name #'muffle-warning))
      (multiple-value-bind (type name)
          (detect-script-type script)
        (ecase type
          ((:lisp-file :unknown) (load name))
          (:core-script (load (core-script-file name)))
          (:system (asdf:load-system name)))))
    (when (or args *script-default-command*)
      (run-command-and-exit args))
    (sb-ext:exit :code 0)))

(defun make-toplevel ()
  (lambda ()
    (sb-ext:disable-debugger)
    (eval (parse-command-line sb-ext:*posix-argv*))))

(defun build-script-core (&key core-name systems)
  (format t "Loading ~S.~%" systems)
  (apply #'asdf:load-systems systems)
  (mapcar (lambda (x)
            (asdf:register-immutable-system x))
          (asdf:already-loaded-systems))
  (sb-ext:save-lisp-and-die
   core-name
   :toplevel (make-toplevel)
   :executable t))

;;; $ sbcl --load build-script-core.lisp hello.core cl-ppcre alexandria
(eval-when (:execute)
  (build-script-core
   :core-name (second sb-ext:*posix-argv*)
   :systems (nthcdr 2 sb-ext:*posix-argv*)))
