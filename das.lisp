(format t "The working directory is ~a" (user-homedir-pathname))
(defvar filePath)
(princ "Enter the current working directory on top of this path (i.e. '~/Mirror/aa3c/'): ")
(setq filePath (read))
(format t "The new working directory is ~a" (user-homedir-pathname))

(load (concatenate 'string (string filePath) "dependencies/quicklisp.lisp"))
(load (concatenate 'string (string filePath) "dependencies/setup.lisp"))

; File path setup and initialization
(defun load-packages()
  (ql:quickload :bt-semaphore)
  t)

(load-packages)

(setq run_key (get-universal-time))
(defvar *key* run_key)

(print *key*)
(print run_key)
(if (equalp *key* run_key) (if (setup) (format t "Setup Complete Successfully")) (format t "Setup Already Completed"))


(defun translate(list-of-conditions)
  (loop for condition in list-of-conditions
    (do tokenize string)))


(defclass constraintNetwork ()
  ((variables
    :initarg :variables
    :accessor variables)
   (domains
    :initarg :domains
    :accessor domains)
   (constraints
    :initarg :constraints
    :accessor constraints)))



