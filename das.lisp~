
; The first part of the progam requires the user to enter in the local file
; location of the folder. This will only be asked every run by storing a key
; globally called filePath
(format t "The working directory is ~a" (user-homedir-pathname))
(defvar filePath nil)

(if (not filePath)
   (progn (princ "Enter the current working directory on top of this path (i.e. '~/Mirror/aa3c/'): ")
   (setq filePath (read))))

(setf *default-pathname-defaults* (truename (format nil "~a" filePath)))

; (format t "The new working directory is ~a" (user-homedir-pathname))

(load (concatenate 'string (string filePath) "dependencies/quicklisp.lisp"))
(load (concatenate 'string (string filePath) "dependencies/setup.lisp"))

; This is sort of the "import" function where I can choose which packages I want to load
(defun load-packages()
  (ql:quickload :bt-semaphore)
  t)

(load-packages)

(format t "Setup Complete Successfully")


; This part of the program takes an input file, and then begins to convert the 
; text file lines to a data format the program can work with.
(setf fileName "puzzleOne.txt")


(setq domains nil)
(setq domtitles nil)
(setq domgrid nil)
(setq rules nil)

(defun addDomain(domName domList)
  (setq domains (append domains (list (append (list domName) (list  domList))))))


(defun counte ()
  (setf counter (+ counter 1)))

(defun position-in-list (letter list)
  (setf counter -1)
  (cond
    ((null list) nil)
    ((equalp (car list) letter) (counte))
    (t (position-in-list letter (cdr list)) (counte))))

(defun in-list (letter list)
  (cond
    ((null list) nil)
    ((equalp (car list) letter) t)
    (t (in-list letter (cdr list)))))

(defun tocase (rule)
  (if (numeric-string-p rule) rule (concatenate 'string "\"" rule "\"")))

(defun toConstraint(rule)
  (setq ops '("==" "!=" "<" ">"))
  (setq constList (uiop:split-string rule :separator " "))
  (setf pos 0)
  (setf opType nil)
  (dolist (op ops) 
    (if (in-list op constList)
        (progn
        (setq pos (position-in-list op constList))
        (setq opType op))))

  (setf leftRule (format nil "~{~A ~}" (subseq constList 0 pos)))
  (setf rightRule (format nil "~{~A ~}" (subseq constList (+ pos 1) (list-length constList))))

  (setf finLeft (totest leftRule))
  (if (in-list "is" ( uiop:split-string rightRule :separator " ")) (setf finRight (totest rightRule)) (setf finRight (tocase (string-trim " " rightRule))))
  (cond 
   ((equalp optype "==") (lambda (og) (equalp (eval (read-from-string finleft)) (eval (read-from-string finright)))))
   ((equalp optype ">") (lambda (og) (> (eval (read-from-string finleft)) (eval (read-from-string finright)))))
   ((equalp optype "<") (lambda (og) (< (eval (read-from-string finleft)) (eval (read-from-string finright)))))
   ((equalp optype "!=") (lambda (og) (not (equalp (eval (read-from-string finleft)) (eval (read-from-string finright))))))
))

(defun retVal (liste)
  (if (numeric-string-p liste) (string (concatenate 'string " " liste " " )) (string (concatenate 'string " \"" liste "\" " ))))

(defun toTest(constraint)
  (setq split (uiop:split-string constraint :separator " "))
  (setf testCount 0)
  (setf firstString "(getVal og ")
  
  (setf firstString (concatenate 'string  firstString (retval (nth 0 split)) " "))
  (setf firstString (concatenate 'string  firstString (retval (nth 4 split)) " "))
  (setf firstString (concatenate 'string  firstString (retval (nth 2 split)) ")" " "))
  (setf firstString (concatenate 'string firstString (format nil "~{~A ~}" (nthcdr (+ (position-in-list "find" split) 2) split)) ")"))
  
  (if (null (in-list "is" split)) (setf firststring constraint))

  (setf mathOps '("+" "-" "/" "*"))
  (setf workina ( uiop:split-string firstString :separator " "))
  (setf counta 0)

  (dolist (op mathops)
    (if (in-list op workina)
        (dolist (part workina)
          (progn 
            (setf counta (+ 1 counta))
            (if (equalp part op)
                (setf firstString (concatenate 'string "(" op " " (format nil "~{~A ~}" (reverse (nthcdr (- (+ (list-length workina) 1) counta) (reverse workina)))) " " (car (nthcdr counta workina)) ")")))))))


  firstString
)

(defun getVal(perms known unknown value)
  (setf posGet (position-in-list known domtitles))
  (setf posFetch (position-in-list value (dict known)))
  (setf numberValueOfKnown (nth posFetch (nth posGet perms)))
  (setf unPosGet (position-in-list unknown domtitles))
  (setf lastCol (position-in-list numberValueOfKnown (nth unPosGet perms)))
  (setf outVal (nth lastCol (nth unPosGet domgrid)))
  outVal)
  
(defun dict(categ)
  (setf ret nil)
  (dolist (dom domains)
    (if (equalp (car dom) categ)
        (setf ret (cdr dom))))
  (car ret))

(defun numeric-string-p (string)
  (let ((*read-eval* nil))
    (ignore-errors (numberp (read-from-string string)))))

(defun diff(a b)
(if (not (null b))
  (if (in-list a b) nil (diff (car b) (cdr b))) t))

(setf totstrings nil)
(defun addstring (string)
  (setf totstrings (append totstrings (list string))))

(setf diffCase nil)

(defun readFile()
    (let ((file (open fileName)))
      (setq domainNumb (parse-integer (format nil "~a~%" (read-line file))))
      (setq domainLength (parse-integer (format nil "~a~%" (read-line file))))
      (read-line file)
      (dotimes (n domainNumb)
        (setf domNameHead (read-line file))
        (setf domGridHead (uiop:split-string (read-line file) :separator " "))
        
        (setf renounce nil)
        (if (NUMERIC-STRING-P (car domGridHead))
           (dolist (val domGridHead)
              (setf renounce (cons (parse-integer val) renounce))))

        (if (NUMERIC-STRING-P (car domGridHead)) (setf domGridHead (reverse renounce)))
        
        (setq domtitles (append domtitles (list domNameHead)))
        (setq domgrid (append domgrid (list domGridHead)))
        (addDomain domNameHead domGridHead))
      (read-line file)

      (setq constraintNumb (parse-integer (format nil "~a~%" (read-line file))))
      (setf totA nil)
      (setf rules nil)

      (dotimes (n constraintNumb)
        (progn
          (setf current (read-line file))
          (if (not (equalp current "diff")) (addstring current)
            (loop for line = (read-line file nil :eof) 
              until (eq line :eof)
              do (setf totA (cons line totA))))))
      (if (not (null totA))
          (setf diffcase (lambda (perms) 
                          (progn 
                            (setf listVals nil)
                            (dolist (element totA)
                              (progn 
                                (if (in-list "is" (uiop:split-string element :separator " "))
                                    (setf listVals (cons (eval (read-from-string (totest element))) listVals)) (setf listVals (cons element listVals)))))
                            (diff (car listVals) (cdr listVals))))))
    (close file)))
(readFile)

; (setq perms '((0 1 2 3 4) (3 0 4 1 2) (0 1 4 2 3) (3 1 4 2 0)))
; (setq perms '((0 1 2 3) (3 0 1 2) (0 1 2 3) (3 1 2 0)))

(defun all-permutations (list)
  (cond ((null list) nil)
        ((null (cdr list)) (list list))
        (t (loop for element in list
             append (mapcar (lambda (l) (cons element l))
                            (all-permutations (remove element list)))))))


(defun range (end)
        (loop for i from 0 below end collect i))

(setq permuts (all-permutations (range domainLength)))

(setf comb nil)

(dotimes (n domainNumb)
  (if (equalp n 0) (setf comb (list (list (range domainLength))))
      (progn 
        (setf new nil)
        (dolist (single comb)
          (dolist (perm permuts)
          (progn 
            (setf temp (append single (list perm)))
            (setf new (append (list temp) new)))))
        (setf comb new))))


(setf counter 0)
(setf true nil)
(defparameter og (nth 5 comb))

(dolist (combination comb)
  (progn 
    (setf flag nil)
    (if (compiled-function-p diffcase) (if (not (funcall diffcase combination)) (setf flag t)))
       (dolist (rule totstrings)
         (progn
          (if (equalp (mod counter 100000) 0) (print "+"))
          (setf counter (+ 1 counter))
          (if (not flag) 
              (progn
                (defparameter og combination)
                (print rule)
                (print (funcall (toConstraint rule) combination))
                (if (not (funcall (toConstraint rule) combination)) (setf flag t))))))
    (if (not flag)
        (setf true combination))))

(print true)

(defun eve(func)
  (eval (read-from-string func)))

