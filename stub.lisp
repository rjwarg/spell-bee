;;file: stub.lisp
;;auth: dick warg
;;date: 17nov2022
;;func: for developing code fragments

(ql:quickload "cl-ppcre")

;; globals
(defvar *yellow* "")
(defvar *gray*  "")
(defvar *green* "")

;; test n step loop
(defun loop-test (x)
  (format t "Number is: ~d~%" x)
  
  (loop while (> x 0)
	      do (format t "num is ~d ~%" x)
	      (setf x (- x 1)))

(format t "Number is: ~d~%" x))

;; reset the test strings
(defun reset-letters ()
  (setf *yellow* "")
  (setf *gray* "")
  (setf *green* ".....")
  )

;; test concatenation
(defun cat (letter)
  (format t "YELLOW was: ~s~%" *yellow*)
  (setq *yellow* (concatenate 'string *yellow* letter))
  (format t "YELLOW is now: ~s~%" *yellow*))


;; the word must have exactly 5 letters
(defun five-letterp (this-word)
  (if (= (length this-word) 5)
;;      (format t "OK: ~s~%" this-word)))
      this-word nil))

;; true if all characters in global string A also exist in string B?
(defun in-stringp (A B)
  ;;  (format t "Test: ~s Input: ~s~%" A B)
  ;; if the test string is empty return nil
  (if (= 0 (length A) ) (return-from in-stringp NIL))
  (do
   ((x 0 (+ x 1)))
   ((>= x (length A)  ) B) ;; all chars found
  ;; (format t "~c ~d~%" (char A x) x)
   (if (not (find (char A x) B))
       (progn
;;	(format t "HIT ~d ~c~%" x (char A x))
        (RETURN-from in-stringp NIL)
       )
    )
   )
  )

;; true if any of the chars in A are also in B
(defun notin-stringp (A B)
    ;; if the test string is empty return string B
  (if (= 0 (length A))  (return-from notin-stringp B))
  (do
   ((x 0 (+ x 1)))
   ((>= x (length A)  ) B) ;; all chars found
  ;; (format t "~c ~d~%" (char A x) x)
   (if  (find (char A x)  B)
       (progn
;;	(format t "HIT ~d ~c~%" x (char A x))
        (RETURN-from notin-stringp NIL)
       )
    )
   )
  )

;; test the current word (CURRENT-WORD) and test strings (YELLOW GRAY)
(defun valid-wordp ( CURRENT-WORD YELLOW GRAY GREEN )
  (IF
   (AND
    (five-letterp CURRENT-WORD)
    (in-stringp YELLOW CURRENT-WORD)
    (notin-stringp GRAY CURRENT-WORD)
    (good-greenp GREEN CURRENT-WORD)
    )
   CURRENT-WORD
   NIL
   )
  )
		

;;
;; prompt for test values
;;
(defun prompt-read (prompt)
	   (format *query-io* "~a :" prompt)
	   (force-output *query-io*)
	   (read-line *query-io*))

(defun get-letters ()
	   (setq *yellow* (prompt-read "Yellow Letters"))
           (setq *gray*  (concatenate 'string *gray* (prompt-read "Gray Letters")))
           (setq *green* (prompt-read "green pattern"))
  )


;; loop through the file and test
(defun list-words (&optional filename)
  (if filename () (setq filename "MIT-words"))
  (setq ix 1)
  (with-open-file (stream filename)
    (format t "Searching ~a ~a ~a~%" filename *yellow* *gray*) 
    (loop for line = (read-line stream nil 'foo)
	until (eq line 'foo)
	do
	   (setq *testword* (string-downcase line))
           (if (valid-wordp *testword* *yellow* *gray* *green*)
             (progn
	       (format t "<~a> " *testword* )
	       (setf ix (+ ix 1))
	       (if (>= ix 6)
		   (let ()
		       (terpri)
		      (setq ix 0)
		   )
		   )
	     )
	   )
    )
    )
  )


;; use regular expr to get the green letter matches
(defun good-greenp (A B)
 (if (= 0 (length A) ) (return-from good-greenp NIL))
 (if (ppcre:scan A B) B NIL)
  ;;
)
;; (setf a (let ((ptrn (ppcre:create-scanner "..a.c")))
;;  (ppcre:scan ptrn "xaaab")))
