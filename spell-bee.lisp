;; file: spell-bee.lisp
;; auth: dick warg
;; date: 10SEP2021
;; func: find all words that contain the anagram of the day


;; define globals
(defvar *must-letter* nil)
(defvar *all-seven* nil)
(defvar *testword* nil)
(defvar *file-name* "words")

;;
;; list word file testtext. with ZZZ at end
;;

(defun list-words-v2 (&optional filename)
  (if filename () (setq filename "MIT-words"))
  (with-open-file (stream filename)
    (format t "Searching ~a ~%" filename) 
    (loop for line = (read-line stream nil 'foo)
	until (eq line 'foo)
	do
	   (setq *testword* (string-downcase line));
	 (if
	      (and
               (> (length *testword*) 3)
	       (find (char *must-letter* 0) *testword*)
	       (string= "" (string-trim *all-seven* *testword*)))
            (format t "<~a> " *testword*)))
    ))


;;
;; list word file testtext with ZZZ at end
;;

(defun list-pangram (&optional filename)
  (if filename () (setq filename "words"))
  (with-open-file (stream filename)
    (format t "Searching ~a ~%" filename) 
    (loop for line = (read-line stream nil 'foo)
	until (eq line 'foo)
	do
	   (setq *testword* (string-downcase line));
	 (if
	      (and
               (> (length *testword*) 6)
	       (find (char *must-letter* 0) *testword*)
	       (string= "" (string-trim *all-seven* *testword*)))
            (format t "<~a> " *testword*)))
       ))

;;
;; prompt for test values
;;
(defun prompt-read (prompt)
	   (format *query-io* "~a :" prompt)
	   (force-output *query-io*)
	   (read-line *query-io*))

(defun get-letters ()
	   (setq *must-letter* (prompt-read "Center Letter"))
	   (setq *all-seven* (prompt-read "Outer Six Letters"))
  (setq *all-seven* (string-downcase (concatenate 'string *must-letter* *all-seven*))))


;; open the dictionary file
(defun read-test () (open "test"))


;; read and print the first word in the dictionary
(defun list-words-old (test-file)
  (let ((in (open test-file)))
    (loop repeat 10
	  do (format t "~a~%" (read-line in)))
    (close in)))

;; load the devel test values
(defun load-test-vals (all-seven)
  (setq *must-letter* (char all-seven 0))
  (setq *all-seven* all-seven))

;; accept the search values
;; the central letter
;; the other 6 letters is a list

;; skip thru dictionary until word contains the central letter and at least 4 total letters

;; test the word for any of the other 6 letters. If a letter is found that is not the 6 word list
;; then skip to next word

(defun list-words ()
  (let ((in (open "words")))
    (setq *testword* nil)
    (loop  while (not (string= *testword* "ZZZ")) 
	  do 
	     (setq *testword* (string-downcase (read-line in nil)))
	     (if
	      (and
	       (find *must-letter* *testword*)
	       (string= "" (string-trim *all-seven* *testword*))) (format t "~a  " *testword*)))
    (close in)))

;;
;; complete list
;;
(defun main ()
  (get-letters)
  (list-words-v2 "MIT-words")
  (terpri)
  (list-words-v2 "words"))

;; run the main

(main)




