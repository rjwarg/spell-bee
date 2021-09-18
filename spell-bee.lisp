;; file: spell-bee.lisp
;; auth: dick warg
;; date: 10SEP2021
;; func: find all words that contain the anagram of the day


;; define globals
(defvar *must-letter* nil)
(defvar *all-seven* nil)
(defvar *testword* nil)


;; open the dictionary file
(defun read-test () (open "test.txt"))


;; read and print the first word in the dictionary
(defun list-words-old ()
  (let ((in (open "test.txt")))
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
  (let ((in (open "words.txt")))
    (setq *testword* nil)
    (loop  while (not (string= *testword* "ZZZ")) 
	  do 
	     (setq *testword* (string-downcase (read-line in nil)))
	     (if
	      (and
	       (find *must-letter* *testword*)
	       (string= "" (string-trim *all-seven* *testword*))) (format t "~a  " *testword*)))
    (close in)
    (return-from nil nil)       ))


