;; jikan.lisp
;; by Nick Baker <njb@robotjunkyard.org>
;;
;; Text console quiz program for practicing the spelling-out
;; of times of the day in Japanese.  This is not comprehensive
;; by any means; I emphasized the part of this topic that I
;; personally had difficulties improving at:  articulating
;; the numerical time into the correct words and spellings.
;;
;; Feel free to improve upon this quiz for AM/PM, days of the
;; week, months, years, words-to-numbers, etc.
;;
;; To run, type at the REPL:
;;   > (load "jikan.lisp")
;;   > (quiz-time!)
;;
;; Answers must be in pure hiragana only (kanji or romaji
;; answers, even if technically correct, will fail).  And,
;; of course, no spaces.
;;
;; Caveat:
;; I'm still very much a newbie to Japanese, so it's possible
;; this may reject valid answers or expect invalid answers for
;; some cases.  Tested this as best I could against a heap of
;; question-and-answer pairs in an accompanying textbook+workbook
;; for any inconsistencies and it seemed to be OK, though.
;;
;; Licensed under GPL3.
;;
;; -------------------------------------------------------------

(defparameter *standard-numbers*
  #("いち"
    "に"
    "さん"
    "よん"
    "ご")
  "This program only ever uses this variable for appending to 'jyuu' for minutes, so only ichi thru go need to be defined.")

(defparameter *faces*
  #("(ﾉ◕ヮ◕)ﾉ*:･ﾟ✧"
    "(°⌣°)"
    "(●´ω｀●)")
  "Array of random happy-faces printed when user answers correctly.")

(defun hour-to-nihongo (hour)
  (declare (type (integer 1 24)))
  (aref #("いちじ"
	  "にじ"
	  "さんじ"
	  "よじ"
	  "ごじ"
	  "ろくじ"
	  "しちじ"
	  "はちじ"
	  "くじ"
	  "じゅうじ"
	  "じゅういちじ"
	  "じゅうにじ")
	(mod (1- hour) 12)))

(defun single-time-digit-to-nihongo (digit)
  (declare (type (integer 0 10) digit))
  (if (= 0 digit)
      ""
      (aref #("いっぷん"
	      "にふん"
	      "さんぷん"
	      "よんぷん"
	      "ごふん"
	      "ろっぷん"
	      "ななふん"
	      "はっぷん"
	      "きゅうふん")
	    (1- digit))))

(defun minute-to-nihongo (minute)
  (declare (type (integer 0 59) minute))
  (if (zerop minute)
      ""
      (multiple-value-bind
	    (greater-digit lesser-digit)
	  (truncate minute 10)
	(cond ((zerop greater-digit)
	       (single-time-digit-to-nihongo minute))
	      ((= 30 minute)
	       "はん")
	      ((zerop (mod minute 10))  ;; is it 10/20/30/40/50?
	       (format nil "~aじっぷん"
		       (if (= 1 greater-digit)
			   ""
			   (aref *standard-numbers*
				 (1- greater-digit)))))
	      (T
	       (format nil "~aじゅう~a"
		       (if (= 1 greater-digit)
			   ""
			   (aref *standard-numbers*
				 (1- greater-digit)))
		       (single-time-digit-to-nihongo
			lesser-digit)))))))

(defun time-to-nihongo (hour minute)
  (declare (type (integer 1 24) hour)
	   (type (integer 0 59) minute))
  (format nil "~a~a"
	  (hour-to-nihongo hour)
	  (minute-to-nihongo minute)))

(defun quiz-time! (&optional (number-of-questions 10))
  (declare (type (integer 1 *) number-of-questions))
  (format t "*************************~%")
  (format t "* ! Q U I Z - T I M E ! *~%")
  (format t "*************************~%~%")
  (loop
     for  i below number-of-questions
     with answered-correctly = 0
     do
       (let* ((hour (1+ (random 12)))  ;; 1..12
	      (minute (random 60))     ;; 0..59
	      (time-as-text (time-to-nihongo hour minute)))
	 (format t "~d) What is [~d:~2,'0d]?~%"
		 (1+ i) hour minute)
	 (let ((answer (string-trim '(#\Space)
				    (read-line))))
	     (if (string= answer time-as-text)
		 (progn
		   (incf answered-correctly)
		   (format t "いいです ~a！~%"
			   (aref *faces*
				 (random (length *faces*)))))
		 (format t "~a ... だめ！~%" time-as-text))))
     finally
       (format t "...~%You answered ~a correctly.~&"
	       answered-correctly)))

(format t "~%--------------------------------~%")
(format t "こんにちは！　jikan.lispがよみこみました。~%~%")
(format t "Run (quiz-time!) to quiz yourself 10 questions.~%")
(format t "(quiz-time! also accepts an optional number~%")
(format t " parameter if you want a different amount than 10.~%")
(format t "--------------------------------~%~%")
