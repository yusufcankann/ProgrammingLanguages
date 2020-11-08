;;;; Yusuf Can Kan - 161044007

(defun read_file ()
	; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
(with-open-file (stream "./test.g++" :if-does-not-exist :create)
  (reverse (cdr (reverse (read_recursively stream)))))) ;;Reads every line and every character with read_recursively function.

;; Reads every line and every character recursively and returns as a list.
(defun read_recursively (file)
 (let ((line (read-line file nil nil)))
  (if (not (eql line nil))
        (progn ;Appends new line every end of the operation.
            (setf line (concatenate 'string line (string #\NewLine)))
            (append (append (convert_to_string_list (coerce line 'list)) (read_recursively file)))))))

; Converts given character string into a list.
(defun convert_to_string_list (list)
  (if (not (equal list nil))
    (cond 
      ;; Sets space with "SPACE" string.
      ((equal (car list) #\Space ) (cons "SPACE" (convert_to_string_list (cdr list))))
      ((equal (car list) #\Tab ) (cons "TAB" (convert_to_string_list (cdr list))))
      ((equal (car list) #\NewLine) (cons "NewLine" (convert_to_string_list (cdr list))))
      ;converts first character to string and recursively calls rest of the list.
      (t (progn (cons (string (car list)) (convert_to_string_list (cdr list))))))))

;; It returns the token of given string if it is operator, digit or ideftifier..
(defun is_op(list)
    (cond
        ((string= (car list) "+")  "OP_PLUS")
        ((string= (car list) "**")  "OP_PLUS")
        ((string= (car list) "-")  "OP_MINUS")
        ((string= (car list) "/")  "OP_DIV")
        ((string= (car list) "*")  "OP_MULT")
        ((string= (car list) "(")  "OP_OP")
        ((string= (car list) ")")  "OP_CP")
        ((string= (car list) "\"") "OP_OC")
        ((string= (car list) ",")  "OP_COMMA")
        ((string= (car list) ";")  "SEMICOLON")
        ((string= (car list) ";;")  "COMMENT")
        (t nil)))

;; This function takes a list and control if its third element is whitespace.
(defun third_whitespace_control (my_list)
  (or (string= (caddr my_list) "SPACE")) 
    (string= (caddr my_list) "NewLine") (string= (caddr my_list) "TAB"))

;; This function just splits every word, digit, operator.
;; It puts every variation in a different element of a list.
(defun split_input(input_list output_list str)
  ;; If first element of input_list is operation this function works. Otherwise it assignes nil to op.
  (setq op (is_op input_list))

  (cond
    ;; Base case for recursive.
    ((equal input_list nil) (if (not (string= str "")) (split_input nil (cons str output_list) "") (reverse output_list)))
    
    ;; 
    ((and (not (string= str ""))(or (string= (car input_list) "NewLine") (string= (car input_list) "SPACE")
              (string= (car input_list) "TAB"))) (split_input (cdr input_list) (cons str output_list) ""))

    ((and (string= str "")(or (string= (car input_list) "NewLine") (string= (car input_list) "SPACE")
              (string= (car input_list) "TAB"))) (split_input (cdr input_list) output_list ""))
               
    ;; ** case
    ((and (string= op "OP_MULT") (string= str "") (string= (is_op (cdr input_list)) "OP_MULT") (third_whitespace_control input_list))
        (split_input (cdddr input_list) (cons "**" output_list) ""))

    ;; comment line case.
    ((and (string= op "SEMICOLON") (string= str "") (string= (is_op (cdr input_list)) "SEMICOLON") (third_whitespace_control input_list))
        (split_input (cdddr input_list) (cons ";;" output_list) ""))
    
    ;; " sign case
    ((and (string= op "OP_OC") (string= str ""))
      (split_input (cdr input_list) (cons "\"" output_list) ""))
    
    ;; " sign case
    ((and (string= op "OP_OC") (not (string= str "")))
      (split_input (cdr input_list) (cons str output_list) "\""))
    
    ;; Open paranthesis case
    ((and (string= op "OP_OP") (string= str ""))
      (split_input (cdr input_list) (cons "(" output_list) ""))

    ;; Close paranthesis case
    ((and (string= op "OP_CP") (not (string= str "")))
      (split_input (cdr input_list) (cons str output_list) ")"))

    (t (split_input (cdr input_list) output_list (concatenate 'string str (car input_list))))
))





(setf a (read_file))
 (setf b nil)


    (setq c "")


(print (split_input a b c))









































;; ; Returns if first element of given list is digit or letter.
;; (defun digit_or_integer (list)
;;   ((and (string<= str "9") (string>= str "0")) "D") ;Digit
;;   ((or (and (string<= str "Z")(string>= str "A"))
;;     (and (string<= str "z")(string>= str "a"))) "L") ;Letter
;;   nil)


;; ;(wrapper)

;; ; Digit 0
;; ; Identifier 1
;; (defun identifier_dfa(my_list)
;;   (cond
;;     ((or (string= (car my_list) "NewLine") (string= (car my_list) "SPACE") 
;;         (and (string= (car my_list) "\"") (= quote_flag 1)) (string= (car my_list) ")") (string= (car my_list) "(")) t)
;;     ((string= (digit_or_integer my_list) "L") (identifier_or_digit (cdr my_list) i))
;;     (t nil)))





;; (defun digit_dfa_w (my_list)
;;   (if (string= (car my_list) "0") "0" (digit_dfa my_list)))

;; (defun digit_dfa (my_list)
;;   (cond 
;;     ((string= (digit_or_integer my_list) "D") ) 
;;         (if (= i 0 nil) (identifier_or_digit (car my_list) i))))



;; ;;;; KEYWORD CONTROLS.
;; (defun keyword_control (my_list length)
;;   (if (<= length 1) nil) ;; For one letter
;;   (if (>= length 2) ;For length 2 keywords. (if-or)
;;     (if (= length 2)
;;       (cond 
;;         ((equal (subseq my_list 0 2) (list "i" "f")) (return-from keyword_control "KW_IF"))
;;         ((equal (subseq my_list 0 2) (list "o" "r")) (return-from keyword_control "KW_OR")))
;;       (cond
;;          ((or (equal (subseq my_list 0 3) (list "i" "f"  "NewLine"))
;;             (equal (subseq my_list 0 3) (list "i" "f" "SPACE"))) (return-from keyword_control "KW_IF"))
;;          ((or (equal (subseq my_list 0 3) (list "o" "r"  "NewLine"))
;;             (equal (subseq my_list 0 3) (list "o" "r" "SPACE"))) (return-from keyword_control "KW_OR")))))

;;   (if (>= length 3) ;For length 3 keywords (and-not-set-for-nil)
;;     (if (= length 3)
;;       (cond 
;;         ((equal (subseq my_list 0 3) (list "a" "n" "d")) (return-from keyword_control "KW_AND"))
;;         ((equal (subseq my_list 0 3) (list "n" "o" "t")) (return-from keyword_control "KW_NOT"))
;;         ((equal (subseq my_list 0 3) (list "s" "e" "t")) (return-from keyword_control "KW_SET"))
;;         ((equal (subseq my_list 0 3) (list "f" "o" "r")) (return-from keyword_control "KW_FOR"))
;;         ((equal (subseq my_list 0 3) (list "n" "i" "l")) (return-from keyword_control "KW_NIL")))
;;       (cond
;;         ((or (equal (subseq my_list 0 4) (list "a" "n" "d" "NewLine"))
;;           (equal (subseq my_list 0 4) (list "a" "n" "d" "SPACE"))) (return-from keyword_control "KW_AND"))
;;         ((or (equal (subseq my_list 0 4) (list "n" "o" "t" "NewLine"))
;;           (equal (subseq my_list 0 4) (list "n" "o" "t" "SPACE"))) (return-from keyword_control "KW_NOT"))  
;;         ((or (equal (subseq my_list 0 4) (list "s" "e" "t" "NewLine"))
;;           (equal (subseq my_list 0 4) (list "s" "e" "t" "SPACE"))) (return-from keyword_control "SET"))  
;;         ((or (equal (subseq my_list 0 4) (list "f" "o" "r" "NewLine"))
;;           (equal (subseq my_list 0 4) (list "f" "o" "r" "SPACE"))) (return-from keyword_control "KW_FOR"))  
;;         ((or (equal (subseq my_list 0 4) (list "n" "i" "l" "NewLine"))
;;           (equal (subseq my_list 0 4) (list "n" "i" "l" "SPACE"))) (return-from keyword_control "KW_NIL")))))

;;   (if (>= length 4) ;For length 4 keywords (less-list-exit-load-disp-true)
;;     (if (= length 4)
;;       (cond
;;         ((equal (subseq my_list 0 4) (list "l" "e" "s" "s")) (return-from keyword_control "KW_LESS"))
;;         ((equal (subseq my_list 0 4) (list "l" "i" "s" "t")) (return-from keyword_control "KW_LIST"))
;;         ((equal (subseq my_list 0 4) (list "e" "x" "i" "t")) (return-from keyword_control "KW_EXIT"))
;;         ((equal (subseq my_list 0 4) (list "l" "o" "a" "d")) (return-from keyword_control "KW_LOAD"))
;;         ((equal (subseq my_list 0 4) (list "d" "i" "s" "p")) (return-from keyword_control "KW_DISP"))
;;         ((equal (subseq my_list 0 4) (list "t" "r" "u" "e")) (return-from keyword_control "KW_TRUE")))
;;       (cond
;;         ((or (equal (subseq my_list 0 5) (list "l" "e" "s" "s" "NewLine"))
;;           (equal (subseq my_list 0 5) (list "l" "e" "s" "s" "SPACE"))) (return-from keyword_control "KW_LESS"))
;;         ((or (equal (subseq my_list 0 5) (list "l" "i" "s" "t" "NewLine"))
;;             (equal (subseq my_list 0 5) (list "l" "i" "s" "t" "SPACE"))) (return-from keyword_control "KW_LIST"))
;;         ((or (equal (subseq my_list 0 5) (list "e" "x" "i" "t"  "NewLine"))
;;             (equal (subseq my_list 0 5) (list "e" "x" "i" "t" "SPACE"))) (return-from keyword_control "KW_EXIT"))
;;         ((or (equal (subseq my_list 0 5) (list "l" "o" "a" "d"  "NewLine"))
;;             (equal (subseq my_list 0 5) (list "l" "o" "a" "d" "SPACE"))) (return-from keyword_control "KW_LOAD"))
;;         ((or (equal (subseq my_list 0 5) (list "d" "i" "s" "p"  "NewLine"))
;;             (equal (subseq my_list 0 5) (list "d" "i" "s" "p" "SPACE"))) (return-from keyword_control "KW_DISP"))
;;         ((or (equal (subseq my_list 0 5) (list "t" "r" "u" "e"  "NewLine"))
;;             (equal (subseq my_list 0 5) (list "t" "r" "u" "e" "SPACE"))) (return-from keyword_control "KW_TRUE")))))
    
;;     (if (>= length 5) ;For length 5 keywords (false-equal)
;;       (if (= length 5)
;;         (cond
;;           ((equal (subseq my_list 0 5) (list "f" "a" "l" "s" "e")) (return-from keyword_control "KW_FALSE"))
;;           ((equal (subseq my_list 0 5) (list "e" "q" "u" "a" "l")) (return-from keyword_control "KW_EQUAL")))
;;         (cond
;;           ((or (equal (subseq my_list 0 6) (list "f" "a" "l" "s" "e" "NewLine"))
;;             (equal (subseq my_list 0 6) (list "f" "a" "l" "s" "e" "SPACE"))) (return-from keyword_control "KW_FALSE"))
;;           ((or (equal (subseq my_list 0 6) (list "e" "q" "u" "a" "l" "NewLine"))
;;             (equal (subseq my_list 0 6) (list "e" "q" "u" "a" "l" "SPACE"))) (return-from keyword_control "KW_EQUAL")))))

;;     (if (>= length 6) ;For length 6 keywords (append-deffun-concat)
;;       (if (= length 6) 
;;         (cond
;;           ((equal (subseq my_list 0 6) (list "a" "p" "p" "e" "n" "d")) (return-from keyword_control "KW_APPEND"))
;;           ((equal (subseq my_list 0 6) (list "d" "e" "f" "f" "u" "n")) (return-from keyword_control "KW_DEFFUN"))
;;           ((equal (subseq my_list 0 6) (list "c" "o" "n" "c" "a" "t")) (return-from keyword_control "KW_CONCAT")))
;;         (cond
;;           ((or (equal (subseq my_list 0 7) (list "a" "p" "p" "e" "n" "d" "NewLine"))
;;               (equal (subseq my_list 0 7) (list "a" "p" "p" "e" "n" "d" "SPACE"))) (return-from keyword_control "KW_APPEND"))
;;           ((or (equal (subseq my_list 0 7) (list "c" "o" "n" "c" "a" "t" "NewLine"))
;;               (equal (subseq my_list 0 7) (list "c" "o" "n" "c" "a" "t" "SPACE"))) (return-from keyword_control "KW_CONCAT"))
;;           ((or (equal (subseq my_list 0 7) (list "d" "e" "f" "f" "u" "n" "NewLine"))
;;               (equal (subseq my_list 0 7) (list "d" "e" "f" "f" "u" "n" "SPACE"))) (return-from keyword_control "KW_DEFFUN")))))
;;     nil)




















































;; (print (keyword_control (list "n" "o" "t") 3))
;; (print (keyword_control (list "o" "r" "NewLine") 3))
;; (print (keyword_control (list "i" "f" "SPACE") 3))
;; (print (keyword_control (list "o" "r") 2))
;; (print (keyword_control (list "n" "o" "t") 3))
;; (print (keyword_control (list "n" "o" "t") 3))
;; (print (keyword_control (list "n" "i" "l") 3))
;; (print (keyword_control (list "n" "i" "l" "SPACE") 4))
;; (print (keyword_control (list "f" "o" "r" "SPACE") 4))
;; (print (keyword_control (list "l" "i" "s" "t") 4))
;; (print (keyword_control (list "l" "i" "s" "t" "NewLine") 5))
;; (print (keyword_control (list "t" "r" "u" "e" "SPACE") 5))
;; (print (keyword_control (list "l" "e" "s" "s" "SPACE") 5))
;; (print (keyword_control (list "d" "i" "s" "p") 4))
;; (print (keyword_control (list "e" "x" "i" "t" "SPACE") 5))
;; (print (keyword_control (list "e" "x" "i" "l" "NewLine") 5))
;; (print (keyword_control (list "f" "a" "l" "s" "e" "SPACE") 6))
;; (print (keyword_control (list "f" "a" "l" "s" "e" "NewLine") 6))
;; (print (keyword_control (list "f" "a" "l" "s" "e") 5))
;; (print (keyword_control (list "e" "q" "u" "a" "l" "NewLine") 6))
;; (print (keyword_control (list "e" "q" "u" "a" "l" "SPACE") 6))
;; (print (keyword_control (list "e" "q" "u" "a" "l") 5))

;; (print (keyword_control (list "a" "p" "p" "e" "n" "d" "SPACE") 7))
;; (print (keyword_control (list "a" "p" "p" "e" "n" "d" "NewLine") 7))
;; (print (keyword_control (list "a" "p" "p" "e" "n" "d") 6))



;; (print (keyword_control (list "c" "o" "n" "c" "a" "t" "SPACE") 7))
;; (print (keyword_control (list "c" "o" "n" "c" "a" "t" "NewLine") 7))
;; (print (keyword_control (list "c" "o" "n" "c" "a" "t") 6))



;; (print (keyword_control (list "d" "e" "f" "f" "u" "n" "SPACE") 7))
;; (print (keyword_control (list "d" "e" "f" "f" "u" "n" "NewLine") 7))
;; (print (keyword_control (list "d" "e" "f" "f" "u" "n") 6))



;; (print (keyword_control (list "n" "o" "t" "NewLine") 4))


;; Checks if given list contains "a" "n" "d" characters and no character more.
;; (defun is_and (my_list)
;;     (or (equal (subseq my_list 0 4) (list "a" "n" "d" "NewLine"))
;;         (equal (subseq my_list 0 4) (list "a" "n" "d" "SPACE")))
;; )

;; (defun is_or (my_list)
;;     (or (equal (subseq my_list 0 3) (list "o" "r" "NewLine"))
;;         (equal (subseq my_list 0 3) (list "o" "r" "SPACE")))
;; )

;; (defun is_not (my_list)
;;     (or (equal (subseq my_list 0 4) (list "n" "o" "t" "NewLine"))
;;         (equal (subseq my_list 0 4) (list "n" "o" "t" "SPACE")))
;; )

;; (defun is_equal (my_list)
;;     (or (equal (subseq my_list 0 6) (list "e" "q" "u" "a" "l" "NewLine"))
;;         (equal (subseq my_list 0 6) (list "e" "q" "u" "a" "l" "SPACE")))
;; )

;; (defun is_less (my_list)
;;     (or (equal (subseq my_list 0 5) (list "l" "e" "s" "s" "NewLine"))
;;         (equal (subseq my_list 0 5) (list "l" "e" "s" "s" "SPACE")))
;; )

;; (defun is_nil (my_list)
;;     (or (equal (subseq my_list 0 4) (list "n" "i" "l" "NewLine"))
;;         (equal (subseq my_list 0 4) (list "n" "i" "l" "SPACE")))
;; )

;; (defun is_list (my_list)
;;     (or (equal (subseq my_list 0 5) (list "l" "i" "s" "t" "NewLine"))
;;         (equal (subseq my_list 0 5) (list "l" "i" "s" "t" "SPACE")))
;; )

;; (defun is_append (my_list)
;;     (or (equal (subseq my_list 0 7) (list "a" "p" "p" "e" "n" "d" "NewLine"))
;;         (equal (subseq my_list 0 7) (list "a" "p" "p" "e" "n" "d" "SPACE")))
;; )

;; (defun is_concat (my_list)
;;     (or (equal (subseq my_list 0 7) (list "c" "o" "n" "c" "a" "t" "NewLine"))
;;         (equal (subseq my_list 0 7) (list "c" "o" "n" "c" "a" "t" "SPACE")))
;; )

;; (defun is_set (my_list)
;;     (or (equal (subseq my_list 0 4) (list "s" "e" "t" "NewLine"))
;;         (equal (subseq my_list 0 4) (list "s" "e" "t" "SPACE")))
;; )

;; (defun is_deffun (my_list)
;;     (or (equal (subseq my_list 0 7) (list "d" "e" "f" "f" "u" "n" "NewLine"))
;;         (equal (subseq my_list 0 7) (list "d" "e" "f" "f" "u" "n" "SPACE")))
;; )

;; (defun is_for (my_list)
;;     (or (equal (subseq my_list 0 4) (list "f" "o" "r" "NewLine"))
;;         (equal (subseq my_list 0 4) (list "f" "o" "r" "SPACE")))
;; )

;; (defun is_if (my_list)
;;     (if (and (= (list-length my_list) 2) (equal (subseq my_list 0 2) (list "i" "f"))) t
;;     (or (equal (subseq my_list 0 3) (list "i" "f" "NewLine"))
;;         (equal (subseq my_list 0 3) (list "i" "f" "SPACE"))))
;; )


;; (print (is_if (list "i" "f")))

;; (defun is_exit (my_list)
;;     (or (equal (subseq my_list 0 5) (list "e" "x" "i" "t"  "NewLine"))
;;         (equal (subseq my_list 0 5) (list "e" "x" "i" "t" "SPACE")))
;; )

;; (defun is_load (my_list)
;;     (or (equal (subseq my_list 0 5) (list "l" "o" "a" "d"  "NewLine"))
;;         (equal (subseq my_list 0 5) (list "l" "o" "a" "d" "SPACE")))
;; )

;; (defun is_disp (my_list)
;;     (or (equal (subseq my_list 0 5) (list "d" "i" "s" "p"  "NewLine"))
;;         (equal (subseq my_list 0 5) (list "d" "i" "s" "p" "SPACE")))
;; )

;; (defun is_true (my_list)
;;     (or (equal (subseq my_list 0 5) (list "t" "r" "u" "e"  "NewLine"))
;;         (equal (subseq my_list 0 5) (list "t" "r" "u" "e" "SPACE")))
;; )

;; (defun is_false (my_list)
;;     (or (equal (subseq my_list 0 6) (list "f" "a" "l" "s" "e" "NewLine"))
;;         (equal (subseq my_list 0 6) (list "f" "a" "l" "s" "e" "SPACE")))
;; )



;(print (read_file))
;(read_file)
