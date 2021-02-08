;;;; Yusuf Can Kan - 161044007


;IN HW2 LEXERS WRITING IN A FILE. IN THIS HOMEWORK WE NEED TO USE 
; TOKENS IN OUR PARSER. SO LEXER STORES TOKENS IN LIST.



(setq quote 0) ;; For quote sign. If this variable is 0, then it is open quote
               ;; If it is 1, it is close
(setf tokensForParser nil) ;stores tokens for parser
(setf inputsForParser nil) ;stores every expression for parser

(setq errorFlag 0)

; ;; In HW2 this part was writing tokens+ to file. Now instead that it will
; ;store is in a variable.
(defun write_file (x &optional (flag -1))
  ;;; WRITING PART
  (if (equal errorFlag 0)
    (if (equal flag 1)
      (progn 
        (setf tokensForParser nil)
        (setq error_output (format nil "SYNTAX_ERROR ~a cannot be tokenized!!" x))
       ; (setf tokensForParser (append (list error_outputs) tokensForParser))   
        (setq errorFlag 1))
        (setf tokensForParser (append (list x) tokensForParser))))
)


;; This function clears inside the "collatz_outputs.txt" file if it exis.
;; If file does not exits it creates empty file.
(defun clear_file ()
  (with-open-file (write_stream "./parsed_lisp.txt"
          :direction :output
          :if-exists :supersede
          :if-does-not-exist :create)
    (close write_stream)) ;Close the file
)

(defun read_file (my_file)
	; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
(with-open-file (stream my_file :if-does-not-exist :create)
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
    
    ;; Whitespace Case 
    ((and (not (string= str ""))(or (string= (car input_list) "NewLine") (string= (car input_list) "SPACE")
              (string= (car input_list) "TAB")))
        (if (string= (car input_list) "NewLine") (split_input (cdr input_list) (cons "NewLine" (cons str output_list)) "")
          (split_input (cdr input_list) (cons str output_list) "")))

    ;; Whitespace Case
    ((and (string= str "")(or (string= (car input_list) "NewLine") (string= (car input_list) "SPACE")
              (string= (car input_list) "TAB")))
        (if (string= (car input_list) "NewLine") (split_input (cdr input_list) (cons "NewLine" output_list) "")    
               (split_input (cdr input_list) output_list "")))
               
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

; Returns if first element of given list is digit or letter.
(defun digit_or_integer (str)
  (cond 
  ((and (string<= str "9") (string>= str "0")) "D") ;Digit
  ((or (and (string<= str "Z")(string>= str "A"))
    (and (string<= str "z")(string>= str "a"))) "L") ;Letter
  (t nil)))


;; is_ideftfier wrapper function.
;; It controls if first element is letter or not.
(defun is_identifier_w (my_string)
  (cond 
    ((not (string= (digit_or_integer (subseq my_string 0 1)) "L")) nil)
    (t (is_identifier (subseq my_string 1)))))

;; This function checks if given string is identifier.
(defun is_identifier (my_string)
  (cond
    ((string= my_string "") t)
    ((not (equal (digit_or_integer (subseq my_string 0 1)) nil))
      (is_identifier (subseq my_string 1)))))


(defun is_digit (my_string)
  (cond
    ((string= my_string "") t)
    ((not (string= (digit_or_integer (subseq my_string 0 1)) "D")) nil)
    (t (is_digit (subseq my_string 1)))))


(defun is_digit_w (my_string)
  (cond
    ((not (string= (digit_or_integer (subseq my_string 0 1)) "D")) nil)
    (t (is_digit my_string))))


;; It returns the token of given string if it is operator, digit or ideftifier..
(defun is_op(list)
    (cond
        ((string= (car list) "+")  "OP_PLUS")
        ((string= (car list) "**")  "OP_DBLMULT")
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

;; It returns the token of given string if it is operator, digit or ideftifier..
(defun is_keyword(my_list)
    (cond
        ((string= (car my_list) "and")  "KW_AND")
        ((string= (car my_list) "or")  "KW_OR")
        ((string= (car my_list) "not")  "KW_NOT")
        ((string= (car my_list) "equal")  "KW_EQUAL")
        ((string= (car my_list) "less")  "KW_LESS")
        ((string= (car my_list) "nil")  "KW_NIL")
        ((string= (car my_list) "list")  "KW_LIST")
        ((string= (car my_list) "append") "KW_APPEND")
        ((string= (car my_list) "concat")  "KW_CONCAT")
        ((string= (car my_list) "set")  "KW_SET")
        ((string= (car my_list) "defvar")  "KW_DEFVAR")
        ((string= (car my_list) "deffun")  "KW_DEFFUN")
        ((string= (car my_list) "for")  "KW_FOR")
        ((string= (car my_list) "if")  "KW_IF")
        ((string= (car my_list) "exit")  "KW_EXIT")
        ((string= (car my_list) "load")  "KW_LOAD")
        ((string= (car my_list) "disp")  "KW_DISP")
        ((string= (car my_list) "true")  "KW_TRUE")
        ((string= (car my_list) "false")  "KW_FALSE")
        (t nil)))

(defun dfa (input)
  (setq op (is_op input))
  (setq kywrd (is_keyword input))

  (if (not (string= (car input) "NewLine"))
    (cond 
      ((null input) (return-from dfa t))
      
      ;Operator case
      ((not (equal op nil))
        (cond 
          ((string= op "OP_OC") ;; " character case.
            (if (= quote 0) (progn (write_file op) (setq quote (+ 1 quote))) (progn (write_file "OP_CC") (setq quote 0))))
          (t (if (string= op "SEMICOLON") (write_file ";" 1) (write_file op)))))
      
      ((and (> (length (car input)) 1) (string= (subseq (car input) 0 2) ";;")) (progn (setq op "COMMENT") (write_file "COMMENT")))

      ((or (string= (subseq (car input) 0 1) "-") (string= (subseq (car input) 0 1) "+"))
        (if (not (equal (is_digit_w (subseq (car input) 1)) nil))
          (if (and (string= (subseq (car input) 1 2) "0") (> (length (car input)) 2)) 
            (write_file (car input) 1)
            (write_file "VALUE"))
          (write_file (car input) 1)))

      ;keyword case
      ((not (equal kywrd nil)) (write_file kywrd))

      ;identifier
      ((not (equal (is_identifier_w (car input)) nil)) (write_file "IDENTIFIER"))

      ;value
      ((not (equal (is_digit_w (car input)) nil))
        (if (and (string= (subseq (car input) 0 1) "0") (> (length (car input)) 1)) 
          (write_file (car input) 1)
          (write_file "VALUE")))
      
      (t (if (not (string= (car input) "NewLine")) (write_file (car input) 1)))
    ))
  (if (and (string= op "COMMENT") (not (equal (cdr input) nil)) (not (string= (cadr input) "NewLine"))) 
    (dfa (pass_comment input)) (dfa (cdr input))))

(defun pass_comment (list)
  (if (and (not (equal (car list) nil)) (not (string= (car list) "NewLine"))) (pass_comment (cdr list)) list)
)

;; Program takes input from given file.
(defun intepreter_file (file)
  (setq inputsForParser (split_input (read_file file) nil ""));;Stores expresions for parser.
  (setq error_check (dfa (split_input (read_file file) nil "")))
  (if (not (equal error_check t)) (write_file error_check 1)))

; Takes input from terminal until user gives an empty string. (2 times enter.)
; IN HW1 IT WAS WAITING UNTIL EMPTY STRING BUT FOR PARSER, IT READS AND CREATES TOKENDS DIRECTLY.
(defun read_terminal (str)
  (setq input (read-line ))
  input)

;(if (string= input "") str (read_terminal (concatenate 'string str (concatenate 'string input (string #\NewLine))))))

(defun interpreter ()
  (setq input (convert_to_string_list (coerce (read_terminal "") 'list)))
  (setq inputsForParser (split_input input nil "")) ;;Stores expresions for parser.
  (setq error_check (dfa (split_input input nil "")))
  (if (not (equal error_check t)) (write_file error_check 1)))

(defun lexer_main ()
;  (clear_file)
  (if (null *args*)
    (interpreter) ;; If there is no input file provided as argument
    (intepreter_file (car *args*)) ;; If there is file provided as an argumenT
  )
  (setq tokensForParser (reverse tokensForParser))
)

