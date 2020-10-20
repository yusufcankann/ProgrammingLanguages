;;;; Yusuf Can Kan - 161044007


;;;VARIABLES
(setf my_list nil) ; Creates an empty list for storing integers inside the
                     ; "integer_inputs.txt" file. "read_file" function.

(setq mstring "") ;This string stores the collatz collatz_sequence.
                  ; "collatz_sequence_calculator" function.


;;; FUNCTİONS

;; This function clears inside the "collatz_outputs.txt" file if it exis.
;; If file does not exits it creates empty file.
(defun clear_file ()
  (with-open-file (write_stream "./collatz_outputs.txt"
          :direction :output
          :if-exists :supersede
          :if-does-not-exist :create)
    (close write_stream)) ;Close the file
)


;; Read file function. It reads integers and stores in a list.
(defun read_file ()
  (let ((my_string "")) ;set a local variable reading numbers
  (with-open-file (stream "./integer_inputs.txt") ;open the file
      (do ((char (read-char stream nil) ;read the file.
                (read-char stream nil)))
                ((null char))

        (cond
          ;if readed character is a number, store it in string
          ((or (and (string<= char "9") (string>= char "0")) (string= char "-")) (setq my_string
                      (concatenate 'string my_string (string char))))
          ((string= char " ")     ;if readed character is not number
            ;convert number to int and add it in a list.
            (progn (setf my_list (cons (parse-integer my_string) my_list))
                  (setq my_string ""))) ;clear storage string.
        )
   )
   (if (not (string= my_string ""))
      (setf my_list (cons (parse-integer my_string) my_list)))
    (close stream)) ;Close the file.
  )
  (setf my_list (reverse my_list)) ; Reverse the list for sorted order.
)



;; This function takes an integer and
(defun collatz_sequence_calculator (integer)
  (cond ((eq integer 1) (setq mstring (concatenate 'string mstring "1"))) ;If integer is 1
        ((eq (mod integer 2) 0) ;If integer is even
          (progn
            (setq mstring (concatenate 'string mstring ;Write to string
                                        (write-to-string integer)))
            (setq mstring (concatenate 'string mstring " "))
            (collatz_sequence_calculator (/ integer 2)))) ;Recursive call
        ((eq (mod integer 2) 1) ;If integer is odd
            (progn
             (setq mstring (concatenate 'string mstring ;Write to string
                                         (write-to-string integer)))
             (setq mstring (concatenate 'string mstring " "))
             (collatz_sequence_calculator (+ (* integer 3) 1)))) ;Recursive call
  )
)


;; Write file function. It prints the results in a file.
(defun write_file (x y)
  ;;; WRITING PART
  (with-open-file (write_stream "./collatz_outputs.txt"
        :direction :output
        :if-exists :append
        :if-does-not-exist :create)
      (format write_stream "~a: ~a ~%" x y)
  (close write_stream)) ;Close the file
)


;; This function takes every element in given list and calculates
;; collatz sequence for every integer. It prints the result in
;; "collatz_outputs.txt" file.
(defun collatz_sequence (x)
  (setq mstring "") ;Resets the string
  (cond
    ((equal (car x) nil) t) ;If list is empty finish the recursive.
    ((< (car x) 1) (collatz_sequence (cdr x)))
    (t (progn (write_file (car x) (collatz_sequence_calculator (car x))) ;Calculate and write file.
        (collatz_sequence (cdr x)) )) ;Recursive part.
  )
)

(read_file) ;This function reads the file and stores the int values in my_list

(clear_file) ;This function clears the "collatz_outputs.txt" file if it exist,
              ;otherwise it creates itfile.

(collatz_sequence my_list)
