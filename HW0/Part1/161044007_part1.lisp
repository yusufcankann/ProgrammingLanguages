;;;; 161044007 - YUSUF CAN KAN

(defun flattener ()

  (setf my_list nil) ; Create an empty list.
  (setq char nil)

  ;;;; READING PART
  (let ((my_string "")) ;set a local variable for reading file
  (with-open-file (stream "./nested_list.txt") ;open the file
      (do ((char (read-char stream nil) ;read the file.
                (read-char stream nil)))
                ((null char))
        (cond
          ((not (or (equal char #\Newline) (string= char "(") (or (string= char " ") (string= char ")")))) (setq my_string
                                      (concatenate 'string my_string (string char)))) ;if readed character is a number, store it in string
          ((or (string= char "(") (or (string= char " ") (string= char ")")))     ;if readed character is not number
            (progn (if (not (string= my_string "")) (setf my_list (cons my_string my_list)) )  ;convert number to int and add it in a list.
                  (setq my_string ""))) ;clear storage string.
        )
   )
   (if (not (string= my_string ""))
      (setf my_list (cons my_string my_list)))
    (close stream)) ;Close the file.
  )

  (setf my_list (reverse my_list)) ; Reverse the list for obtaining the same order
                                   ; in the file.

  ;;;; WRITING PART
  (with-open-file (write_stream "./flattened_list.txt"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (format write_stream "~a~%" my_list)
      (close write_stream)) ;Close the file

)

;;;; MAIN PART

(flattener)
