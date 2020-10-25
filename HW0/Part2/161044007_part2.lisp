;;;; 161044007 - YUSUF CAN KAN

;;;; VARIABLES
(setf my_list nil) ; Creates an empty list for storing boundaries.



;;; This function clears inside the "primedistribution.txt" file if it exis.
;;; If file does not exits it creates empty file.
(defun clear_file ()
  (with-open-file (write_stream "./primedistribution.txt"
          :direction :output
          :if-exists :supersede
          :if-does-not-exist :create)
    (close write_stream)) ;Close the file
)


; Prime function. X is the number, y is always 2.
;If given x is prime it returns true.
(defun isPrime (x y)
  (cond
  ((> y (/ x 2)) t)
  ((= 0 (mod x y)) nil)
  (t (isPrime x (+ y 1)))
  )
)


; Wrapper for isPrime function.
(defun isPrimeCheck (x)
  (cond
    ( (< x 2) nil)
    (t (isPrime x 2))
    )
)


; Semi prime function. x is the number, t is always 2.
; If given number is semi prime it returns true.
(defun semi-prime (x y)
  (cond
  ((> y (/ x 2)) t)
  ((= 0 (mod x y))
        (cond ;if both of the divider is prime
          ((and (isPrimeCheck y) (isPrimeCheck (/ x y))) t)))
  (t (semi-prime x (+ y 1)))  ;Recursive part
  )
)


;Semi prime wrapper function.
(defun isSemiPrime (x)
  (cond
    ((< x 3) nil)
    (t (semi-prime x 2))
  )
)


;; Read file function. It reads boundaries and stores in a list.
(defun read_file ()
(let ((my_string "")) ;set a local variable reading numbers
(with-open-file (stream "./boundries.txt" :if-does-not-exist :create) ;open the file
    (do ((char (read-char stream nil) ;read the file.
              (read-char stream nil)))
              ((null char))
              (cond
                ((or (and (string<= char "9") (string>= char "0")) (string= char "-")) (setq my_string
                                            (concatenate 'string my_string (string char)))) ;if readed character is a number, store it in string
                ((string= char " ")     ;if readed character is not number
                  (progn (if (not (string= my_string "")) (setf my_list (cons (parse-integer my_string) my_list)))  ;convert number to int and add it in a list.
                        (setq my_string ""))) ;clear storage string.
              )
         )
 (if (not (string= my_string ""))
    (setf my_list (cons (parse-integer my_string) my_list)))
  (close stream)) ;Close the file.
)
  (setf my_list (reverse my_list)) ; Reverse the list for sorted order.
)


;; Write file function. It prints the results in a file.
(defun write_file (x y)
  ;;;; WRITING PART

  (with-open-file (write_stream "./primedistribution.txt"
        :direction :output
        :if-exists :append
        :if-does-not-exist :create)

      (cond
        ((= y 0)(format write_stream "~a is prime~%" x))
        (t (format write_stream "~a is Semi-prime~%" x))
      )

  (close write_stream)) ;Close the file
)

;primecrawler function. Calculates prime and semi-prime and printes in file.
(defun primecrawler (x y)
  (cond
    ((> x y) t) ; Control for boundaries
    (t (progn
      (if (isPrimeCheck x) (write_file x 0) ;Prime check
        ( if (isSemiPrime x) (write_file x 1) )) ;semi prime check.
      (primecrawler (+ x 1) y) ;Recursive call
     ))
  )
)

; Checks if given data is number.
(defun number_check (data)
  (if (and (<= data 9)(>= data 9)) t nil)
)


;;;; MAIN PART

(read_file) ;Read the file and store boundaries.

(if (or (not (= (list-length my_list) 2))
            (number_check (car my_list))
              (number_check (cadr my_list)))
              (print "Please provide proper input file!")
              (progn
                (clear_file);; This function clears the "primedistribution.txt" file if it exist,
                             ;; otherwise it creates itfile.

                (if (< (length my_list) 2) ; Control if given file is empty or not.
                  (print "Please provide boundaries!")
                  (primecrawler (car my_list) (cadr my_list)))

                )
              )
