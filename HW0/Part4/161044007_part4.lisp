;; 161044007 YUSUF CAN KAN

; Converts given character string into a list.
(defun convert_to_string_list (list)
  (if (not (equal list nil))
    (cond 
      ;; Sets space with "SPACE" string.
      ((equal (car list) #\Space ) (cons "SPACE" (convert_to_string_list (cdr list)))) 
      ;converts first character to string and recursively calls rest of the list.
      (t (progn (cons (string (car list)) (convert_to_string_list (cdr list)))))                      
)))


;; Reads every line and every character recursively and returns as a list.
(defun read_recursively (file)
 (let ((line (read-line file nil nil)))
  (if (not (eql line nil))
      ;Appends new line every end of the operation.
      (append (append (convert_to_string_list (coerce line 'list)) (read_recursively file)) (list "NEWLINE"))
)))



(defun read_file ()
	; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
(with-open-file (stream "./paragraph.txt" :if-does-not-exist :create)
  (reverse (cdr (reverse (read_recursively stream)))) ;;Reads every line and every character with read_recursively function.
))
  


;;This function takes an element(character and frequenxy list and does the following;
;; -If element frequency already added in the list it increases the frequency and
;;    returns the new list.
;; -If list does not contains element it adds the element in frequency list and
;;    sets its initial frequency value to 1.
(defun add_frequency_list (element freq_list)
  (cond
    ;If frequenxy list is empty it creates new value for given element and sets
    ; the freq. value to 1.
    ((equal freq_list nil) (list (list 1 element)))

    ;; If first element is our element it means we found our element.
    ;; In this case we need to increase the frequecy 1. We create new node
    ;; with same element but frequency is increased by 1 and construct with
    ;; rest of the list. In this way we get rid of old initial value and set the
    ;; new value. At last we return the list.
    ((equal (list element) (cdr (car freq_list))) ;; If first elements character is equal
      (cons (list (+ 1 (car (car freq_list))) (cadr (car freq_list))) (cdr freq_list)))

    ;; This part is the recursive part. (cons ..) function constructs a new list
    ;; with 1 element and 1 list. If we put first element of our list as a first
    ;; parameter and call the function with recursively using rest of out list
    ;; in this way we iterates our list. For this reason we control only first element
    ;; of our list above condition. So in this part each iteration removes the first
    ;; element in the list and uses the rest of the list and when recursive calls
    ;; going back it constructs first elements one by one.
    (t (cons (car freq_list) (add_frequency_list element (cdr freq_list))))
  )
)


;; It reads every element recursively in elements_list (which is our paragraph)
;; and it constructs a frequecy list.
(defun construct_freq_list (elements_list freq_list)
  (cond
    ; If there is no character lefts it returns freq_list.
    ((equal elements_list nil) freq_list)

    ; This part is a recursive call. First it adds frequency of first element to
    ; frequency list and re-does the same operation for rest of the list.
    (t (progn (setq freq_list (add_frequency_list (car elements_list) freq_list)))
          (construct_freq_list (cdr elements_list) freq_list)))
)


;; It takes 2 list and checks first elements of both list.
;; If list1's first element is smaller than the other it returns true.
;; This function using for sorting parameters with frequencies.
(defun compare_frequency (list1 list2) (<  (car list1)  (car list2)))


;; This function takes a list and sorts the list with respect to
;; compare_frequency function.
(defun sort_my_list (mylist) (sort mylist #'compare_frequency))


;Gives the first element value of huffman tree.
;Ex: ( (12 (a 6) (b 6)) (15 (a 5) (b 10)) ) => 12.
(defun f (list) (car (car list)))


;Gives the second element value of huffman tree.
;Ex: ( (12 (a 6) (b 6)) (15 (a 5) (b 10)) ) => 15.
(defun s (list) (car (cadr list)))

; Compares 2 huffman codes and returns t if first one is smaller than other.
(defun compare_huff_code_length (s1 s2) (< (length (cadr s1)) (length (cadr s2))))

; Sorts the result list with respect to its code length.
(defun sort_result_list (result) (sort result #'compare_huff_code_length))

;Creates a huffman tree.
(defun create_huffman (huff_tree)
  (cond
    ((= (list-length huff_tree) 1) huff_tree)
    (t (create_huffman  ;Recursive part construct new tree and combines.
      (sort_my_list
        (cons (list (+ (f huff_tree) (s huff_tree))
                      (car huff_tree) (cadr huff_tree))
            (cdr (cdr huff_tree))))))
))

;Wrapper for create_huffman
(defun create_huffman_wrap (huff_tree)
  (if (equal huff_tree nil) nil
    (create_huffman huff_tree))
)


;; This function takes an huffman tree and encodes it in a list and returns it.
;; Returned list will be as ((A 1100) (B 101))
(defun huffman_encode (huffman_tree string_code store)
  (cond
    ((= (list-length huffman_tree) 3)
      (progn
        ;Right branch of tree.
        (setf store
          (huffman_encode (cadr huffman_tree) (concatenate 'string string_code "0") store))
        ;Left branch of tree.
        (setf store
          (huffman_encode (caddr huffman_tree) (concatenate 'string string_code "1") store))
     ))
    ; When we find leaf node we construct new node for result list.
    ((= (list-length huffman_tree) 2)
      (progn
       (setf store (cons (list (cadr huffman_tree) string_code) store )))
)))


;; Wrarapper function for huffman_encod
;; It controls some inial situations.
(defun huffman_encode_wrap (huffman_tree)
  (setf result ())
  (setq string_for_codes "")
  (cond
    ((= (list-length huffman_tree) 2) (list (list (cadr huffman_tree) "0")))
    (t (huffman_encode  huffman_tree string_for_codes result))))

;; Write file function. It prints the results in a file.
(defun write_file (x y)
  ;;; WRITING PART
  (with-open-file (write_stream "./huffman_codes.txt"
        :direction :output
        :if-exists :append
        :if-does-not-exist :create)
      (format write_stream "~a: ~a ~%" x y)
  (close write_stream)) ;Close the file
)

;This function writes result in file.
(defun write_result (huff_result_list)
  (if (equal huff_result_list  nil) t
    (progn
      (write_file (car (car huff_result_list)) (cadr (car huff_result_list)))
      (write_result (cdr huff_result_list))
    )))


;; This function clears inside the "collatz_outputs.txt" file if it exis.
;; If file does not exits it creates empty file.
(defun clear_file ()
  (with-open-file (write_stream "./huffman_codes.txt"
    :direction :output
    :if-exists :supersede
    :if-does-not-exist :create)
  (close write_stream)) ;Close the file
)

;; MAIN
(defun main ()

  (setf words (read_file)) ;Read file

  (setf frq ())
  (setf frq (construct_freq_list words frq))
  (setf tree (car (create_huffman_wrap (sort_my_list frq))))

  (clear_file) ;If there is an existing file it clears it. If there is not
               ; It creates an empty file.
  (write_result (sort_result_list (huffman_encode_wrap tree)))

)


(main)


