(load "./gpp_lexer.lisp")

(setq line 0)
(setq errno -500)
(setq errVar "")
(setq kwExit 0)
(setf identifierList nil)
(setf functionList nil)
(setf resultList nil)

;; Contains error informations
(defun errorFunction(errno)
    (cond
        ((equal errno 1) "Identifier has no value!!")
        ((equal errno 2) "Paranthesis Error!!")
        ((equal errno 3) "Expression couldn't parse.")
        ((equal errno 4) (format nil "Variable ~% doesn't have any value." errVar))
        ((equal errno 5) "Invalid arguments for set")
        ((equal errno 6) "Function definition is wrong!")
        ((equal errno 7) "Undefined function!!")
        ((equal errno 8) "Please provide proper boolean expression!")
        ((equal errno 9) "Please check your function variable count!")
        ((equal errno 10) "Invalid syntax structure in for loop.")
        (t "ERROR!!")))


(defun write_file_parser (x)
  ;;; WRITING PART
  (if (not (null x)) (progn 
  (with-open-file (write_stream "./parsed_lisp.txt"
        :direction :output
        :if-exists :append
        :if-does-not-exist :create)
      (format write_stream "~a ~%" (car x)))
  ;(close write_stream)
  (write_file_parser (cdr x))
  ))) ;Close the file



(defun isEXPB (tokenList)
    (if (or (equal (car tokenList) "KW_TRUE") (equal (car tokenList) "KW_FALSE"))
        (return-from isEXPB t))

    (if (and (> (list-length tokenList) 2) (equal (car tokenList) "OP_OP"))
        (cond 
            ((equal (cadr tokenList) "KW_AND") (return-from isEXPB t))
            ((equal (cadr tokenList) "KW_OR") (return-from isEXPB t))
            ((equal (cadr tokenList) "KW_NOT") (return-from isEXPB t))
            ((equal (cadr tokenList) "KW_EQUAL") (return-from isEXPB t))
            ((equal (cadr tokenList) "KW_LESS") (return-from isEXPB t))
            (t (return-from isEXPB nil))
        ))
    (return-from isEXPB nil))

(defun isEXPI (tokenList)
    (if (or (equal (car tokenList) "IDENTIFIER") (equal (car tokenList) "VALUE"))
    (return-from isEXPI t))

    (if (and (> (list-length tokenList) 2) (equal (car tokenList) "OP_OP"))
    (cond 
        ((equal (cadr tokenList) "OP_PLUS") (return-from isEXPI t))
        ((equal (cadr tokenList) "OP_MINUS") (return-from isEXPI t))
        ((equal (cadr tokenList) "OP_DIV") (return-from isEXPI t))
        ((equal (cadr tokenList) "OP_MULT") (return-from isEXPI t))
        ((equal (cadr tokenList) "OP_DBLMULT") (return-from isEXPI t))
        ((equal (cadr tokenList) "KW_DEFVAR") (return-from isEXPI t))
        ((equal (cadr tokenList) "KW_SET") (return-from isEXPI t))
        ((equal (cadr tokenList) "IDENTIFIER") (return-from isEXPI t))
        ((equal (cadr tokenList) "KW_DEFFUN") (return-from isEXPI t))
        ((equal (cadr tokenList) "KW_IF") (return-from isEXPI t))
        ((equal (cadr tokenList) "KW_FOR") (return-from isEXPI t))
        (t (return-from isEXPI nil))
    )))

(defun isLISTVALUE (tokenList)
    (if (equal (car tokenList) "KW_NIL") (return-from isLISTVALUE t))
    (if (and (> (list-length tokenList) 2) (equal (car tokenList) "OP_OP"))
    (cond 
        ((equal (cadr tokenList) "KW_NIL") (return-from isLISTVALUE t))
        ((equal (cadr tokenList) "KW_LIST") (return-from isLISTVALUE t))
        (t (return-from isLISTVALUE nil))
    )))

(defun isEXPLISTI (tokenList)
    (if (isLISTVALUE tokenList) (return-from isEXPLISTI t))
    (if (and (> (list-length tokenList) 2) (equal (car tokenList) "OP_OP"))
    (cond 
        ((equal (cadr tokenList) "KW_CONCAT") (return-from isEXPLISTI t))
        ((equal (cadr tokenList) "KW_APPEND") (return-from isEXPLISTI t))
        (t (return-from isEXPLISTI nil))
)))



;;;;FOR FUNCTION IMPLEMENTATIONS

(defun isFunctionDefined (iden flist)
    (cond
        ((null flist) (progn (setq errno 7) (return-from isFunctionDefined -1)))
        ((string= (caar flist) iden) (return-from isFunctionDefined (car flist)))
        (t (isFunctionDefined iden (cdr flist)))))

;;Returns the given statements with respect to count.
(defun returnCountStatement (inputList count)
    (if (equal count 0) (return-from returnCountStatement (list ))
        (append (list (car inputList)) (returnCountStatement (cdr inputList) (- count 1)))))

;RETURNS FUNCTION BODY
(defun functionBody (tokenList inputList)
(let ((count 0))
    (setq count (nextStatementCount tokenList 0 0))
    (if (equal count -1) (return-from functionBody -1)) ;;error check
    (setf tokenList (return_rest tokenList count))
    (setf inputList (return_rest inputList count))

    (setq count (nextStatementCount tokenList 0 0))
    (if (equal count -1) (return-from functionBody -1)) ;;error check
    (return-from functionBody (list (returnCountStatement tokenList count) (returnCountStatement inputList count)))))

;RETURN FUNCTION ID LIST
(defun functionIDList (tokenList inputList)
    (if (equal (car tokenList) "IDENTIFIER") 
            (append (list (car inputList)) (functionIDList (cdr tokenList) (cdr inputList)))
    (return-from functionIDList (list ))))


(defun asigneValue (tokenList inputList variableName value)
    (let ((result nil))
    (cond
        ((null tokenList) (return-from asigneValue (list (list ) (list ))))
        ((equal (car inputList) variableName)
            (progn 
                (setf result (asigneValue (cdr tokenList) (cdr inputList) variableName value))
                (return-from asigneValue (list (append (list "VALUE") (car result)) (append (list value) (cadr result))))))
        (t (progn 
                (setf result (asigneValue (cdr tokenList) (cdr inputList) variableName value))
                (return-from asigneValue 
                        (list (append (list (car tokenList)) (car result)) (append (list (car inputList)) (cadr result)))))))
))

;;Rewrites the variables inside the function with respect to function arguments.
(defun assignFunction (tokenList inputList variables values)
    (let ((resultarr nil))
    (cond
        ((not (equal (list-length variables) (list-length values)))(progn (setq errno 9) (return-from assignFunction -1)))
        ((null variables) (return-from assignFunction (list (list tokenList) (list inputList))))
        (t (progn 
                (setf resultarr (asigneValue tokenList inputList (car variables) (write-to-string (car values))))
                (assignFunction (car resultarr) (cadr resultarr) (cdr variables) (cdr values)))))))


;; Clculates for loop and returns every iteration result (EXPLIST) in a list.
;; (for (i 1 3) (list i (+ i i)))
;; RESULT ((1 2) (2 4)) 
(defun calculateFor (tokenList inputList val1 val2 iden)
    (cond
        ((>= val1 val2) (return-from calculateFor (list )))
        (t (progn
                (setf exprr (asigneValue tokenList inputList iden (write-to-string val1)))
                (append (list (calculateEXPLISTI (car exprr) (cadr exprr))) (calculateFor tokenList inputList (+ 1 val1) val2 iden)) 
))))


;;wrapper for get identifier
(defun getId (id) (getIdentifier id identifierList))


;;wrapper for set identifier
(defun setId (id value) (setIdentifier id value identifierList) (return-from setId value))


;;wrapper for change identifier
(defun changeId (id value)
    (setf identifierList (changeIdentifier id value identifierList))
)


;Returns id value from identifier list
(defun getIdentifier (id idList)
    (cond 
        ((equal idList nil) (progn (setq errno 4) (setq errVar id) (return-from getIdentifier -1)))
        ((equal (caar idList) id) (cadar idList))
        (t (getIdentifier id (cdr idList)))))


;Set id value or assign value to id.
(defun setIdentifier (id value idList)
    (cond 
        ((equal idList nil) (setf identifierList (append (list (list id value)) identifierList)))
        ((equal (caar idList) id) (changeId id value))
        (t (setIdentifier id value (cdr idList)))))



;Updates the identifier value
(defun changeIdentifier (id value idList)
    (if (equal id (caar idList)) (append (list (list id value)) (cdr idList))
        (cons (car idList) (changeIdentifier id value (cdr idList)))))



;;Calculates double mult op.
(defun doubleMult (x y) (if (equal y 0) 1 (* x (doubleMult x (- y 1)))))



;; Prints given input to screen
(defun printscreen (x &optional (printLine -1))
    (if (equal printLine -1)
        (format t "~a ~%" x)
        (progn (format t "[%d]>" line) (setq line (+ 1 line)))))



;; returns the element count of next statement
(defun nextStatementCount(tokenList count rCount)
    ;;If first statement is just boolean expression it returns 1.
    (if (and (equal count 0) (not (null tokenList)) 
            (or (equal (car tokenList) "IDENTIFIER") (equal (car tokenList) "VALUE") 
                (equal (car tokenList) "KW_TRUE") (equal (car tokenList) "KW_FALSE"))) 
                (return-from nextStatementCount 1))
    (cond 
        ((equal (car tokenList) "OP_OP") (setq count (+ count 1)))
        ((equal (car tokenList) "OP_CP") (setq count (- count 1))))
    (if (and (not (equal count 0)) (null tokenList)) 
        (progn (setq errno 2) (return-from nextStatementCount -1)))
    (if (equal count 0) (+ 1 rCount) 
        (nextStatementCount (cdr tokenList) count (+ 1 rCount))))


; Removes given count element and returns it
(defun return_rest (exp count) (if (not (equal count 0)) (return_rest (cdr exp) (- count 1)) exp))


;; removes 1 full statement (such as "(and true false)") from lists.
(defun removeNextStatement (tokenList inputList)
    (setq c (nextStatementCount tokenList 0 0))
    (if (equal c -1) (return-from removeNextStatement -1))
    (if (not (equal tokenList nil)) (setf tokenList (return_rest tokenList c)) (setf tokenList nil))
    (if (not (equal inputList nil)) (setf inputList (return_rest inputList c)) (setf inputList nil))
    (return-from removeNextStatement (list tokenList inputList)))



;;On error return -1, return boolean exp. otherwise.
(defun calculateEXPB (tokenList inputList)
    (let ((exp1 nil) (exp2 nil)) ;;define local variables for this function
    (cond 
        ((equal (car tokenList) "KW_TRUE") (return-from calculateEXPB t))
        ((equal (car tokenList) "KW_FALSE") (return-from calculateEXPB nil)))
    
    (if (and (> (list-length tokenList) 4) (equal (car tokenList) "OP_OP"))
        (cond
            ((equal (cadr tokenList) "KW_AND") ;;AND
                (progn
                    (setq exp1 (calculateEXPB (cddr tokenList) (cddr inputList))) ;;calculate first expression
                    (setq valuesForParser (removeNextStatement (cddr tokenList) (cddr inputList)))
                    (if (equal valuesForParser -1) (return-from calculateEXPB -1))
                    (setf tokenList (car valuesForParser))
                    (setf inputList (cadr valuesForParser))
                    (setf tokenList (append (list "OP_OP") (append (list "KW_AND") tokenList)))
                    (setf inputList (append (list "(") (append (list "and") inputList)))
                    (if (equal exp1 -1) (return-from calculateEXPB -1)) ;;error check            
                    (setq exp2 (calculateEXPB (cddr tokenList)(cddr inputList)))
                    (if (equal exp2 -1) (return-from calculateEXPB -1))
                    (return-from calculateEXPB (and exp1 exp2))
                ))
            ((equal (cadr tokenList) "KW_OR") ;OR
                (progn
                    (setq exp1 (calculateEXPB (cddr tokenList) (cddr inputList))) ;;calculate first expression
                    (setq valuesForParser (removeNextStatement (cddr tokenList) (cddr inputList)))
                    (if (equal valuesForParser -1) (return-from calculateEXPB -1))
                    (setf tokenList (car valuesForParser))
                    (setf inputList (cadr valuesForParser))
                    (setf tokenList (append (list "OP_OP") (append (list "KW_OR") tokenList)))
                    (setf inputList (append (list "(") (append (list "or") tokenList)))
                    (if (equal exp1 -1) (return-from calculateEXPB -1)) ;;error check
                    (setq exp2 (calculateEXPB (cddr tokenList)(cddr inputList)))
                    (if (equal exp2 -1) (return-from calculateEXPB -1))
                    (return-from calculateEXPB (or exp1 exp2))
                ))
            ((equal (cadr tokenList) "KW_NOT") ;; NOT
                (progn
                    (setq exp1 (calculateEXPB (cddr tokenList)(cddr inputList))) ;;calculate first expression
                    (if (equal exp1 -1) (return-from calculateEXPB -1))
                    (return-from calculateEXPB (not exp1))))
             ((equal (cadr tokenList) "KW_EQUAL")
                (progn
                    (cond 
                        ((isEXPB (cddr tokenList))  (setq exp1 (calculateEXPB (cddr tokenList)(cddr inputList))))
                        ((isEXPI (cddr tokenList))  (setq exp1 (calculateEXPI (cddr tokenList)(cddr inputList)))))

                        (setq valuesForParser (removeNextStatement (cddr tokenList) (cddr inputList)))
                        (if (equal valuesForParser -1) (return-from calculateEXPB -1))
                        (setf tokenList (append (list "OP_OP") (append (list "KW_EQUAL") (car valuesForParser))))
                        (setf inputList (append (list "(") (append (list "equal") (cadr valuesForParser))))

                        (if (equal exp1 -1) (return-from calculateEXPB -1)) ;;error check  
                        (cond
                        ((isEXPB (cddr tokenList))  (setq exp2 (calculateEXPB (cddr tokenList)(cddr inputList))))
                        ((isEXPI (cddr tokenList))  (setq exp2 (calculateEXPI (cddr tokenList)(cddr inputList)))))

                        (return-from calculateEXPB (equal exp1 exp2))))
                ((equal (cadr tokenList) "KW_LESS")
                (progn
                        (setq exp1 (calculateEXPI (cddr tokenList)(cddr inputList)))
                        (setq valuesForParser (removeNextStatement (cddr tokenList) (cddr inputList)))
                        (if (equal valuesForParser -1) (return-from calculateEXPB -1))
                        (setf tokenList (append (list "OP_OP") (append (list "KW_LESS") (car valuesForParser))))
                        (setf inputList (append (list "(") (append (list "<") (cadr valuesForParser))))

                        (if (equal exp1 -1) (return-from calculateEXPB -1)) ;;error check  
                        (setq exp2 (calculateEXPI (cddr tokenList)(cddr inputList)))
                        (return-from calculateEXPB (< exp1 exp2))))
            (t (progn (setq errno 3) (return-from calculateEXPB -1))))
        (progn (setq errno 3) (return-from calculateEXPB -1)))))



;;CALCULATES EXPI (+ - / * **) returns value.
(defun calculateEXPI (tokenList inputList)
    (let ((exp1 nil) (exp2 nil)) ;;define local variables for this function
    (cond 
        ((equal (car tokenList) "VALUE") (return-from calculateEXPI (parse-integer (car inputList))))
        ((equal (car tokenList) "IDENTIFIER")
            (progn
                (setq rval (getId (car inputList)))
                (if (typep rval 'string) (setq rval (parse-integer rval)))
                (if (null rval) (return-from calculateEXPI -1) (return-from calculateEXPI rval)))))

    (if (and (> (list-length tokenList) 4) (equal (car tokenList) "OP_OP"))
        (cond
            ((equal (cadr tokenList) "OP_PLUS")
                (progn
                    (setq exp1 (calculateEXPI (cddr tokenList)(cddr inputList) ))
                    (setq valuesForParser (removeNextStatement (cddr tokenList) (cddr inputList)))
                    (if (equal valuesForParser -1) (return-from calculateEXPI -1))
                    (setf tokenList (append (list "OP_OP") (append (list "OP_PLUS") (car valuesForParser))))
                    (setf inputList (append (list "(") (append (list "+") (cadr valuesForParser))))
                    (if (and (equal exp1 -1) (not (equal errno -500))) (return-from calculateEXPI -1)) ;;error check            
                    (setq exp2 (calculateEXPI (cddr tokenList) (cddr inputList)))
                    (if (and (equal exp1 -1) (not (equal errno -500))) (return-from calculateEXPI -1))
                    (if (not (typep exp1 'integer)) (setq exp1 (parse-integer exp1)))
                    (if (not (typep exp2 'integer)) (setq exp2 (parse-integer exp2)))
                    (return-from calculateEXPI (+ exp1 exp2))
            ))
            ((equal (cadr tokenList) "OP_MINUS")
                (progn
                    (setq exp1 (calculateEXPI (cddr tokenList)(cddr inputList) ))
                    (setq valuesForParser (removeNextStatement (cddr tokenList) (cddr inputList)))
                    (if (equal valuesForParser -1) (return-from calculateEXPI -1))
                    (setf tokenList (append (list "OP_OP") (append (list "OP_MINUS") (car valuesForParser))))
                    (setf inputList (append (list "(") (append (list "-") (cadr valuesForParser))))
                    (if (and (equal exp1 -1) (not (equal errno -500))) (return-from calculateEXPI -1)) ;;error check            
                    (setq exp2 (calculateEXPI (cddr tokenList) (cddr inputList)))
                    (if (and (equal exp1 -1) (not (equal errno -500))) (return-from calculateEXPI -1))
                    (if (not (typep exp1 'integer)) (setq exp1 (parse-integer exp1)))
                    (if (not (typep exp2 'integer)) (setq exp2 (parse-integer exp2)))
                    (return-from calculateEXPI (- exp1 exp2))
            ))
            ((equal (cadr tokenList) "OP_DIV")
                (progn
                    (setq exp1 (calculateEXPI (cddr tokenList)(cddr inputList) ))
                    (setq valuesForParser (removeNextStatement (cddr tokenList) (cddr inputList)))
                    (if (equal valuesForParser -1) (return-from calculateEXPI -1))
                    (setf tokenList (append (list "OP_OP") (append (list "OP_DIV") (car valuesForParser))))
                    (setf inputList (append (list "(") (append (list "/") (cadr valuesForParser))))
                    (if (and (equal exp1 -1) (not (equal errno -500))) (return-from calculateEXPI -1)) ;;error check          
                    (setq exp2 (calculateEXPI (cddr tokenList) (cddr inputList)))
                    (if (and (equal exp1 -1) (not (equal errno -500))) (return-from calculateEXPI -1))
                    (if (not (typep exp1 'integer)) (setq exp1 (parse-integer exp1)))
                    (if (not (typep exp2 'integer)) (setq exp2 (parse-integer exp2)))
                    (return-from calculateEXPI (/ exp1 exp2))
            ))
            ((equal (cadr tokenList) "OP_MULT")
                (progn
                    (setq exp1 (calculateEXPI (cddr tokenList)(cddr inputList) ))
                    (setq valuesForParser (removeNextStatement (cddr tokenList) (cddr inputList)))
                    (if (equal valuesForParser -1) (return-from calculateEXPI -1))
                    (setf tokenList (append (list "OP_OP") (append (list "OP_MULT") (car valuesForParser))))
                    (setf inputList (append (list "(") (append (list "*") (cadr valuesForParser))))
                    (if (equal exp1 -1) (return-from calculateEXPI -1)) ;;error check            
                    (setq exp2 (calculateEXPI (cddr tokenList) (cddr inputList)))
                    (if (equal exp2 -1) (return-from calculateEXPI -1))
                    (if (not (typep exp1 'integer)) (setq exp1 (parse-integer exp1)))
                    (if (not (typep exp2 'integer)) (setq exp2 (parse-integer exp2)))
                    (return-from calculateEXPI (* exp1 exp2))
            ))
            ((equal (cadr tokenList) "OP_DBLMULT")
                (progn
                    (setq exp1 (calculateEXPI (cddr tokenList)(cddr inputList) ))
                    (setq valuesForParser (removeNextStatement (cddr tokenList) (cddr inputList)))
                    (if (equal valuesForParser -1) (return-from calculateEXPI -1))
                    (setf tokenList (append (list "OP_OP") (append (list "OP_DBLMULT") (car valuesForParser))))
                    (setf inputList (append (list "(") (append (list "**") (cadr valuesForParser))))
                    (if (equal exp1 -1) (return-from calculateEXPI -1)) ;;error check            
                    (setq exp2 (calculateEXPI (cddr tokenList) (cddr inputList)))
                    (if (equal exp2 -1) (return-from calculateEXPI -1))

                    (if (not (typep exp1 'integer)) (setq exp1 (parse-integer exp1)))
                    (if (not (typep exp2 'integer)) (setq exp2 (parse-integer exp2)))
                    (return-from calculateEXPI (expt exp1 exp2))
            ))
            ((or (equal (cadr tokenList) "KW_SET") (equal (cadr tokenList) "KW_DEFVAR"));;set and defvar
                (cond 
                    ((and (equal (caddr tokenList) "IDENTIFIER") (equal (cadddr tokenList) "VALUE")) 
                        (setId (caddr inputList)(parse-integer (cadddr inputList))))
                    ((and (equal (caddr tokenList) "IDENTIFIER") (equal (cadddr tokenList) "OP_OP")) 
                        (progn
                        (setq exp1 (calculateEXPI (cdddr tokenList)(cdddr inputList)))
                        (if (and (equal exp1 -1) (not (equal errno -500))) (return-from calculateEXPI -1))
                        (setId (caddr inputList) exp1)))
                    (t (progn (setq errno 5) (return-from calculateEXPI -1))
                ))
            )
            ((equal (cadr tokenList) "IDENTIFIER")
                (progn
                    (setq exp1 (isFunctionDefined (cadr inputList) functionList))
                    (if (equal exp1 -1) (return-from calculateEXPI -1))
                    

                    (setq exp2 (assignFunction (caaddr exp1) (car (cdaddr exp1)) (cadr exp1) 
                                                                (calculateEXPLISTI (cddr tokenList) (cddr inputList))))
                    (return-from calculateEXPI (car (last (calculateEXPLISTI (caar exp2) (caadr exp2)))))
            ))
            ((equal (cadr tokenList) "KW_DEFFUN")
                (if (equal (caddr tokenList) "IDENTIFIER")
                    (progn
                        (setf f1 (functionIDList (cddddr tokenList) (cddddr inputList)))
                        (setf f2 (functionBody (cdddr tokenList) (cdddr inputList)))
                        (if (or (equal f1 -1) (equal f2 -1)) (progn (setq errno 6) (return-from calculateEXPI -1)))
                        (setf functionList (append (list (list (caddr inputList) f1 f2)) functionList))
                        (return-from calculateEXPI 0))
            ))
            ((equal (cadr tokenList) "KW_IF")
                (progn
                    (if (not (equal (isEXPB (cddr tokenList)) t))(progn (setq errno 8) (return-from calculateEXPI -1))
                        (if (equal (calculateEXPB (cddr tokenList)(cddr inputList)) t)
                            (progn 
                                (setq exp1 (removeNextStatement (cddr tokenList) (cddr inputList)))
                                (setf tokenList (car exp1))
                                (setf inputList (cadr exp1))
                                
                                (return-from calculateEXPI (car (last (calculateEXPLISTI tokenList inputList))))
                            )
                        (return-from calculateEXPI))
            )))
            ((equal (cadr tokenList) "KW_FOR") ;; RETURNS LAST EXPRESSION OF LAST ITERATION
                (progn
                    (if (not (equal (caddr tokenList) "OP_OP")) (progn (setq errno 10) (return-from calculateEXPI -1))
                    (car (last (car (last (calculateFor (cdddr (cddddr tokenList)) (cdddr (cddddr inputList)) 
                        (parse-integer (car (cddddr inputList))) (parse-integer (cadr (cddddr inputList))) (cadddr inputList)))))))
                )
            )
            (t (progn (setq errno 3) (return-from calculateEXPI -1))))
        (progn (setq errno 3) (return-from calculateEXPI -1)))))

; Returns values as a list.
(defun calculateLISTVALUE (tokenList inputList)
    (cond 
        ((equal (car tokenList) "KW_NIL") (return-from calculateLISTVALUE nil)))

    (if (and (> (list-length tokenList) 3) (equal (car tokenList) "OP_OP"))
        (cond 
            ((and (equal (car tokenList) "OP_OP") (equal (cadr tokenList) "KW_LIST") (or (isEXPI (cddr tokenList)) 
            (equal (caddr tokenList) "VALUE"))) 
                (calculateVALUES (cddr tokenList) (cddr inputList)))
            ((and (equal (car tokenList) "OP_OP") (equal (cadr tokenList) "KW_LIST") (equal (caddr tokenList) "OP_CP"))
                (return-from calculateLISTVALUE nil))
            (progn (setq errno 3) (return-from calculateLISTVALUE -1)))
            (progn (setq errno 3) (return-from calculateLISTVALUE -1))))


;;Takes every integer value and creates and values list.
(defun calculateVALUES (tokenList inputList)
(let ((e 0))
(if (equal (isEXPI tokenList) t) 
        (progn
            (setq valuesresult (calculateEXPI tokenList inputList))
                (if (and (equal valuesresult -1) (not (equal errno -500))) 
                        (return-from calculateVALUES -1) 
                    (progn 
                        (setq e (removeNextStatement tokenList inputList))
                        (if (equal e -1) (return-from calculateVALUES -1))
                        (setf tokenList (car e))
                        (setf inputList (cadr e))
                        (append (list valuesresult) (calculateVALUES tokenList inputList)))))
        (return-from calculateVALUES (list ))            
)))

(defun calculateEXPLISTI (tokenList inputList)
    (let ((exp1 nil) (exp2 nil))
    (cond 
        ((equal (car tokenList) "KW_NIL")(return-from calculateEXPLISTI nil)))
    (if (> (list-length tokenList) 3)
        (cond
            ((and (equal (car tokenList) "OP_OP") (equal (cadr tokenList) "KW_CONCAT"))
                (progn
                (setq exp1 (calculateLISTVALUE (cddr tokenList) (cddr inputList)))
                (setq c (nextStatementCount (cddr tokenList) 0 0))
                (if (equal c -1) (return-from calculateEXPLISTI -1)) ;;error check
                (setf tokenList (return_rest (cddr tokenList) c)) ;;remove first expression for calculating second.
                (setf tokenList (append (list "OP_OP") (append (list "KW_CONCAT") tokenList)))
                (setf inputList (return_rest (cddr inputList) c)) ;;remove first expression for calculating second.
                (setf inputList (append (list "(") (append (list "concat") inputList)))
                (if (equal exp1 -1)(return-from calculateEXPLISTI -1)) ;;error check            
                (setq exp2 (calculateLISTVALUE (cddr tokenList) (cddr inputList)))
                (if (equal exp2 -1) (return-from calculateEXPLISTI -1))
                (return-from calculateEXPLISTI (concatenate 'list exp1 exp2)))
            )
            ((and (equal (car tokenList) "OP_OP") (equal (cadr tokenList) "KW_APPEND"))
                (progn
                (setq exp1 (calculateEXPI (cddr tokenList) (cddr inputList)))
                (setq c (nextStatementCount (cddr tokenList) 0 0))
                (if (equal c -1) (return-from calculateEXPLISTI -1)) ;;error check
                (setf tokenList (return_rest (cddr tokenList) c)) ;;remove first expression for calculating second.
                (setf tokenList (append (list "OP_OP") (append (list "KW_APPEND") tokenList)))
                (setf inputList (return_rest (cddr inputList) c)) ;;remove first expression for calculating second.
                (setf inputList (append (list "(") (append (list "append") inputList)))
                (if (equal exp1 -1) (return-from calculateEXPLISTI -1)) ;;error check            
                (setq exp2 (calculateLISTVALUE (cddr tokenList) (cddr inputList)))
                (if (equal exp2 -1) (return-from calculateEXPLISTI -1))
                (return-from calculateEXPLISTI (append (list exp1) exp2)))
            )
            ((and (equal (car tokenList) "OP_OP") (equal (cadr tokenList) "KW_LIST"))
                (return-from calculateEXPLISTI (calculateLISTVALUE tokenList inputList)))
            (t (progn (setq errno 3) (return-from calculateEXPLISTI -1))))
            (progn (setq errno 3) (return-from calculateEXPLISTI -1)))))

(defun passComment (inputList)
    (if (null inputList) (return-from passComment nil))
    (if (not (equal (car inputList) "NewLine")) (passComment (cdr inputList))
        (cdr inputList))
)
(defun isComment (tokenList)
    (if (equal (car tokenList) "COMMENT") t)
)

(defun clearComments (inputList)
    (if (null inputList) (return-from clearComments (list )))
    (if (equal (car inputList) ";;")(progn  (setf inputList (append (list (car inputList)) (passComment (cdr inputList))))))
    (append (list (car inputList)) (clearComments (cdr inputList))))


(defun calculateParseTree (tokenList inputList)
(let ((result 0) (disp 0) (ex1 0) (ex2 0) (comm 0))
    (if (null tokenList) (return-from calculateParseTree) 
    (cond
        ((and (> (list-length tokenList) 2) (equal (car tokenList) "OP_OP") 
        (equal (cadr tokenList) "KW_EXIT") (equal (caddr tokenList) "OP_CP"))
            (progn (setq kwExit 1)(return-from calculateParseTree "")))
        ((isComment tokenList) (setq comm 1))
        ((and (equal (car tokenList) "OP_OP") (equal (cadr tokenList) "KW_DISP"))
            (progn (setq disp 1) 
            
                (setq errorC (nextStatementCount tokenList 0 0))
                (if (equal errorC -1) (progn (setq errno 20) (return-from calculateParseTree -1)))

                (setq ex1 (returnCountStatement tokenList errorC))
                (setq ex2 (returnCountStatement inputList errorC))

                (calculateParseTree (cddr (reverse (cdr (reverse ex1)))) 
                (cddr (reverse (cdr (reverse ex2))))))
        )
        ((isEXPB tokenList) (setq result (calculateEXPB tokenList inputList)))
        ((isEXPI tokenList) (setq result (calculateEXPI tokenList inputList)))
        ((isEXPLISTI tokenList)
            (setq result (calculateEXPLISTI tokenList inputList)))
        (t (setq result -1))))
    (if (and (equal result -1) (not (equal errno -500))) (print (errorFunction errno))
        (if (not (null tokenList))
            (progn
                (if (not (equal comm 0)) 
                    (progn
                        (setf inputList (cdr inputList))
                        (setf tokenList (cdr tokenList))
                        (calculateParseTree tokenList inputList))
                (progn
                    (if (equal disp 0) (progn (setf resultList (append (list result) resultList))))
                    (setq newLists (removeNextStatement tokenList inputList))
                    (setf tokenList (car newLists))
                    (setf inputList (cadr newLists))
                    (calculateParseTree tokenList inputList)))
            )))))


(defun clearNewLines (inputList)
    (cond
        ((null inputList) (list ))
        ((equal (car inputList) "NewLine") (clearNewLines (cdr inputList)))
        (t (append (list (car inputList)) (clearNewLines (cdr inputList))))))

(defun parseTree (tokenList inputList)
    (setq inputList (clearComments inputList))
    (setq inputList (clearNewLines inputList))
   
    (calculateParseTree tokenList inputList))


(defun gppinterpreter (&optional file)
    (if (not (equal file nil))
        (progn
            (lexer_main)
            (if (equal errorFlag 1) (progn (write_file_parser (list error_output)) (return-from gppinterpreter -1)))
            (parseTree tokensForParser inputsForParser)
            (write_file_parser (reverse resultList))
            (setf resultList nil))
        (progn
            (lexer_main)
            (if (equal errorFlag 1)(progn (write_file_parser (list error_output)) (return-from gppinterpreter -1)))
            (setf resultList nil)
            (parseTree tokensForParser inputsForParser)
            (setf tokensForParser nil)
            (setf inputsForParser nil)
            (if (not (equal errno -500)) (return-from gppinterpreter))
            (if (not (equal kwExit 1)) (progn (write_file_parser resultList) (gppinterpreter file)))
        )))

(defun parserMain ()
    (clear_file)
    (if (null *args*)
        (gppinterpreter nil)
        (gppinterpreter (car *args*))))


(parserMain)

