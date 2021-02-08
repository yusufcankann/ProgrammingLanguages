(defun read_file (my_file)
	; Reads a file containing one word per line and returns a list of words (each word is in turn a list of characters)."
(with-open-file (stream my_file :if-does-not-exist :create)
  (read_recursively stream))) ;;Reads every line and every character with read_recursively function.

;; Reads every line and every character recursively and returns as a list.
(defun read_recursively (file)
 (let ((line (read file nil nil)))
  (if (not (eql line nil))
        (progn ;Appends new line every end of the operation.
            (append line (read_recursively file))))))

;; Write file function. It prints the results in a file.
(defun write_file (x)
  ;;; WRITING PART
  (with-open-file (write_stream "./output.txt"
        :direction :output
        :if-exists :append
        :if-does-not-exist :create)
      (format write_stream "~a ~%" x)
  (close write_stream)) ;Close the file
)

;; This function clears inside the "output.txt" file if it exis.
;; If file does not exits it creates empty file.
(defun clear_file ()
  (with-open-file (write_stream "./output.txt"
          :direction :output
          :if-exists :supersede
          :if-does-not-exist :create)
    (close write_stream)) ;Close the file
)

;;Constructs clause list and returns it
(defun constructClauseList (input)
    (if (not (null (car input)))
        (cond 
            ((not (null (caar input)))
                (if (typep (caadar input) 'string)
                    (append (list (cons (caar input) (list (cdar input)))) (constructClauseList (cdr input)))
                    (append (list (car input)) (constructClauseList (cdr input))))
            ) 
            (t (constructClauseList (cdr input))))))

;; Constructs qeries and returns is
(defun constructQueries (input)
    (if (not (null (car input)))
        (cond 
            ((null (caar input))
                (progn
                    (if (typep (caadar input) 'string)
                        (append (list (cons (caar input) (list (cdar input)))) (constructQueries (cdr input)))
                        (append (list (car input)) (constructQueries (cdr input)))
                    )))

            (t (constructQueries (cdr input))))))

; sorts the clauses with putting facts to the top
(defun comparelength (list1 list2) (<  (length (cadr list1))  (length (cadr list2))))
(defun sort_my_list (mylist) (sort mylist #'comparelength))

; Takes parameter list and returns variable count
; Input: (a b C d E f)
; Output: 2
(defun variableCount (l)
    (if (not (null l))
        (if (and (typep (car l) 'string) (not (some #'lower-case-p (subseq (car l) 0 1))))
            (+ 1 (variableCount (cdr l)))
            (variableCount (cdr l)))0))


(defun findnthindex (l n)
    (cond 
        ((null l) nil)
        ((equal n 0) (car l))
        (t (findnthindex (cdr l) (- n 1)))))

;does s2 contains s1
; (a b c) ( 1 2 3 4  a b c 23 2 ae g)
(defun containsSubList (l1 l2)
    (cond
        ((null l1) t)
        (t  (if (equal (member (car l1) l2 :test 'equal) nil) 
                nil 
                (containsSubList (cdr l1) l2)))))

; i1 index 
; i2 list
(defun findIndexes (i1 i2 i1index i2index)
    (if (not (null i2))
        (if (equal i1 (car i2)) (append (list (list i1index i2index)) (findIndexes i1 (cdr i2) i1index (+ i2index 1)))
        (findIndexes i1 (cdr i2) i1index (+ i2index 1)))
    ))

;; Finds the intersection element indexes
;   Ex:
;   (X Y Z) (Y Z A) => ((1 0) (2,1))
(defun findInterElementIndex (i1 i2 indexi1)
    (if (not (null i1))
            (append (findIndexes (car i1) i2 indexi1 0) (findInterElementIndex (cdr i1) i2 (+ 1 indexi1)))
        ))

(defun isVariable (e)
    (and (typep e 'string) (not (some #'lower-case-p (subseq e 0 1)))))

;; GUNCELLEEEEEEEEE VARIABLE CASE
;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ if (equal ise) or (one of them is variable) 
;find if elemets are the same for given pair
(defun findsameindex (pair s1 s2)
    (if (or (or (isVariable (findnthindex s1 (car pair))) (isVariable (findnthindex s2 (cadr pair))))
    (equal (findnthindex s1 (car pair)) (findnthindex s2 (cadr pair)))) t nil))


;find if elemets are the same for all given pair
(defun isSameIndex (pairs s1 s2)
    (if (not (null pairs))
        (and (findsameindex (car pairs) s1 s2) (isSameIndex (cdr pairs) s1 s2)) t))

; Finds and pairs l1 in all elemnts of l2 with respect to pairs
; Ex:
; Output: ((("X" 1 2) ("X" 1 5)) (("X" 1 2) ("X" 1 6)) (("X" 1 2) ("X" 1 10))) 
; Inputs:
; (setf l1 (list "X" 1 2))
; (setf l2 (list (list "X" 1 5) (list 1 2 3) (list "X" 1 6) (list "5" "X" 2) (list "X" 1 10)))
; (setf pairs (list (list 0 0) (list 1 1)))
(defun intersectionl2 (l1 l2 pairs)
    (if (not (null l2))
        (if (isSameIndex pairs l1 (car l2)) 
            (append (list (append l1 (car l2))) (intersectionl2 l1 (cdr l2) pairs))
            (intersectionl2 l1 (cdr l2) pairs))))

; Finds and pairs all l1 elements in all l2 elements with respect to pairs
; It uses intersectionl2
(defun my_intersection (l1 l2 pairs)
    (if (not (null l1))
        (append (intersectionl2 (car l1) l2 pairs) (my_intersection (cdr l1) l2 pairs))))


; Finds intersection elements given 2 list withrespect to their index names\
; Ex:
; (setf s1 (list (list "A" "B" "C") (list (list 3 2 1) (list 5 1 6) (list 7 2 8) (list 6 3 1) (list 8 8 3) (list 3 2 1) (list 2 1 6))))
; (setf s2 (list (list "C" "D" "E") (list (list 1 2 3) (list 7 3 6) (list 9 3 1) 
    ; (list 2 3 1) (list 1 3 6) (list 6 1 3) (list 6 1 9) (list 8 1 5))))
; (setf s3 (list (list "F" "E" "Z")  (list (list 5 3 6) (list 8 1 3) (list 9 6 5) 
    ; (list 8 3 5) (list 1 9 1) (list 6 7 7) (list 7 7 7) (list 9 5 6) )))
; (print (find_intersection s1 s2))
(defun find_intersection (values1 values2)
    (cond 
        ((null values1) values2)
        ((null values2) values1)
        (t 
    (let ((samevars (findInterElementIndex (car values1) (car values2) 0)))
        ;(if (not (null samevars))
            (cons (append (car values1) (car values2)) (list (my_intersection (cadr values1) (cadr values2) samevars)))
            ;values1
    ))));)



;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;; Finds all intersection with given lists and given
;; element and combine them all to one
(defun findIntersections (e1 l2)
    (if (not (null l2))
        (if (not (containsSubList (caar l2) (car e1)) )
            (findIntersections (find_intersection e1 (car l2)) (cdr l2))
            (findIntersections e1 (cdr l2))) e1))

; ; ; ;;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
; ; ; ;; Finds all intersection with given lists and given
; ; ; ;; element and combine them all to one
; ; ; (defun findIntersections (e1 l2)
; ; ;     (if (not (null l2))
; ; ;         ;(if (not (containsSubList (caar l2) (car e1)) )
; ; ;             (findIntersections (find_intersection e1 (car l2)) (cdr l2))
; ; ;             (findIntersections e1 (cdr l2))) e1))




;Wrapper for findAllIntersection
(defun findAllIntersections (allbody)
    (findAllIntersection allbody (length allbody)))

;; Finds all intersections with given list
;; First element of index must contain list_of_parameters of predicate
(defun findAllIntersection (allbody n)
    (if (not (equal n 0))
        (findAllIntersection (cons (findIntersections (car allbody) (cdr allbody)) (cdr allbody)) (- n 1))
        (car allbody)))


; This function finds the wanted parameters
; From intersected results
; n stores the previously founded element
(defun findPairs (params &optional (n -1))
    (if (not (null params))
        (if (not (equal (caar params) n)) (append (list (cadar params)) (findPairs (cdr params) (caar params)))
            (findPairs (cdr params) (caar params)))))


;; Finds multiple indexes (which placed in l1)
;; in l2 and returns it as a list
(defun findMultipleIndex (l1 l2)
    (if (not (null l1))
        (append (list (findnthindex l2 (car l1))) (findMultipleIndex (cdr l1) l2))))

;; Returns the nessesary variables that wanted for head
(defun returnHeadVariables (l1 l2)
    (if (not (null l2))
        (append (list (findMultipleIndex l1 (car l2))) (returnHeadVariables l1 (cdr l2)))))


(defun samevarsHas (samevars e1)
    (if (not (null samevars))
        (if (equal (caar samevars) e1) (cadar samevars)
            (samevarsHas (cdr samevars) e1))
        nil))

(defun consResultElements (resultelem samevars headparam n)
    (if (not (null headparam))
        (let ((x (samevarsHas samevars n)))
            (if (null x)
                (append (list (car headparam)) (consResultElements resultelem samevars (cdr headparam) (+ n 1)))
                (append (list (findnthindex resultelem x)) (consResultElements resultelem samevars (cdr headparam) (+ n 1)))
        ))))

(defun constructHeadVariables (headparams samevars resultBody)
    (if (not (null resultBody))
    ;(progn (print "33333333333") (print resultBody) (print "33333333333")
        (append (list (consResultElements (car resultBody) samevars headparams 0)) 
            (constructHeadVariables headparams samevars (cdr resultBody)))));)


;; headparams must be head of body but it must be assigned from query!
(defun calculateIntersectionElements (l headParams)
    (setq result (findAllIntersections l))
    (let ((samevars (findInterElementIndex headParams (car result) 0)))
        (if (null samevars) result
            (append (list headparams) (list (constructHeadVariables headparams samevars (cadr result))))
            

)))

(defun isEqual (l1 l2)
    (cond
        ((and (null l1) (null l2)) t)
        ((equal (car l1) (car l2)) (isEqual (cdr l1) (cdr l2)))
        (t nil)))

;; If query has no variable match with fact.
(defun factNoVariable (fact query)
    (cond
        ((null fact) (list nil))
        (t (let ( (var (factNoVariable (cdr fact) (cdr query))))
                (if (null var) nil
                    (if (or (equal (car fact) (car query)) (isVariable (car fact)))
                        (append (list (car query)) var) nil))))))

;; for query
;; l => variable list
;; var => variable in query
(defun listHasVariable (l var)
    (cond
        ((null l) nil)
        ((equal (car l) nil) nil)
        ((and (typep (car l) 'list) (equal (caar l) var)) (cadar l))
        (t (listHasVariable (cdr l) var))))

;;for fact
(defun listHasVariablef (l var)
    (cond
        ((null l) nil)
        ((equal (car l) nil) nil)
        ((and (typep (car l) 'list) (equal (cadar l) var)) (caar l))
        (t (listHasVariablef (cdr l) var))))

(defun updateVar (index new var)
    (if (or (not (null var)) (not (null (car var))))
        (if (equal index (cadar var))
            (append (list (list (caar var) new)) (updateVar index new (cdr var)))
            (append (list (car var)) (updateVar index new (cdr var))))))

;; Finds query result from fact
(defun factVariable (fact query)
    (cond
        ((null fact) (list nil)) ;base case
        (t (let ((var (factVariable (cdr fact) (cdr query)))) ;go to the end
                (if (null var) nil
                    (cond
                        ((and (not (isVariable (car fact))) (not (isVariable (car query)))) ; If both is not variable
                            (if (equal (car fact) (car query))
                                (append (list (car query)) var) nil))
                        
                        ((and (not (isVariable (car fact))) (isVariable (car query))) ; fact is not variable , query is.
                            (let ((has (listHasVariable var (car query))))
                                ;(print has)
                                (if (null has)
                                    (append (list (list (car query) (car fact))) var)
                                    (if (not (equal has (car fact)))
                                        (if (isVariable has)
                                            (updateVar has (car fact) var)
                                            nil
                                        )
                                        var
                                    ))))
                        ((and (isVariable (car fact)) (not (isVariable (car query)))) ;;fact is variable, query is not 
                            (let ((has (listHasVariablef var (car fact))))
                                (if (null has)
                                    (append (list (list (car query) (car fact))) var)
                                    (if (not (equal has (car query)))
                                        nil
                                        (append (list (list (car query) (car fact))) var)
                                    ))))
                        ((and (isVariable (car fact)) (isVariable (car query))) ;;both is variable
                            (let ((has1 (listHasVariable var (car query))) (has2 (listHasVariablef var (car fact))))
                                (cond
                                    ((and (null has1) (null has2))
                                        (append (list (list (car query) (car fact))) var))
                                    ((and (not (null has1)) (not (null has2)))
                                        (cond 
                                            ((or (not (equal has1 (car fact))) (not (equal has2 (car query)))) nil)
                                            (t var)))
                                    ((and (null has1) (not (null has2)))
                                        (append (list (list (car query) has2)) var))
                                    
                                    ((and (not (null has1)) (null has2)) var))
                    ))))))))




; This function using in matching body part with head of clause list.
; It compares the parameter list of head and provided body.
; Basically this function just compares the parameters
(defun parameterComparator (parameter1 parameter2)
    (cond
        ((not (equal (length parameter1) (length parameter2))) nil)
        ((null parameter1) t)
        ((or (isVariable (car parameter1)) (isVariable (car parameter2)))
            (parameterComparator (cdr parameter1) (cdr parameter2)))
        ((equal (car parameter1) (car parameter2)) 
            (parameterComparator (cdr parameter1) (cdr parameter2)))
        (t nil)))


;; remove first founded element from given list
(defun removeElement (e1 l)
    (if (not (null l))
        (if (equal e1 (car l))
            (cdr l)
            (cons (car l) (removeElement e1 (cdr l))))))


; using in constructHeadResult function
(defun findInFactList (fact query)
        (if (not (null fact))
            (cond
                ((equal (car fact) query) query)
                ((and (typep (car fact) 'list) (equal (caar fact) query))
                (car fact))
                (t (findInFactList (cdr fact) query)))))


;; It takes output of factVariable and query variables
;; combines and constructs a final result of query
(defun constructHeadResult (fact query)
    (if (not (null query))
        (progn
            (let ((x (findInFactList fact (car query))))
                (if (typep x 'list)
                    (cond 
                        ((isVariable (cadr x))
                            (if (not (isVariable (car query)))
                                (append (list (car x)) (constructHeadResult (removeElement x fact) (cdr query)))
                                (append (list (car x)) (constructHeadResult fact (cdr query)))))
                        (t 
                            (if (not (isVariable (car query)))
                                (append (list (cadr x)) (constructHeadResult (removeElement x fact) (cdr query)))
                                (append (list (cadr x)) (constructHeadResult fact (cdr query))))))
                    (append (list x) (constructHeadResult (removeElement x fact) (cdr query)))
                )))))


; Creates a list with using  <term list>  of predicate and query
; It returns a list with first element is variable, second element is value
; Ex;
    ; (setq test1 (list "yusuf" "can" 1 2 3))
    ; (setq test2 (list "X" "Y" 1 "Z" "yusufcankan"))
    ; (print (unificationElements test1 test2))
    ; output: (("X" "yusuf") ("Y" "can") ("Z" 2))
;predicate1 => query
(defun unificationElements (predicate1 predicate2)
    (if (not (null predicate1))
        (cond 
            ( (isVariable (car predicate2))
                (append (list (list (car predicate2) (car predicate1))) 
                (unificationElements (cdr predicate1) (cdr predicate2))))
            (t (unificationElements (cdr predicate1) (cdr predicate2)))
)))



; This fuction takes head of 
; matched clause and query
; and combines their head in
; proper way
;predicate1 => query
; It does unification
(defun headQueryCombine (predicate1 predicate2)
    (if (not (null predicate1))
        (cond 
            ((isVariable (car predicate2))
                (append (list (car predicate1))
                (headQueryCombine (cdr predicate1) (cdr predicate2))))
            (t (append (list (car predicate2)) 
            (headQueryCombine (cdr predicate1) (cdr predicate2))))
)))


(defun doesHaveVariable (l)
    (cond 
        ((and (not (null l)) (isVariable (car l))) t)
        ((null l) nil)
        (t (doesHaveVariable (cdr l)))))


;; Finds all clauselists matched with given predicatename.
(defun findForHead (query clauseList)
    (if (not (null clauseList))
        (if (and (equal (car query) (caaar clauseList)) (equal (length (cadr query)) (length (cadaar clauseList)))
            (parameterComparator (cadr query) (cadaar clauseList)))
                (append (list (car clauseList)) (findForHead query (cdr clauseList)))
            (findForHead query (cdr clauseList)))))

;; finds matched clauses and sends head
(defun calculateHead (query clauseList)
    ;; Finds all proper querys in clause list
    (let ((matches (findForHead query clauseList)))
        (if (null matches) nil ;; if not matched clause return nil.
            (head query matches clauseList))))


;; It takes one predicate in body and proofs it.
;; If predicate is fact it returns the predicate variable list
;; If it is not fact, it will send the body part and proof its body first.
(defun head (query matches clauseList)
    (if (not (null matches)) ;; iterate all founded clauses
         (cond
            ((null (cadar matches)) ;If founded clause is fact
                (let ((x (factVariable (cadaar matches) (cadr query))));; Find query result from founded fact
                    (if (null x)
                        (head query (cdr matches) clauseList) ;; If not true 
                        ;If clause is true construct list and append to result
                        
                        (append (list (constructHeadResult (reverse (cdr (reverse x))) (cadr query))) 
                            (head query (cdr matches) clauseList))))
            )
            (t ;If founded clause is not fact
                (let ((x (unificationElements (cadr query) (cadaar matches)))) ;; variable value match list
                    (if (null x) ;;If clause head does not have any variable
                    
                        (if (null (body clauseList x (cadar matches) (cadr query)));; If body proof is wrong
                            (head query (cdr matches) clauseList)
                            (append (list (cadaar matches)) (head query (cdr matches) clauseList))
                        )
                                    ;;If clause head have variable
                        (progn
                            ;; combine query and head params for body
                            (let ((params (headQueryCombine (cadr query) (cadaar matches))))

                                ;; params stores combination variable for head
                                ;; X 1 Y (head params query) => 3 A B (head params clauese)  ====> 3 1 Y
                                ;; this must go to body

                                (let ((bodyResult (body clauseList x (cadar matches) params)))
                                    (if (null bodyResult)
                                        (head query (cdr matches) clauseList)
                                        (append (cadr bodyResult) (head query (cdr matches) clauseList))
                                ))))))))))






(defun constructBody (bodyList clauseList)
    (if (not (null bodyList))
        (append (list (list (cadar bodyList) (calculateHead (car bodyList) clauseList)))
            (constructBody (cdr bodyList) clauseList))
        ))

;; Assignes given variable list to body list
(defun assignVariablestoBody (variableList bodyList)
    (if (not (null bodyList))
        (append (list (assignVariables variableList (car bodyList))) 
            (assignVariablestoBody variableList (cdr bodyList)))))

;; Assignes given variable list to given body
(defun assignVariables (variableList body)
    (if (not (null variableList))
        (assignVariables (cdr variableList) (list (car body) (assign (car variableList) (cadr body))))
        body))

;;;;;; @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@GUNCELLLE BOZUK
;; place the given value instead of variable. 
;; ( variable=(variable,value) )
;; EX:
;; body: ("X" "AS" "X" "x" "Y" 1 "Z" "yusufcankan") 
;; variable: ("X" 1)
;; output: (1 "AS" 1 "x" "Y" 1 "Z" "yusufcankan") 
(defun assign (variable body)
    (cond
        ((not (null body))
            (if (equal (car body) (car variable))
                    (append (list (cadr variable)) (assign variable (cdr body)))
                (append (list (car body)) (assign variable (cdr body)))))))


(defun ifQueryNull (Qlist)
    (if (not (null Qlist))
        (if (null (cadar Qlist)) nil
            (ifQueryNull (cdr Qlist)))
        t))

;; body implementation of clause
;; This function takes a body part of clause and proofs it
;; with using given query parameter lists.
; clauseList => list of clauses
; variableList => matched variables  (unification element function from head)
;   between query argumens and arguments 
;   from previous body. (UNIFICATION)
; bodyList => list of elements in body. This part will be prooved.
; assignedHead => Constructed variables between query and clause head (headQueryCombine function in head)
(defun body (clauseList variableList bodyList assignedHead)
    (let ((bodyl (assignVariablestoBody variableList bodyList)))
        (if (ifQueryNull (constructBody bodyl clauseList)) ; If one of the body is not prooved
            (let ( ( a (calculateIntersectionElements (constructBody bodyl clauseList) assignedHead)))
                (if (null (cadr a)) nil a))
            nil)          
))

(defun variableHas (l)
    (if (not (null l))
        (if (isVariable (car l)) t (variableHas (cdr l)))
        nil))

;;It looks if query has variable.
(defun variableFound (query)
    (if (not (null query))
        (if (variableHas (cadar query)) t (variableFound (cdr query)))
    ))

(defun calculateQueryList (queries clauseList)

    (if (not (null queries))
        (progn
            (if (variableFound (cadar queries))
                (let ((result (body clauseList nil (cadar queries) nil)))
                    (if (null result)
                        (write_file "False")
                        (write_file result)))
                (if (null (body clauseList nil (cadar queries) nil) )
                    (write_file "False")
                    (write_file "True")
                ))
            (calculateQueryList (cdr queries) clauseList)
        )))

(defun main ()
    (clear_file)
    (setq input (read_file "input.txt"))
    (setq clauseList (constructClauseList input))
    (sort_my_list clauseList)
    (setq queries (constructQueries input))

    (calculateQueryList queries clauseList)

)

(main)
