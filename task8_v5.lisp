(defun sdd (automat)
(cond ((null automat) NIL)
((set1(cons (cadar automat) (sdd (cdr automat)))))
))

(defun member1 (elem L) 
(cond ((null L) NIL) 
((equal (car L) elem) L) 
(T (member1 elem (cdr L))))) 

(defun inclus (L1 L2) 
(cond ((null L1) T) 
((member1 (car L1) L2) (inclus (cdr L1) L2)) 
(T NIL))) 

(defun set2 (l)
(cond
((null l) NIL)
((inclus (list(car l)) (cdr l)) (set1 (cdr l)))
((cons (car l) (set1 (cdr l))))))

(defun delNIL (L)
(cond ((null L) NIL)
((eql (caddar L) NIL) (delNIL (cdr L)))
((cons (car L) (delNIL (cdr L))))))

(defun set1 (l)
(cond
((null l) NIL)
((member (car l) (cdr l)) (set1 (cdr l)))
(T (cons (car l) (set1 (cdr l))))))

(defun checkIdenticalMarksNode (A B SD)
(cond ((null SD) NIL)
((and (eql A (caar SD)) (eql B (cadar SD)))T)
((checkIdenticalMarksNode A B (cdr SD)))))

(defun checkNonDeterministicAutomat (automat)
(cond ((null automat) NIL)
((checkIdenticalMarksNode (caar automat) (cadar automat) (cdr automat)) T)
((checkNonDeterministicAutomat (cdr automat)))))

(defun getIdenticalMarksNode (start letter L automat)
(cond ((null automat) L)
((and (eql start (caar automat)) (eql letter (cadar automat))) (getIdenticalMarksNode start letter (cons (caddar automat) L) (cdr automat)))
 ((getIdenticalMarksNode start letter L (cdr automat)))))

(defun getStatesH (alp start automat)
(cond ((or(null alp) (null automat)) NIL)
((cons (cons (list start) (cons (car alp) (cons (getIdenticalMarksNode  start  (car alp) NIL automat) NIL))) (getStatesH(cdr alp) start automat))))) ; добавил list и reverse

(defun getListStates (listEdgesWithNewStates) ; состояния ищем dre --> searchStates
(cond ((null listEdgesWithNewStates) NIL)
((set1 (cons (caddar listEdgesWithNewStates) (getListStates (cdr listEdgesWithNewStates)))))))

(defun getEdgesState (automat letter state) ; одна буква - один элемент ; находим с  буквой и состоянием кусок графа aswe --> getEdgesState
(cond ((null automat) NIL)
((and (eql (caar automat) state) (eql (cadar automat) letter)) (cons (car automat) (getEdgesState (cdr automat) letter state))) ; надо идти по всему циклу до конца, а не выдавать первый 
((getEdgesState (cdr automat) letter state))))

(defun getEdgesEachState (automat letter listSt) ; одна буква. цикл по состояниям (b s) serw --> getEdgesEachState
(cond ((null listSt) NIL)
((eql (getEdgesState automat letter (car listSt)) NIL)	(getEdgesEachState automat letter (cdr listSt)))
((append (getEdgesState automat letter (car listSt)) (getEdgesEachState automat letter (cdr listSt)))))) ;поменял c cons на append

(defun getNewEdgesForLetter (automat letter listSt)  ;sdf --> getNewEdgesForLetter
;(cond ((eql (setq temp (getEdgesEachState automat letter listSt)) NIL) NIL) 
(cons listSt (cons letter (list(getListStates (getEdgesEachState automat letter listSt))))))

(defun getNewEdgesForNode (automat alp listSt) ; цикл по одной букве  ; rte --> getNewRibs
(cond ((null alp)  NIL)
((DelNIL (cons (getNewEdgesForLetter automat (car alp) listSt)  (getNewEdgesForNode automat (cdr alp) listSt))))
))

(defun mas1 (L1 L2)
(cond ((null (cdr L2)) (not(set-exclusive-or (caddr L1) (caddar L2))))
((or(not(set-exclusive-or (caddr L1) (caddar L2))) (mas1  L1 (cdr L2))) T)))

(defun mas23 (L1 L2)
(cond ((null (cdr L2)) (not(set-exclusive-or L1 (car L2))))
((or(not(set-exclusive-or L1 (car L2))) (mas23  L1 (cdr L2))) T)))

(defun mas (L1 L2)
(cond ((null L1) NIL)
((mas1 (car L1) L2) (mas (cdr L1) L2))
((cons (car L1) (mas (cdr L1) L2)))))

(defun compare1 (L1 L2 L3 temp)
(cond ((eql (length (remove NIL temp)) 0)  L3)
((append temp L3)))) ;чтобы прочитались все состояния, которые остались, а не только последнее

(defun compareNewWithOldStates (L1 L2 L3) 
(compare1 L1 L2 L3 (mas L1 L2)))

(defun getDet1 (temp automat L2 tailL1 alp)
(getDeterministicAutomat (compareNewWithOldStates temp L2 tailL1) automat alp (append temp L2)))

(defun getDeterministicAutomat (L1 automat alp L2)
(cond ((null L1) L2)
((eql (car L1) NIL) (getDeterministicAutomat (cdr L1) automat alp L2))
((getDet1 (getNewEdgesForNode automat alp (caddar L1)) automat L2 (cdr L1) alp))))

(defun sder (L2 acc)
(cond ((null  L2) acc)
((and(member 'S (caddar L2)) (not(mas23 (caddar L2) acc))) (sder (cdr L2) (cons (set1 (caddar L2))  acc)))
((sder (cdr L2) acc))
))

(defun check2 (state L2)
(cond ((null L2) state)
((not(set-exclusive-or state (car L2))) (car L2))
((check2 state (cdr L2)))
))

(defun mur3 (L1 state er flag L2)
(cond ((eql er 0))
((format t "|")))
(format T "~(~a~)" (cadar L1)) 
(cond ((atom (caddar L1)) (write  (caddar L1)))
((format T "~{~a~}" (set1 (check2  (caddar L1) L2)))))
;((format T "~{~a~}"  (set1 (caddar L1)) L2)))
(mur2 (cdr L1) state 1 flag L2))

(defun mur2 (L1 state er flag L2)
(cond ((null L1))
((cond ((and (not (atom state)) (= (length state) 1) (eql  (car state) 'H)) (eql (car state) (caaar L1)))
((eql state (caar L1)))) (mur3 L1 state er flag L2))
((mur2 (cdr L1) state er flag L2))))

(defun mur1 (L1 state flag L2)
(cond ((atom state) (write state))
((format T "~{~a~}" (set1 state))))
(format t "=")
(mur2 L1 state 0 flag L2)
(cond ((and flag  (eql state 'S)) (not(format t ";")))
((cond ((or (atom state) (= (length state) 1) (not(member 'S state))) (not(format t ";")))
((not(format t ";")))))))

(defun print3 (L1)
(print "заключительные состояния:")
(write (sder L1 NIL)))

(defun print1 (s3 flag L1)
(cond (s3 (cond (flag (print "заключительные состояния:") (write '((S)))) ((print3 L1))))))

(defun mur (L1 sddd flag L2 s3)
(cond ((null sddd))
((mur1 L1 (car sddd) flag L2) (mur L1 (cdr sddd) flag L2 NIL)))
;(cond (s3 (print "заключительные состояния: ")
;(write (sder L1 NIL)))))
(print1 s3 flag L1))

(defun getFirstMarks (determenisticAutomat) 
(cond ((null determenisticAutomat) NIL)
((cons (caar determenisticAutomat) (getFirstMarks (cdr determenisticAutomat))))))

(defun determAut (alp automat  statesH)
(reverse(getDeterministicAutomat statesH automat alp statesH)))

(defun determAut1 (L)
(mur L (set2 (getFirstMarks L)) NIL (set2 (getFirstMarks L)) T ))


(defun getGrammar (automat)
(cond ((checkNonDeterministicAutomat automat) 
(determAut1 (determAut (sdd automat) automat (delNIL(getStatesH (sdd automat) 'H automat)))))
((mur automat (set1(getFirstMarks automat)) T (set1(getFirstMarks automat)) T))))

(getGrammar (read))
