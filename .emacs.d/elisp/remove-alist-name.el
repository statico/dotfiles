; remove an element from an associative list (alist) 
(defun remove-alist-name (name alist)
  "Removes element whose car is NAME from ALIST."
  (cond ((equal name (car (car alist)))	  ; found name
         (cdr alist))
        ((null alist)		; end of list (termination cond)
         nil)
        (t
         (cons (car alist)	; first of alist plus rest w/ recursion
               (remove-alist-name name (cdr alist))))))
