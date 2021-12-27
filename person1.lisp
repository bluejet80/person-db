;; Define the inital variables

(defvar *people-db* nil)
(defvar *temp-list* nil)
(defparameter *file-path* "~/lisp_prog/Person_DB/people.db")

;; Function to setup the prompt

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun make-init (name sex age address phone uniqueness)
  (setf *temp-list*
	(list :name name
	      :sex sex
	      :age age
	      :address address
	      :phone phone
	      :uniqueness uniqueness)))

(defun add-rec (rec) (push rec *people-db*))


(defun get-input ()
  (format t "~%~%")
  (make-init
   (prompt-read "Name")
   (prompt-read "Sex")   
   (prompt-read "Age")
   (get-address)
   (prompt-read "Phone")
   (prompt-read "Uniqueness")))

(defun get-address()
  (list :street (prompt-read "Street")
	:city (prompt-read "City")
        :state (prompt-read "State[XY]")
        :zip (prompt-read "Zip Code")))

(defun show-rec (rec)
	  (if (listp (car rec))
	      (print-db rec)
	      (print-temp rec)))

(defun add-person ()
    (get-input)
    (format t "~%~%")
    (show-rec *temp-list*)
    (let ((resp nil))
      (setf resp (prompt-read "Does the above look correct?[y/n]"))
      (if (equal resp "y")
	  (add-rec *temp-list*)
	  (add-person)))
    (let ((resp nil))
      (setf resp (prompt-read "Do you wish to add another person?[y/n]"))
      (if (equal resp "y")
	  (add-person))))

(defun save-db ()
  (with-open-file (out *file-path*
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (print *people-db* out))))

(defun load-db ()
  (with-open-file (in *file-path*)
    (with-standard-io-syntax
      (setf *people-db* (read in)))))



(defun print-db (rec)
      (loop for item in rec
	    do (format t "================-----------~%")
	       (format t "Name: ~a" (getf item :NAME))	       
	       (format t "  SEX: ~a~%" (getf item :SEX))
	       (format t "AGE: ~a~%" (getf item :AGE))
       	       (format t "PHONE: ~a~%" (getf item :PHONE))
	       (format t "ADDRESS: ~{~a~%~9t~a, ~a ~a~}~%"
			  (remove-if-not #'stringp (getf item :ADDRESS)))
	       (format t "~%")
	       (format t "UNIQUENESS: ~a~%" (getf item :UNIQUENESS))
	       (format t "~%")))


(defun print-temp (rec)
  (format t "================-----------~%")
  (format t "Name: ~a" (getf rec :NAME))
  (format t "   SEX: ~a~%" (getf rec :SEX))
  (format t "AGE: ~a~%" (getf rec :AGE))
  (format t "PHONE: ~a~%" (getf rec :PHONE))
  (format t "ADDRESS: ~{~a~%~9t~a, ~a ~a~}~%"
	  (remove-if-not #'stringp (getf rec :ADDRESS)))
  (format t "~%")
  (format t "UNIQUENESS: ~a~%" (getf rec :UNIQUENESS))
  (format t "~%"))
