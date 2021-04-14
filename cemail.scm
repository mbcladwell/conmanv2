(define-module (conmanv2 cemail)
  #:export (get-rows
	    any-not-false?
	    to-regular-char
	    ))


(use-modules  (ice-9 regex) ;;list-matches
	       (ice-9 textual-ports)
	      )


(define (get-rows batchid)
  (let*((sql (string-append "SELECT DISTINCT email, journal, title, firstn FROM ref, conman WHERE email NOT LIKE 'null' AND ref.pmid=conman.pmid AND batchid = '" batchid "'"))
	(ciccio (dbi-open "mysql" "plapan_conman_ad:welcome:plapan_conman:tcp:192.254.187.215:3306"))
	(holder '())
	(dummy (dbi-query ciccio sql))
	(ret (dbi-get_row ciccio))
        (holder (append holder (list ret)))
	(dummy (while (not (equal? ret #f))
	 	 (set! holder (append holder (list ret )))
	 	 (set! ret (dbi-get_row ciccio))
	 	 ))
	(dummy (dbi-close ciccio))
	)
    holder))

;; (pretty-print (get-rows "202104140227"))

(define an-item '(("email" . "Leen.Delang@kuleuven.be")
  ("journal" . "Microorganisms")
  ("title"
   .
   "Repurposing Drugs for Mayaro Virus: Identification of EIDD-1931, Favipiravir and Suramin as Mayaro Virus Inhibitors.")
  ("firstn" . "Rana")))

(assoc-ref an-item "email")

(define (get-rand-file-name pre suff)
  (string-append "tmp/" pre "-" (number->string (random 10000000000000000000000)) "." suff))


(define (make-custom-email item)
  ;; an item is a list with needed elements from query
  ;; (("email" . "Leen.Delang@kuleuven.be")
  ;;  ("journal" . "Microorganisms")
  ;;  ("title" . "Repurposing Drugs for Mayaro Virus: Identification.... Inhibitors.")
  ;;  ("firstn" . "Rana"))
  (let* ((str1 "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \"http://www.w3.org/TR/REC-html40/loose.dtd\">\n<html><head><title></title></head><body style=\"font-family:Arial;font-size:14px\">\n<p>Dear ")
	 (str2 ",<br><br>\nYour recent article entitled \"")
	 (str3 "\" in the journal <i>")
	 (str4 "</i> suggests you might benefit from our product.<br>\nVisit <a href=\"http://www.labsolns.com\">Laboratory Automation Solutions</a> and learn how LIMS*Nucleus can help you.<br><br>\nLIMS*Nucleus can:<br><br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Reformat plates - four 96 well plates into a 384 well plate; four 384 well plates into a 1536 well plate<br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Associate assay data with plate sets<br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Identify hits scoring in assays using included algorithms - or write your own<br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Export annotated data<br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Generate worklists for liquid handling robots<br>\n&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Rearray hits into a smaller collection of plates<br>\n
&nbsp; &nbsp; &nbsp; &nbsp; *&nbsp; &nbsp;Track samples<br><br>\nLIMS*Nucleus can serve as the core of a LIMS system.<br>\nPrototype algorithms, dashboards, visualizations with R/Shiny.<br>\nEvaluate the software by visiting <a href=\"http://labsolns.com/software/evaluate/\">labsolns.com</a><br><br>\nThanks<br><br><a href=\"mailto:info@labsolns.com\">info@labsolns.com</a><br><br>\n<img src=\"cid:las.png\" style=\"width: 175px; height: 62px;\">\n</body></html>")
	 (html-composite (string-append str1 (assoc-ref an-item "firstn") str2 (assoc-ref an-item "title") str3 (assoc-ref an-item "journal") str4 ))
	 (dummy (system "rm tmp/rnd*.txt"))
	 (dummy (system "rm tmp/rnd*.html"))
	 (html-file-name (get-rand-file-name "rnd" "html"))
	 (p  (open-output-file html-file-name))
	 (dummy (begin
		  (put-string p html-composite )
		  (force-output p)))
	 (str5 "Dear ")
	 (str6 ",\nYour recent article entitled \"")	 
	 (str7 "\" in the journal ")
	 (str8 "suggests you might benefit from our product.\nVisit Laboratory Automation Solutions at www.labsolns.com and learn how LIMS*Nucleus can help you.\n\nLIMS*Nucleus can:\n\n-Reformat plates - four 96 well plates into a 384 well plate; four 384 well plates into a 1536 well plate\n
-Associate assay data with plate sets\n
-Identify hits scoring in assays using included algorithms - or write your own\n
-Export annotated data\n
-Generate worklists for liquid handling robots\n
-Rearray hits into a smaller collection of plates\n
-Track samples\n\n

LIMS*Nucleus can serve as the core of a LIMS system.\n
Prototype algorithms, dashboards, visualizations with R/Shiny.\n
Evaluate the software by visiting www.labsolns.com/software/evaluate/\n\nFor more information contact info@labsolns.com\n\nThank You!")
	 (txt-composite (string-append str5 (assoc-ref an-item "firstn") str6 (assoc-ref an-item "title") str7 (assoc-ref an-item "journal") str8 ))
	 (txt-file-name (get-rand-file-name "rnd" "txt"))
	 (p2  (open-output-file txt-file-name))
	 (dummy (begin
		  (put-string p2 txt-composite )
		  (force-output p2)))
	 (smtp-command (string-append "./bin/smtp-cli --verbose --host mail.labsolns.com:587 --subject LIMS development --enable-auth --user info@labsolns.com --password EKhD8GB48F8wFalt --from info@labsolns.com --to " (assoc-ref an-item "email") " --body-plain " txt-file-name " --body-html " html-file-name " --attach-inline tmp/las.png"))
	 )
    

  smtp-command
  ))



(pretty-print (make-custom-email an-item))




 
