#!/usr/bin/guile \
-e main -s
!#
;; Retrieve rows from mysql and process each row by sending a custom email
 (add-to-load-path "/home/mbc/projects")

;; (define-module (conmanv2 cemail)
;;   #:export (get-rows
;; 	    ))

(use-modules  (ice-9 regex) ;;list-matches
	      (ice-9 textual-ports)
	      (ice-9 pretty-print)
	      (dbi dbi)
	      (srfi srfi-19) ;; date-time
	      )

(define (get-todays-batchid)
  (let*((bid-pre (date->string  (current-date) "~Y~m~d"))
	;;(sql (string-append "SELECT DISTINCT batchid FROM conman WHERE  batchid LIKE '" bid-pre "%'"))
	(sql (string-append "SELECT DISTINCT batchid FROM conman WHERE  batchid LIKE '" bid-pre "%'"))	
	(ciccio (dbi-open "mysql" "plapan_conman_ad:welcome:plapan_conman:tcp:192.254.187.215:3306"))
	(dummy (dbi-query ciccio sql))
	(ret (dbi-get_row ciccio))
	(dummy (dbi-close ciccio))
	)
 ;;   ret))
    (number->string (assoc-ref ret "batchid"))))

;; (get-todays-batchid)


(define (get-rows batchid)
  (let*((sql (string-append "SELECT DISTINCT email, journal, title, firstn FROM ref, conman WHERE email NOT LIKE 'null' AND ref.pmid=conman.pmid AND batchid = '" batchid "'"))
	(ciccio (dbi-open "mysql" "plapan_conman_ad:welcome:plapan_conman:tcp:192.254.187.215:3306"))
	(holder '())
	(dummy (dbi-query ciccio sql))
	(ret (dbi-get_row ciccio))
	(dummy (while (not (equal? ret #f))
	 	 (set! holder (append holder (list ret )))
	 	 (set! ret (dbi-get_row ciccio))
	 	 ))
	(dummy (dbi-close ciccio))
	)
    holder))


(define (fname-from-email email)
  (let* ((at-loc (string-index email #\@))
	 (a (substring email 0 at-loc))
	 (b (string-index a #\.))
	 (c (if b (string-capitalize! (substring a 0 b))  #f)))
    c))


(define (get-rand-file-name pre suff)
  (string-append "tmp/" pre "-" (number->string (random 10000000000000000000000)) "." suff))


(define (send-custom-email item)
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
	 (email (assoc-ref item "email"))
	 (first-name (if (fname-from-email email) (fname-from-email email)(assoc-ref item "firstn")))
	 (html-composite (string-append str1 first-name  str2 (assoc-ref item "title") str3 (assoc-ref item "journal") str4 ))
;;	 (dummy (system "rm tmp/rnd*.txt"))
;;	 (dummy (system "rm tmp/rnd*.html"))
	 (html-file-name (get-rand-file-name "rnd" "html"))
	 (p  (open-output-file html-file-name))
	 (dummy (begin
		  (put-string p html-composite )
		  (force-output p)))
	 (str5 "Dear ")
	 (str6 ",\nYour recent article entitled \"")	 
	 (str7 "\" in the journal ")
	 (str8 " suggests you might benefit from our product.\nVisit Laboratory Automation Solutions at www.labsolns.com and learn how LIMS*Nucleus can help you.\n\nLIMS*Nucleus can:\n\n-Reformat plates - four 96 well plates into a 384 well plate; four 384 well plates into a 1536 well plate\n-Associate assay data with plate sets\n-Identify hits scoring in assays using included algorithms - or write your own\n-Export annotated data\n-Generate worklists for liquid handling robots\n-Rearray hits into a smaller collection of plates\n-Track samples\n\nLIMS*Nucleus can serve as the core of a LIMS system.\nPrototype algorithms, dashboards, visualizations with R/Shiny.\nEvaluate the software by visiting www.labsolns.com/software/evaluate/\n\nFor more information contact info@labsolns.com\n\nThank You!")
	 (txt-composite (string-append str5 first-name str6 (assoc-ref item "title") str7 (assoc-ref item "journal") str8 ))
	 (txt-file-name (get-rand-file-name "rnd" "txt"))
	 (p2  (open-output-file txt-file-name))
	 (dummy (begin
		  (put-string p2 txt-composite )
		  (force-output p2)))
	 (smtp-command (string-append "/home/mbc/bin/smtp-cli --host mail.labsolns.com:587 --subject 'Multi-well plate management software' --enable-auth --user info@labsolns.com --password EKhD8GB48F8wFalt --from info@labsolns.com --to " (assoc-ref item "email") " --bcc info@labsolns.com --body-plain /home/mbc/projects/conmanv2/" txt-file-name " --body-html /home/mbc/projects/conmanv2/" html-file-name " --attach-inline /home/mbc/projects/conmanv2/tmp/las.png"))
	;; (dummy (system smtp-command))
	 )
 
  smtp-command
  ))

(define (process-all-rows lst)
  (if (null? (cdr lst))
     (send-custom-email (car lst))
      (begin
	(send-custom-email (car lst))
 	(process-all-rows (cdr lst)))))


(define (main args)
  (let* (
	 (bid (get-todays-batchid))
	 (all-rows (get-rows bid))
	 (dummy (process-all-rows all-rows))	 
	 )
    #f))
  

 ;;(main "cemail.scm")
 
;; (pretty-print (get-rows "202104140227"))
