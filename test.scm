#!/gnu/store/q8brh7j5mwy0hbrly6hjb1m3wwndxqc8-guile-3.0.5/bin/guile \
-e main -s
!#

 (add-to-load-path "/home/mbc/projects")

(use-modules (rnrs bytevectors)
	     (dbi dbi)
	     (srfi srfi-19))


(define myurl "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id=33782712,33781022,33085113,32866150")
;; (define all-summaries   (receive (response-status response-body)
;;			     (http-request myurl) response-body))

;;(define b (find-occurences-in-string "<DocSum>" all-summaries))
;;(define c (map (lambda (x) (substring all-summaries (car x) (cdr x))) b))
;;(define d (map get-id-authors c))

(define (get-todays-batchid)
  (let*((bid-pre (date->string  (current-date) "~Y~m~d"))
	;;(sql (string-append "SELECT DISTINCT batchid FROM conman WHERE  batchid LIKE '" bid-pre "%'"))
	(sql (string-append "SELECT DISTINCT batchid FROM conman WHERE  batchid LIKE '20210414%'"))	
	(ciccio (dbi-open "mysql" "plapan_conman_ad:welcome:plapan_conman:tcp:192.254.187.215:3306"))
	(dummy (dbi-query ciccio sql))
	(ret (dbi-get_row ciccio))
   	(dummy (dbi-close ciccio)))
    (number->string (assoc-ref ret "batchid"))))

(define (main args)
  (let*((bid-pre (date->string  (current-date) "~Y~m~d"))
	;;(sql (string-append "SELECT DISTINCT batchid FROM conman WHERE  batchid LIKE '" bid-pre "%'"))
	(sql (string-append " INSERT INTO ref(pmid) VALUES ('777584')"))	
	(ciccio (dbi-open "postgresql" "postgres:postgres:conman:tcp:192.168.1.17:5432"))

	(dummy (dbi-query ciccio sql))
   	(dummy (dbi-close ciccio)))
    #f))
