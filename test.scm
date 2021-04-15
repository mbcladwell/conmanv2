
;; (pretty-print (get-summaries "7" "10"))
;; https://pubmed.ncbi.nlm.nih.gov/32866150/
;;  https://pubmed.ncbi.nlm.nih.gov/33085113/

;;  (pretty-print (find-fl-aoi "McPhaul MJ"))   

(use-modules (rnrs bytevectors)
	     (dbi dbi)
	     (srfi srfi-19))

https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=96+multi+well+OR+high-throughput+screening+assay+(2021%2F03%2f29 [epdat])&retmax=3
<Id>33782712</Id>
<Id>33781022</Id>

https://pubmed.ncbi.nlm.nih.gov/33807492

(define myurl "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id=33782712,33781022,33085113,32866150")
 (define all-summaries   (receive (response-status response-body)
			     (http-request myurl) response-body))

(define b (find-occurences-in-string "<DocSum>" all-summaries))
(define c (map (lambda (x) (substring all-summaries (car x) (cdr x))) b))
(define d (map get-id-authors c))

(define (get-todays-batchid)
  (let*((bid-pre (date->string  (current-date) "~Y~m~d"))
	;;(sql (string-append "SELECT DISTINCT batchid FROM conman WHERE  batchid LIKE '" bid-pre "%'"))
	(sql (string-append "SELECT DISTINCT batchid FROM conman WHERE  batchid LIKE '20210414%'"))	
	(ciccio (dbi-open "mysql" "plapan_conman_ad:welcome:plapan_conman:tcp:192.254.187.215:3306"))
	(dummy (dbi-query ciccio sql))
	(ret (dbi-get_row ciccio))
   	(dummy (dbi-close ciccio)))
    (number->string (assoc-ref ret "batchid"))))


(let*((bid-pre (date->string  (current-date) "~Y~m~d"))
	;;(sql (string-append "SELECT DISTINCT batchid FROM conman WHERE  batchid LIKE '" bid-pre "%'"))
	(sql (string-append " INSERT INTO ref(pmid) VALUES ('777584')"))	
	(ciccio (dbi-open "mysql" "plapan_conman_ad:welcome:plapan_conman:tcp:192.254.187.215:3306"))
	(dummy (dbi-query ciccio sql))
   	(dummy (dbi-close ciccio)))
    #f)
