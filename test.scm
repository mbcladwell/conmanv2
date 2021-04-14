
;; (pretty-print (get-summaries "7" "10"))
;; https://pubmed.ncbi.nlm.nih.gov/32866150/
;;  https://pubmed.ncbi.nlm.nih.gov/33085113/

;;  (pretty-print (find-fl-aoi "McPhaul MJ"))   

(use-modules (rnrs bytevectors))

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

(define (get-id-authors x)
  (let* ((a (map match:substring  (list-matches  "<Item Name=\"Author\" Type=\"String\">[-a-zA-Z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜƏƒƠơƯƯǺǻǼǽǾǿńŻć<>~_+=,.:;()&#@\" ]+</Item>" x )))
	 (b (map (lambda (x) (substring x 34 (- (string-length x) 7)) ) a))
	 (c (string-match "<Id>[0-9]{8}</Id>" x))
	 (d (xsubstring x (+ (match:start c) 4) (- (match:end c) 5))))
    (cons (list d) (list b))))

(define all-chars "-a-zA-Z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜƏƒƠơƯƯǺǻǼǽǾǿńŻć<>~_+=,.:;()&#@®\" ")



(define (process-vec-pmid lst results)
  ;;results passed in is '()
  (if (null? (cdr lst))
      (begin
	(if (null? (car lst))
	    (set! results (append  results '("null")))
	    (set! results (append  results (list (xsubstring (match:string (caar lst) )  (+ (match:start (caar lst)) 4) (- (match:end (caar lst)) 5) )))))
	results)
      (begin
	(if (null? (car lst))
	    (set! results (append  results '("null")))
            (set! results (append  results (list (xsubstring (match:string (caar lst) )  (+ (match:start (caar lst)) 4) (- (match:end (caar lst)) 5) )))))
	(process-vec-pmid (cdr lst) results))))

(define (process-vec-journal lst results)
  ;;results passed in is '()
  (if (null? (cdr lst))
      (begin
	(if (null? (car lst))
	    (set! results (append  results '("null")))
	    (set! results (append  results (list (xsubstring (match:string (caar lst) )  (+ (match:start (caar lst)) 43) (- (match:end (caar lst)) 7) )))))
	results)
      (begin
	(if (null? (car lst))
	    (set! results (append  results '("null")))
            (set! results (append  results (list (xsubstring (match:string (caar lst) )  (+ (match:start (caar lst)) 43) (- (match:end (caar lst)) 7) )))))
	(process-vec-journal (cdr lst) results))))

(define (process-vec-title lst results)
  ;;results passed in is '()
  (if (null? (cdr lst))
      (begin
	(if (null? (car lst))
	    (set! results (append  results '("null")))
	    (set! results (append  results (list (xsubstring (match:string (caar lst) )  (+ (match:start (caar lst)) 33) (- (match:end (caar lst)) 7) )))))
	results)
      (begin
	(if (null? (car lst))
	    (set! results (append  results '("null")))
            (set! results (append  results (list (xsubstring (match:string (caar lst) )  (+ (match:start (caar lst)) 33) (- (match:end (caar lst)) 7) )))))
	(process-vec-title (cdr lst) results))))



(define (get-title2 x)
  (let* ((search-term (string-append "<Id>[0-9]+</Id>"))
	 (a  (map list-matches  (circular-list search-term) x ))
	 (b (process-vec-pmid a '()))
	 (search-term (string-append "<Item Name=\"Title\" Type=\"String\">[" all-chars  "]+</Item>"))
	 (c  (map list-matches  (circular-list search-term) x ))
	 (d (process-vec-title c '()))
	 (search-term (string-append "<Item Name=\"FullJournalName\" Type=\"String\">[" all-chars  "]+</Item>"))
	 (e  (map list-matches  (circular-list search-term) x ))
	 (f (process-vec-journal e '()))
	 (sql (make-ref-sql b f d "INSERT INTO ref (pmid, journal, title) VALUES "))
	 (dummy (dbi-query ciccio sql))
	 )
    sql))


(define (make-ref-sql pmid journal title sql)
  ;;must pass in sql: INSERT INTO ref (pmid, journal, title) VALUES
  ;; full SQL statement will be created for insert into the ref table
  (if (null? (cdr pmid))
      (begin
	    (set! sql (string-append  sql "('" (car pmid) "', '" (car journal) "', '" (car title)  "')"))
	sql)
      (begin
            (set! sql (string-append  sql  "('" (car pmid) "', '" (car journal) "', '" (car title)  "'),"))
	(make-ref-sql (cdr pmid) (cdr journal) (cdr title) sql))) 
  )


(pretty-print (get-title2 c))
(pretty-print  c)

(define n '("33782712" "33781022" "33085113" "32866150"))

(car n)


(pretty-print (map list-matches (circular-list "<Item Name=\"Author\" Type=\"String\">[-a-zA-Z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜƏƒƠơƯƯǺǻǼǽǾǿńŻć<>~_+=,.:;()&#@\" ]+</Item>"  c ))



(define (get-title x)
  ;; x must be a string
  (let* ((a  (string-match "<Item Name=\"Title\" Type=\"String\">[-a-zA-Z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜƏƒƠơƯƯǺǻǼǽǾǿńŻć<>~_+=,.:;()&#@\" ]+</Item>"  x))
	 (start (+ (car (vector-ref a 1)) 33))
	 (stop (- (cdr (vector-ref a 1)) 7))
	 (s (vector-ref a 0))
	 (b  (substring  s start stop ))
	 ;;(c (string-match "[-a-zA-Z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜƏƒƠơƯƯǺǻǼǽǾǿńŻć<>~_+=,.:;()&#@\" ]+" c))
	 ;;(d (xsubstring x (+ (match:start c) 4) (- (match:end c) 5)))
	 )
      b))


(define (get-journal x)
  ;; x must be a string
  (let* ((a  (string-match "<Item Name=\"FullJournalName\" Type=\"String\">[-a-zA-Z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜƏƒƠơƯƯǺǻǼǽǾǿńŻć<>~_+=,.:;()&#@\" ]+</Item>"  x))
	 (start (+ (car (vector-ref a 1)) 33))
	 (stop (- (cdr (vector-ref a 1)) 7))
	 (s (vector-ref a 0))
	 (b  (substring  s start stop ))
	 ;;(c (string-match "[-a-zA-Z0-9ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿĀāĂăĄąĆćĈĉĊċČčĎďĐđĒēĔĕĖėĘęĚěĜĝĞğĠġĢģĤĥĦħĨĩĪīĬĭĮįİıĲĳĴĵĶķĸĹĺĻļĽľĿŀŁłŃńŅņŇňŉŊŋŌōŎŏŐőŒœŔŕŖŗŘřŚśŜŝŞşŠšŢţŤťŦŧŨũŪūŬŭŮůŰűŲųŴŵŶŷŸŹźŻżŽžſǍǎǏǐǑǒǓǔǕǖǗǘǙǚǛǜƏƒƠơƯƯǺǻǼǽǾǿńŻć<>~_+=,.:;()&#@\" ]+" c))
	 ;;(d (xsubstring x (+ (match:start c) 4) (- (match:end c) 5)))
	 )
      b))



(define title (get-journal  (car c)))


(define a '((#("Sum>\n\t<Id>33782712</Id>\n\t<Item Name=\"PubDate\" Type=\"Date\">2021 Mar 29</Item>\n\t<Item Name=\"EPubDate\" Type=\"Date\">2021 Mar 29</Item>\n\t<Item Name=\"Source\" Type=\"String\">Cell Mol Life Sci</Item>\n\t<Item Name=\"AuthorList\" Type=\"List\">\n\t\t<Item Name=\"Author\" Type=\"String\">Lyu J</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Mu X</Item>\n\t</Item>\n\t<Item Name=\"LastAuthor\" Type=\"String\">Mu X</Item>\n\t<Item Name=\"Title\" Type=\"String\">Genetic control of retinal ganglion cell genesis.</Item>\n\t<Item Name=\"Volume\" Type=\"String\"></Item>\n\t<Item Name=\"Issue\" Type=\"String\"></Item>\n\t<Item Name=\"Pages\" Type=\"String\"></Item>\n\t<Item Name=\"LangList\" Type=\"List\">\n\t\t<Item Name=\"Lang\" Type=\"String\">English</Item>\n\t</Item>\n\t<Item Name=\"NlmUniqueID\" Type=\"String\">9705402</Item>\n\t<Item Name=\"ISSN\" Type=\"String\">1420-682X</Item>\n\t<Item Name=\"ESSN\" Type=\"String\">1420-9071</Item>\n\t<Item Name=\"PubTypeList\" Type=\"List\">\n\t\t<Item Name=\"PubType\" Type=\"String\">Journal Article</Item>\n\t\t<Item Name=\"PubType\" Type=\"String\">Review</Item>\n\t</Item>\n\t<Item Name=\"RecordStatus\" Type=\"String\">PubMed - as supplied by publisher</Item>\n\t<Item Name=\"PubStatus\" Type=\"String\">aheadofprint</Item>\n\t<Item Name=\"ArticleIds\" Type=\"List\">\n\t\t<Item Name=\"pubmed\" Type=\"String\">33782712</Item>\n\t\t<Item Name=\"doi\" Type=\"String\">10.1007/s00018-021-03814-w</Item>\n\t\t<Item Name=\"pii\" Type=\"String\">10.1007/s00018-021-03814-w</Item>\n\t\t<Item Name=\"rid\" Type=\"String\">33782712</Item>\n\t\t<Item Name=\"eid\" Type=\"String\">33782712</Item>\n\t</Item>\n\t<Item Name=\"DOI\" Type=\"String\">10.1007/s00018-021-03814-w</Item>\n\t<Item Name=\"History\" Type=\"List\">\n\t\t<Item Name=\"received\" Type=\"Date\">2021/01/01 00:00</Item>\n\t\t<Item Name=\"accepted\" Type=\"Date\">2021/03/18 00:00</Item>\n\t\t<Item Name=\"revised\" Type=\"Date\">2021/02/27 00:00</Item>\n\t\t<Item Name=\"entrez\" Type=\"Date\">2021/03/30 08:05</Item>\n\t\t<Item Name=\"pubmed\" Type=\"Date\">2021/03/31 06:00</Item>\n\t\t<Item Name=\"medline\" Type=\"Date\">2021/03/31 06:00</Item>\n\t</Item>\n\t<Item Name=\"References\" Type=\"List\"></Item>\n\t<Item Name=\"HasAbstract\" Type=\"Integer\">1</Item>\n\t<Item Name=\"PmcRefCount\" Type=\"Integer\">0</Item>\n\t<Item Name=\"FullJournalName\" Type=\"String\">Cellular and molecular life sciences : CMLS</Item>\n\t<Item Name=\"ELocationID\" Type=\"String\">doi: 10.1007/s00018-021-03814-w</Item>\n\t<Item Name=\"SO\" Type=\"String\">2021 Mar 29;</Item>\n</DocSum>\n"
    (387 . 476)))
 (#("Sum>\n\t<Id>33781022</Id>\n\t<Item Name=\"PubDate\" Type=\"Date\">2021 Mar 29</Item>\n\t<Item Name=\"EPubDate\" Type=\"Date\">2021 Mar 29</Item>\n\t<Item Name=\"Source\" Type=\"String\">Minerva Urol Nephrol</Item>\n\t<Item Name=\"AuthorList\" Type=\"List\">\n\t\t<Item Name=\"Author\" Type=\"String\">Veccia A</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Carbonara U</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Derweesh I</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Mehrazin R</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Porter J</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Abdollah F</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Mazzone E</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Sundaram CP</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Gonzalgo M</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Mastroianni R</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Ghoreifi A</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Cacciamani GE</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Patel D</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Marcus J</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Danno A</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Steward J</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Bhattu AS</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Asghar A</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Reese AC</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Wu Z</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Uzzo RG</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Minervini A</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Rha KH</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Ferro M</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Margulis V</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Hampton LJ</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Simone G</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Eun DD</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Djaladat H</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Mottrie A</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Autorino R</Item>\n\t</Item>\n\t<Item Name=\"LastAuthor\" Type=\"String\">Autorino R</Item>\n\t<Item Name=\"Title\" Type=\"String\">Single stage Xi® robotic radical nephroureterectomy for upper tract urothelial carcinoma: surgical technique and outcomes.</Item>\n\t<Item Name=\"Volume\" Type=\"String\"></Item>\n\t<Item Name=\"Issue\" Type=\"String\"></Item>\n\t<Item Name=\"Pages\" Type=\"String\"></Item>\n\t<Item Name=\"LangList\" Type=\"List\">\n\t\t<Item Name=\"Lang\" Type=\"String\">English</Item>\n\t</Item>\n\t<Item Name=\"NlmUniqueID\" Type=\"String\">101777299</Item>\n\t<Item Name=\"ISSN\" Type=\"String\">2724-6051</Item>\n\t<Item Name=\"ESSN\" Type=\"String\">2724-6442</Item>\n\t<Item Name=\"PubTypeList\" Type=\"List\">\n\t\t<Item Name=\"PubType\" Type=\"String\">Journal Article</Item>\n\t</Item>\n\t<Item Name=\"RecordStatus\" Type=\"String\">PubMed - as supplied by publisher</Item>\n\t<Item Name=\"PubStatus\" Type=\"String\">aheadofprint</Item>\n\t<Item Name=\"ArticleIds\" Type=\"List\">\n\t\t<Item Name=\"pubmed\" Type=\"String\">33781022</Item>\n\t\t<Item Name=\"pii\" Type=\"String\">S2724-6051.21.04247-8</Item>\n\t\t<Item Name=\"doi\" Type=\"String\">10.23736/S2724-6051.21.04247-8</Item>\n\t\t<Item Name=\"rid\" Type=\"String\">33781022</Item>\n\t\t<Item Name=\"eid\" Type=\"String\">33781022</Item>\n\t</Item>\n\t<Item Name=\"DOI\" Type=\"String\">10.23736/S2724-6051.21.04247-8</Item>\n\t<Item Name=\"History\" Type=\"List\">\n\t\t<Item Name=\"entrez\" Type=\"Date\">2021/03/30 02:53</Item>\n\t\t<Item Name=\"pubmed\" Type=\"Date\">2021/03/31 06:00</Item>\n\t\t<Item Name=\"medline\" Type=\"Date\">2021/03/31 06:00</Item>\n\t</Item>\n\t<Item Name=\"References\" Type=\"List\"></Item>\n\t<Item Name=\"HasAbstract\" Type=\"Integer\">1</Item>\n\t<Item Name=\"PmcRefCount\" Type=\"Integer\">0</Item>\n\t<Item Name=\"FullJournalName\" Type=\"String\">Minerva urology and nephrology</Item>\n\t<Item Name=\"ELocationID\" Type=\"String\">doi: 10.23736/S2724-6051.21.04247-8</Item>\n\t<Item Name=\"SO\" Type=\"String\">2021 Mar 29;</Item>\n</DocSum>\n"
    (1940 . 2102)))
 (#("Sum>\n\t<Id>33085113</Id>\n\t<Item Name=\"PubDate\" Type=\"Date\">2021 Jan</Item>\n\t<Item Name=\"EPubDate\" Type=\"Date\">2020 Dec 10</Item>\n\t<Item Name=\"Source\" Type=\"String\">Environ Toxicol Chem</Item>\n\t<Item Name=\"AuthorList\" Type=\"List\">\n\t\t<Item Name=\"Author\" Type=\"String\">de la Rosa R</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Vazquez S</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Tachachartvanich P</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Daniels SI</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Sillé F</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Smith MT</Item>\n\t</Item>\n\t<Item Name=\"LastAuthor\" Type=\"String\">Smith MT</Item>\n\t<Item Name=\"Title\" Type=\"String\">Cell-Based Bioassay to Screen Environmental Chemicals and Human Serum for Total Glucocorticogenic Activity.</Item>\n\t<Item Name=\"Volume\" Type=\"String\">40</Item>\n\t<Item Name=\"Issue\" Type=\"String\">1</Item>\n\t<Item Name=\"Pages\" Type=\"String\">177-186</Item>\n\t<Item Name=\"LangList\" Type=\"List\">\n\t\t<Item Name=\"Lang\" Type=\"String\">English</Item>\n\t</Item>\n\t<Item Name=\"NlmUniqueID\" Type=\"String\">8308958</Item>\n\t<Item Name=\"ISSN\" Type=\"String\">0730-7268</Item>\n\t<Item Name=\"ESSN\" Type=\"String\">1552-8618</Item>\n\t<Item Name=\"PubTypeList\" Type=\"List\">\n\t\t<Item Name=\"PubType\" Type=\"String\">Journal Article</Item>\n\t</Item>\n\t<Item Name=\"RecordStatus\" Type=\"String\">PubMed - in process</Item>\n\t<Item Name=\"PubStatus\" Type=\"String\">ppublish+epublish</Item>\n\t<Item Name=\"ArticleIds\" Type=\"List\">\n\t\t<Item Name=\"pubmed\" Type=\"String\">33085113</Item>\n\t\t<Item Name=\"doi\" Type=\"String\">10.1002/etc.4903</Item>\n\t\t<Item Name=\"pmc\" Type=\"String\">PMC7793542</Item>\n\t\t<Item Name=\"mid\" Type=\"String\">NIHMS1638979</Item>\n\t\t<Item Name=\"rid\" Type=\"String\">33085113</Item>\n\t\t<Item Name=\"eid\" Type=\"String\">33085113</Item>\n\t\t<Item Name=\"pmcid\" Type=\"String\">pmc-id: PMC7793542;manuscript-id: NIHMS1638979;embargo-date: 2022/01/01;</Item>\n\t</Item>\n\t<Item Name=\"DOI\" Type=\"String\">10.1002/etc.4903</Item>\n\t<Item Name=\"History\" Type=\"List\">\n\t\t<Item Name=\"received\" Type=\"Date\">2020/04/13 00:00</Item>\n\t\t<Item Name=\"revised\" Type=\"Date\">2020/06/24 00:00</Item>\n\t\t<Item Name=\"accepted\" Type=\"Date\">2020/10/12 00:00</Item>\n\t\t<Item Name=\"pmc-release\" Type=\"Date\">2022/01/01 00:00</Item>\n\t\t<Item Name=\"pubmed\" Type=\"Date\">2020/10/22 06:00</Item>\n\t\t<Item Name=\"medline\" Type=\"Date\">2020/10/22 06:00</Item>\n\t\t<Item Name=\"entrez\" Type=\"Date\">2020/10/21 12:27</Item>\n\t</Item>\n\t<Item Name=\"References\" Type=\"List\"></Item>\n\t<Item Name=\"HasAbstract\" Type=\"Integer\">1</Item>\n\t<Item Name=\"PmcRefCount\" Type=\"Integer\">0</Item>\n\t<Item Name=\"FullJournalName\" Type=\"String\">Environmental toxicology and chemistry</Item>\n\t<Item Name=\"ELocationID\" Type=\"String\">doi: 10.1002/etc.4903</Item>\n\t<Item Name=\"SO\" Type=\"String\">2021 Jan;40(1):177-186</Item>\n</DocSum>\n"
    (622 . 769)))
 (#("Sum>\n\t<Id>32866150</Id>\n\t<Item Name=\"PubDate\" Type=\"Date\">2020</Item>\n\t<Item Name=\"EPubDate\" Type=\"Date\">2020 Aug 31</Item>\n\t<Item Name=\"Source\" Type=\"String\">PLoS One</Item>\n\t<Item Name=\"AuthorList\" Type=\"List\">\n\t\t<Item Name=\"Author\" Type=\"String\">Silva PIT</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Silva-Junior OB</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Resende LV</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Sousa VA</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Aguiar AV</Item>\n\t\t<Item Name=\"Author\" Type=\"String\">Grattapaglia D</Item>\n\t</Item>\n\t<Item Name=\"LastAuthor\" Type=\"String\">Grattapaglia D</Item>\n\t<Item Name=\"Title\" Type=\"String\">A 3K Axiom SNP array from a transcriptome-wide SNP resource sheds new light on the genetic diversity and structure of the iconic subtropical conifer tree Araucaria angustifolia (Bert.) Kuntze.</Item>\n\t<Item Name=\"Volume\" Type=\"String\">15</Item>\n\t<Item Name=\"Issue\" Type=\"String\">8</Item>\n\t<Item Name=\"Pages\" Type=\"String\">e0230404</Item>\n\t<Item Name=\"LangList\" Type=\"List\">\n\t\t<Item Name=\"Lang\" Type=\"String\">English</Item>\n\t</Item>\n\t<Item Name=\"NlmUniqueID\" Type=\"String\">101285081</Item>\n\t<Item Name=\"ISSN\" Type=\"String\"></Item>\n\t<Item Name=\"ESSN\" Type=\"String\">1932-6203</Item>\n\t<Item Name=\"PubTypeList\" Type=\"List\">\n\t\t<Item Name=\"PubType\" Type=\"String\">Journal Article</Item>\n\t</Item>\n\t<Item Name=\"RecordStatus\" Type=\"String\">PubMed - indexed for MEDLINE</Item>\n\t<Item Name=\"PubStatus\" Type=\"String\"></Item>\n\t<Item Name=\"ArticleIds\" Type=\"List\">\n\t\t<Item Name=\"pubmed\" Type=\"String\">32866150</Item>\n\t\t<Item Name=\"doi\" Type=\"String\">10.1371/journal.pone.0230404</Item>\n\t\t<Item Name=\"pii\" Type=\"String\">PONE-D-20-05745</Item>\n\t\t<Item Name=\"pmc\" Type=\"String\">PMC7458329</Item>\n\t\t<Item Name=\"rid\" Type=\"String\">32866150</Item>\n\t\t<Item Name=\"eid\" Type=\"String\">32866150</Item>\n\t\t<Item Name=\"pmcid\" Type=\"String\">pmc-id: PMC7458329;</Item>\n\t</Item>\n\t<Item Name=\"DOI\" Type=\"String\">10.1371/journal.pone.0230404</Item>\n\t<Item Name=\"History\" Type=\"List\">\n\t\t<Item Name=\"received\" Type=\"Date\">2020/02/27 00:00</Item>\n\t\t<Item Name=\"accepted\" Type=\"Date\">2020/08/05 00:00</Item>\n\t\t<Item Name=\"entrez\" Type=\"Date\">2020/09/01 06:00</Item>\n\t\t<Item Name=\"pubmed\" Type=\"Date\">2020/09/01 06:00</Item>\n\t\t<Item Name=\"medline\" Type=\"Date\">2020/09/29 06:00</Item>\n\t</Item>\n\t<Item Name=\"References\" Type=\"List\"></Item>\n\t<Item Name=\"HasAbstract\" Type=\"Integer\">1</Item>\n\t<Item Name=\"PmcRefCount\" Type=\"Integer\">0</Item>\n\t<Item Name=\"FullJournalName\" Type=\"String\">PloS one</Item>\n\t<Item Name=\"ELocationID\" Type=\"String\">doi: 10.1371/journal.pone.0230404</Item>\n\t<Item Name=\"SO\" Type=\"String\">2020;15(8):e0230404</Item>\n</DocSum>\n\n</eSummaryResult>\n"
    (613 . 845))))
)
