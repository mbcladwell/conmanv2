
DROP TABLE IF EXISTS conman CASCADE;

--users------------------------------------------------------

DROP TABLE IF EXISTS conman CASCADE;
CREATE TABLE conman
(id SERIAL PRIMARY KEY,
        batchid BIGINT,
        pmid VARCHAR(250),
	qname  VARCHAR(250),
	wholen  VARCHAR(250),
	firstn  VARCHAR(250),
	lastn  VARCHAR(250),
	affil  VARCHAR(65000),
	email  VARCHAR(250),
	updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        sent TIMESTAMP);

DROP TABLE IF EXISTS ref CASCADE;
CREATE TABLE ref
(id SERIAL PRIMARY KEY,
        pmid VARCHAR(250),
	journal  VARCHAR(65000),
	title  VARCHAR(65000),
	updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        sent TIMESTAMP);




DROP TABLE IF EXISTS conmanstats CASCADE;
CREATE TABLE conmanstats
(id SERIAL PRIMARY KEY,
        batchid BIGINT,
        article INTEGER,
        author INTEGER,
        author_search INTEGER,
        elapsed INTEGER,
	updated TIMESTAMP DEFAULT CURRENT_TIMESTAMP);


DROP FUNCTION IF exists get_unique_email_batch( _start_date VARCHAR(250));

CREATE OR REPLACE FUNCTION get_unique_email_batch(_start_date VARCHAR(250))
RETURNS TABLE( firstn VARCHAR(250),  emails VARCHAR(250)) AS
$BODY$
DECLARE
ids INTEGER[];
id_var INTEGER;
emails VARCHAR[];
start_time TIMESTAMP = _start_date || ' 00:00:00';
ts TIMESTAMP = CURRENT_TIMESTAMP;
BEGIN

DROP TABLE IF EXISTS t1;
CREATE TEMP TABLE t1 AS SELECT DISTINCT ON (email) email, conman.ID from conman WHERE updated >  start_time and email != 'null';
select array (select t1.id from t1) into ids;

FOR id_var IN 1..array_length(ids,1) LOOP
update conman  SET sent= ts WHERE conman.id = ids[id_var];
END LOOP;

DROP TABLE IF EXISTS t1;

RETURN QUERY SELECT conman.firstn, conman.email FROM conman WHERE sent = ts;

END;
$BODY$
  LANGUAGE plpgsql VOLATILE;


-- SELECT * FROM get_unique_email_batch('2020-09-10');
-- SELECT email, sent FROM conman ORDER BY email;
-- UPDATE conman SET sent = NULL;

-- SELECT DISTINCT on (email) email, conman.ID from conman WHERE updated >  '2020-09-10 00:00:00' and email != 'null' ORDER BY email;























