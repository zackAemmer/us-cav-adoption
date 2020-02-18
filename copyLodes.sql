CREATE TABLE puma_tract_dict (
	tract_code CHARACTER VARYING(50),
	puma_code CHARACTER VARYING(50)
);

CREATE TABLE trips_new (
	w_geocode VARCHAR(15),
	h_geocode VARCHAR(15),
	s000 INT,
	sa01 INT,
	sa02 INT,
	sa03 INT,
	se01 INT,
	se02 INT,
	se03 INT,
	si01 INT,
	si02 INT,
	si03 INT,
	createdate VARCHAR(50)
);

DROP TABLE trips_temp;
CREATE TABLE trips_temp AS (SELECT * FROM trips LIMIT 1000000);

COPY trips_new(w_geocode, h_geocode, s000, sa01, sa02, sa03, se01, se02, se03, si01, si02, si03, createdate)
FROM 'C:\Users\Zae5o\Desktop\STL\Toyota AV\Data\LODES\wy_od_main_JT00_2017.csv' DELIMITER ',' CSV HEADER;

CREATE INDEX tracts_index ON puma_tract_dict(tract_code);
CREATE INDEX tracts_index_trips ON trips_temp(h_geocode);

ALTER TABLE trips_new 
ADD tract_code_trips VARCHAR(11);

UPDATE trips_new
SET tract_code_trips = LEFT(h_geocode, 11);

EXPLAIN
SELECT * INTO trips_w_pumas
FROM trips_new
INNER JOIN puma_tract_dict
ON trips_new.tract_code_trips = puma_tract_dict.tract_code;

CREATE INDEX pumas_index ON trips_w_pumas(puma_code);

SELECT * FROM non_selected_trips WHERE puma_code = '2504903';

DROP TABLE trips_new;

SELECT * INTO non_selected_trips
FROM trips_w_pumas;

CREATE TABLE selected_trips (
	w_geocode VARCHAR(15),
	h_geocode VARCHAR(15),
	s000 INT,
	sa01 INT,
	sa02 INT,
	sa03 INT,
	se01 INT,
	se02 INT,
	se03 INT,
	si01 INT,
	si02 INT,
	si03 INT,
	createdate VARCHAR(50),
	tract_code_trips VARCHAR(11),
	tract_code VARCHAR (50),
	puma_code VARCHAR (50)
);

COPY selected_trips(w_geocode, h_geocode, s000, sa01, sa02, sa03, se01, se02, se03, si01, si02, si03, createdate, tract_code, puma_code)
FROM 'C:\Users\Zae5o\Desktop\STL\Toyota AV\Data\selected_ods.csv' DELIMITER ',' CSV HEADER;

CREATE INDEX puma_index ON non_selected_trips(puma_code);

EXPLAIN
DELETE FROM non_selected_trips
USING selected_trips
WHERE non_selected_trips.w_geocode = selected_trips.w_geocode AND non_selected_trips.h_geocode = selected_trips.h_geocode;

SELECT COUNT(w_geocode)
FROM selected_trips;




