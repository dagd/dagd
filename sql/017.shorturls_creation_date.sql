ALTER TABLE shorturls ADD COLUMN creation_date DATE GENERATED ALWAYS AS (DATE(creation_dt)) VIRTUAL;
CREATE INDEX creation_date_idx ON shorturls (creation_date);
