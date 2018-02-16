ALTER TABLE shorturls ADD COLUMN longurl_hash VARCHAR(64) DEFAULT '';
UPDATE shorturls SET longurl_hash=SHA2(longurl, 256);
ALTER TABLE shorturls ADD INDEX shorturl_existence_idx (
  `longurl_hash`,
  `enabled`,
  `custom_shorturl`);
