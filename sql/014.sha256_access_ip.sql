-- Add back the not null constraint but lengthen the field for sha256'd IPs
ALTER TABLE shorturl_access MODIFY COLUMN ip VARCHAR(64) NOT NULL DEFAULT '';
