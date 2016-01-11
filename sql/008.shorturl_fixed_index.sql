ALTER TABLE shorturls DROP INDEX shorturls_idx;
ALTER TABLE shorturls ADD INDEX shorturls_idx (`shorturl`, `enabled`);
