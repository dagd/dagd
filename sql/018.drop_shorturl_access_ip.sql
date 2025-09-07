-- DESTRUCTIVE
-- This migration will drop the (long unused) `ip` column
-- from the `shorturl_access` table. This column has not
-- contained new data since May 2020. The historical data
-- will be dropped. If you wish to preserve it, BACK UP YOUR
-- TABLE before applying this migration.
ALTER TABLE shorturl_access
  DROP COLUMN ip,
  ALGORITHM=INPLACE,
  LOCK=none;
