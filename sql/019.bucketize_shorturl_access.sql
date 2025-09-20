-- This migration introduces a new table for bucketized shorturl access metrics.
-- It migrates existing data from `shorturl_access` into the new `shorturl_access_buckets`
-- table. For now, `shorturl_access` is left as-is, but it will be dropped in a future
-- migration.

CREATE TABLE `shorturl_access_buckets` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `shorturl_id` int(11) NOT NULL,
  `access_hour` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `access_count` int(11) NOT NULL DEFAULT 1,
  PRIMARY KEY (`id`),
  UNIQUE KEY `shorturl_id_access_hour` (`shorturl_id`, `access_hour`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

INSERT INTO shorturl_access_buckets (shorturl_id, access_hour, access_count)
SELECT
  shorturl_id,
  STR_TO_DATE(
    DATE_FORMAT(access_dt, '%Y-%m-%d %H:00:00'),
    '%Y-%m-%d %H:%i:%s'
  ) as access_hour,
  COUNT(*) as access_count
FROM shorturl_access
GROUP BY shorturl_id, access_hour;
