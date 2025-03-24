CREATE VIEW activitywatch_notafk_period_human_readable AS
SELECT
    hostname,
    notafk_start_timestamp,
    notafk_end_timestamp,
    notafk_end_timestamp - notafk_start_timestamp len,
    datetime(notafk_start_timestamp, 'unixepoch') start,
    datetime(notafk_end_timestamp, 'unixepoch') end
FROM activitywatch_notafk_period anp
