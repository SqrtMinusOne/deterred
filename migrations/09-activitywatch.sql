CREATE TABLE activitywatch_currentwindow_agg (
  day TEXT NOT NULL,
  hostname TEXT NOT NULL,
  app TEXT NOT NULL,
  total_duration REAL NOT NULL,
  PRIMARY KEY (day, hostname, app)
) STRICT;

CREATE TABLE activitywatch_notafk_period (
  hostname TEXT NOT NULL,
  notafk_start_timestamp INTEGER NOT NULL,
  notafk_end_timestamp INTEGER NOT NULL,
  PRIMARY KEY (hostname, notafk_start_timestamp, notafk_end_timestamp)
) STRICT;

CREATE TABLE location_static_hostnames (
  hostname TEXT NOT NULL,
  timezone INT NOT NULL,
  PRIMARY KEY (hostname)
) STRICT;
