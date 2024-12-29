CREATE TABLE location (
  id TEXT NOT NULL,
  name TEXT NOT NULL,
  latitude REAL NOT NULL,
  longitude REAL NOT NULL,
  timezone INT NOT NULL,
  PRIMARY KEY (id)
) STRICT;

CREATE TABLE location_times (
  location_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  PRIMARY KEY (location_id, timestamp),
  FOREIGN KEY (location_id) REFERENCES location (id)
) STRICT;

CREATE VIEW location_times_human_readable AS
SELECT
    l.id AS location_id,
    l.name AS location_name,
    l.latitude,
    l.longitude,
    l.timezone,
    datetime(lt.timestamp, 'unixepoch') AS time
FROM location l inner join location_times lt on l.id = lt.location_id;
