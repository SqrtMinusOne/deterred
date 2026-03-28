CREATE TABLE fit_activity (
  id TEXT NOT NULL,
  software TEXT NULL,
  sport_name TEXT NULL,
  version REAL NULL,
  part_number TEXT NULL,
  start_timestamp INTEGER NOT NULL,
  end_timestamp INTEGER NOT NULL,
  start_lat REAL NULL,
  end_lat REAL NULL,
  distance REAL NULL,
  average_speed REAL NULL,
  PRIMARY KEY (id)
) STRICT;
