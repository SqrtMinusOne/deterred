CREATE TABLE org_headline (
  id TEXT NOT NULL,
  title TEXT NOT NULL,
  file_path TEXT NOT NULL,
  file_name TEXT NOT NULL,
  headline_path TEXT NOT NULL,
  tags TEXT NOT NULL,
  category TEXT NULL,
  deadline INTEGER NULL,
  scheduled INTEGER NULL,
  closed INTEGER NULL,
  created INTEGER NULL,
  started INTEGER NULL,
  PRIMARY KEY (id)
) STRICT;

CREATE TABLE org_clock_item (
  headline_id TEXT NOT NULL,
  start_timestamp INTEGER NOT NULL,
  end_timestamp INTEGER NOT NULL,
  PRIMARY KEY (headline_id, start_timestamp, end_timestamp),
  FOREIGN KEY (headline_id) REFERENCES org_headline (id)
) STRICT;
