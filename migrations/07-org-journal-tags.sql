CREATE TABLE org_journal_tag (
  id TEXT NOT NULL,
  name TEXT NOT NULL,
  PRIMARY KEY (id)
) STRICT;

CREATE TABLE org_journal_record (
  id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  size INTEGER NOT NULL,
  PRIMARY KEY (id)
) STRICT;

CREATE TABLE org_journal_record_tag (
  record_id TEXT NOT NULL,
  tag_id TEXT NOT NULL,
  PRIMARY KEY (record_id, tag_id),
  FOREIGN KEY (record_id) REFERENCES org_journal_record (id),
  FOREIGN KEY (tag_id) REFERENCES org_journal_tag (id)
) STRICT;
