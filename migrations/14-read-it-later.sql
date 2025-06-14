CREATE TABLE read_it_later_article (
  id TEXT NOT NULL,
  href TEXT NOT NULL,
  url TEXT NOT NULL,
  title TEXT NOT NULL,
  host TEXT NOT NULL,
  created_at INTEGER NOT NULL,
  read_at INTEGER NOT NULL,
  provider TEXT,
  PRIMARY KEY (id)
) STRICT;
