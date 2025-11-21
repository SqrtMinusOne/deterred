-- Create the read_it_later_host table
CREATE TABLE read_it_later_host (
  host TEXT NOT NULL,
  language TEXT,
  PRIMARY KEY (host)
) STRICT;

-- Populate with existing hosts from read_it_later_article
INSERT INTO read_it_later_host (host, language)
SELECT DISTINCT host, NULL
FROM read_it_later_article;

-- Recreate read_it_later_article with foreign key constraint
CREATE TABLE read_it_later_article_new (
  id TEXT NOT NULL,
  href TEXT NOT NULL,
  url TEXT NOT NULL,
  title TEXT NOT NULL,
  host TEXT NOT NULL,
  created_at INTEGER NOT NULL,
  read_at INTEGER NOT NULL,
  provider TEXT,
  PRIMARY KEY (id),
  FOREIGN KEY (host) REFERENCES read_it_later_host(host)
) STRICT;

-- Copy data from old table
INSERT INTO read_it_later_article_new
SELECT * FROM read_it_later_article;

-- Drop old table and rename new one
DROP TABLE read_it_later_article;
ALTER TABLE read_it_later_article_new RENAME TO read_it_later_article;
