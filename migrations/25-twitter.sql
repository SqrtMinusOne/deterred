CREATE TABLE twitter_post (
  id TEXT NOT NULL,
  twitter_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  body TEXT NOT NULL,
  PRIMARY KEY (id)
) STRICT;
