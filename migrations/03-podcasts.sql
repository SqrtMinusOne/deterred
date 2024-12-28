CREATE TABLE podcasts_feed (
  id TEXT NOT NULL,
  title TEXT NOT NULL,
  url TEXT,
  language TEXT,
  PRIMARY KEY (id),
  UNIQUE (url)
) STRICT;

CREATE TABLE podcasts_listened (
  feed_id TEXT NOT NULL,
  item_id TEXT NOT NULL,
  url TEXT NOT NULL,
  title TEXT NOT NULL,
  published_timestamp INTEGER NOT NULL,
  total_duration INTEGER NOT NULL,
  played_duration INTEGER NOT NULL,
  timestamp INTEGER NOT NULL,
  PRIMARY KEY (feed_id, item_id),
  FOREIGN KEY (feed_id) REFERENCES podcasts_feed (id)
) STRICT;
