CREATE TABLE reddit_comment (
  id TEXT NOT NULL,
  url TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  subreddit TEXT NOT NULL,
  body TEXT NOT NULL,
  PRIMARY KEY (id)
) STRICT;

CREATE TABLE reddit_post (
  id TEXT NOT NULL,
  url TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  subreddit TEXT NOT NULL,
  body TEXT NOT NULL,
  title TEXT NOT NULL,
  PRIMARY KEY (id)
) STRICT;
