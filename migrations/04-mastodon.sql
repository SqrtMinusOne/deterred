CREATE TABLE mastodon_account (
  id TEXT NOT NULL,
  username TEXT NOT NULL,
  server TEXT NOT NULL,
  PRIMARY KEY (id)
) STRICT;

CREATE TABLE mastodon_post_mention (
  post_id TEXT NOT NULL,
  account_id TEXT NOT NULL,
  PRIMARY KEY (post_id, account_id),
  FOREIGN KEY (post_id) REFERENCES mastodon_post (id),
  FOREIGN KEY (account_id) REFERENCES mastodon_account (id)
) STRICT;

CREATE TABLE mastodon_post (
  id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  uri TEXT NOT NULL UNIQUE,
  server TEXT NOT NULL,
  replies_count INTEGER NOT NULL,
  reblogs_count INTEGER NOT NULL,
  favourites_count INTEGER NOT NULL,
  content TEXT NOT NULL,
  application TEXT,
  is_reply INTEGER NOT NULL,
  PRIMARY KEY (id)
) STRICT;
