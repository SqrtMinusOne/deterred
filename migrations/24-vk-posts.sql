CREATE TABLE vk_post (
  id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  body TEXT NOT NULL,
  link TEXT,
  PRIMARY KEY (id)
) STRICT;
