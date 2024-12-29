CREATE TABLE messenger_user (
  id TEXT NOT NULL,
  telegram_id INTEGER UNIQUE,
  name TEXT,
  PRIMARY KEY (id)
) STRICT;

CREATE TABLE messenger_chat (
  id TEXT NOT NULL,
  telegram_id INTEGER UNIQUE,
  name TEXT,
  type TEXT NOT NULL,
  target_user_id TEXT,
  PRIMARY KEY (id),
  FOREIGN KEY (target_user_id) REFERENCES messenger_user (id)
) STRICT;

CREATE TABLE messenger_message (
  id TEXT NOT NULL,
  telegram_id INTEGER,
  sender_id TEXT NOT NULL,
  chat_id TEXT NOT NULL,
  content TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  is_attachment INTEGER NOT NULL,
  PRIMARY KEY (id),
  FOREIGN KEY (sender_id) REFERENCES messenger_user (id),
  FOREIGN KEY (chat_id) REFERENCES messenger_chat (id)
) STRICT;
