CREATE TABLE messenger_import_progress (
  messenger TEXT NOT NULL,
  remote_chat_id TEXT NOT NULL,
  cutoff INTEGER NOT NULL,
  cursor TEXT NOT NULL,
  PRIMARY KEY (messenger, remote_chat_id)
) STRICT;
