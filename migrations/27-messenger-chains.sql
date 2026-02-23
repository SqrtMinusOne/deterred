CREATE TABLE messenger_message_chain (
  id TEXT NOT NULL,
  chat_id TEXT NOT NULL,
  timestamp_start INTEGER NOT NULL,
  timestamp_end INTEGER NOT NULL,
  PRIMARY KEY (id),
  FOREIGN KEY (chat_id) REFERENCES messenger_chat (id)
) STRICT;

ALTER TABLE messenger_message ADD COLUMN chain_id TEXT REFERENCES messenger_message_chain (id);

-- To delete stuff from the table faster
CREATE INDEX idx_message_chain_id ON messenger_message(chain_id);
