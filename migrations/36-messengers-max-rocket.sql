ALTER TABLE messenger_user ADD COLUMN max_id TEXT;
ALTER TABLE messenger_chat ADD COLUMN max_id TEXT;
ALTER TABLE messenger_message ADD COLUMN max_id TEXT;

ALTER TABLE messenger_user ADD COLUMN rocket_id TEXT;
ALTER TABLE messenger_chat ADD COLUMN rocket_id TEXT;
ALTER TABLE messenger_message ADD COLUMN rocket_id TEXT;

CREATE INDEX idx_messenger_message_chat_timestamp
ON messenger_message (messenger, chat_id, timestamp);
