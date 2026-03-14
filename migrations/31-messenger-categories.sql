ALTER TABLE messenger_chat ADD COLUMN category TEXT;
ALTER TABLE messenger_message ADD COLUMN category TEXT;
UPDATE messenger_message SET chain_id = NULL;
DELETE FROM messenger_message_chain;
ALTER TABLE messenger_message_chain ADD COLUMN sequence_id TEXT NOT NULL;
