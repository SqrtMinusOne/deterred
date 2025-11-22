ALTER TABLE messenger_user ADD COLUMN vk_id TEXT;
ALTER TABLE messenger_chat ADD COLUMN vk_id TEXT;
ALTER TABLE messenger_message ADD COLUMN messenger TEXT;
UPDATE messenger_message SET messenger = 'telegram';
