-- Discard the old Rocket import and rebuild it with workspace group chats.
-- Clear affected chains before deleting their messages, including mixed chains.
CREATE TEMP TABLE deterred_rocket_chains AS
SELECT DISTINCT chain_id FROM messenger_message
WHERE messenger = 'rocket' AND chain_id IS NOT NULL;

UPDATE messenger_message SET chain_id = NULL
WHERE chain_id IN (SELECT chain_id FROM deterred_rocket_chains);

DELETE FROM messenger_message_chain
WHERE id IN (SELECT chain_id FROM deterred_rocket_chains);

DROP TABLE deterred_rocket_chains;

DELETE FROM messenger_message WHERE messenger = 'rocket';
DELETE FROM messenger_import_progress WHERE messenger = 'rocket';

-- Keep personal identities and group records still used by another messenger.
DELETE FROM messenger_chat
WHERE rocket_id IS NOT NULL AND type = 'group'
  AND telegram_id IS NULL AND vk_id IS NULL
  AND discord_id IS NULL AND max_id IS NULL
  AND NOT EXISTS (
    SELECT 1 FROM messenger_message WHERE chat_id = messenger_chat.id
  )
  AND NOT EXISTS (
    SELECT 1 FROM messenger_message_chain WHERE chat_id = messenger_chat.id
  );

UPDATE messenger_chat SET rocket_id = NULL
WHERE rocket_id IS NOT NULL AND type = 'group';

-- Track room progress separately even when messages share a workspace chat.
ALTER TABLE messenger_message ADD COLUMN rocket_room_id TEXT;
CREATE INDEX idx_messenger_message_rocket_room_timestamp
ON messenger_message (messenger, rocket_room_id, timestamp);
