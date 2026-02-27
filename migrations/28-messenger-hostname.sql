ALTER TABLE messenger_message ADD COLUMN hostname TEXT;

CREATE INDEX idx_notafk_start ON activitywatch_notafk_period (notafk_start_timestamp, notafk_end_timestamp, hostname);
CREATE INDEX idx_msg_timestamp ON messenger_message (timestamp);
CREATE INDEX idx_message_chain_hostname ON messenger_message(chain_id, hostname);
