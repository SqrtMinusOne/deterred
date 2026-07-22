CREATE TABLE meta_ai_pricing_revision (
  pricing_revision_id TEXT NOT NULL PRIMARY KEY,
  provider TEXT NOT NULL CHECK (provider IN ('global', 'codex', 'claude')),
  source_hash TEXT NOT NULL UNIQUE,
  rules_json TEXT NOT NULL,
  created_at INTEGER NOT NULL,
  CHECK (length(source_hash) > 0)
) STRICT;

CREATE TABLE meta_ai_import_file (
  provider TEXT NOT NULL CHECK (provider IN ('codex', 'claude')),
  hostname TEXT NOT NULL,
  source_key TEXT NOT NULL,
  source_path TEXT NOT NULL,
  session_id TEXT,
  device_id INTEGER,
  inode INTEGER,
  size_bytes INTEGER NOT NULL CHECK (size_bytes >= 0),
  mtime_ns INTEGER NOT NULL,
  parsed_byte_offset INTEGER NOT NULL DEFAULT 0 CHECK (parsed_byte_offset >= 0),
  parsed_line_number INTEGER NOT NULL DEFAULT 0 CHECK (parsed_line_number >= 0),
  offset_boundary_hash TEXT,
  parser_version TEXT NOT NULL,
  parser_state_json TEXT,
  last_success_at INTEGER,
  last_error_at INTEGER,
  last_error TEXT,
  PRIMARY KEY (provider, hostname, source_key),
  UNIQUE (provider, hostname, source_path)
) STRICT;

CREATE TABLE meta_ai_authoritative_source (
  hostname TEXT NOT NULL,
  provider TEXT NOT NULL CHECK (provider IN ('codex', 'claude')),
  record_kind TEXT NOT NULL,
  parser_version TEXT NOT NULL,
  completed_at INTEGER NOT NULL,
  source_count INTEGER NOT NULL CHECK (source_count >= 0),
  usage_row_count INTEGER NOT NULL CHECK (usage_row_count >= 0),
  file_row_count INTEGER NOT NULL CHECK (file_row_count >= 0),
  source_fingerprint TEXT NOT NULL,
  PRIMARY KEY (hostname, provider, record_kind)
) STRICT;

ALTER TABLE ai_usage_item ADD COLUMN provider TEXT CHECK (provider IS NULL OR provider IN ('codex', 'claude'));
ALTER TABLE ai_usage_item ADD COLUMN record_kind TEXT CHECK (record_kind IS NULL OR record_kind IN ('codex-turn-model', 'claude-message', 'claude-stats', 'legacy-token-event'));
ALTER TABLE ai_usage_item ADD COLUMN data_quality TEXT CHECK (data_quality IS NULL OR data_quality IN ('observed', 'estimated', 'legacy-db', 'partial'));
ALTER TABLE ai_usage_item ADD COLUMN source_key TEXT;
ALTER TABLE ai_usage_item ADD COLUMN parser_version TEXT;
ALTER TABLE ai_usage_item ADD COLUMN usage_date TEXT;
ALTER TABLE ai_usage_item ADD COLUMN end_timestamp INTEGER;
ALTER TABLE ai_usage_item ADD COLUMN turn_key TEXT;
ALTER TABLE ai_usage_item ADD COLUMN parent_session_id TEXT;
ALTER TABLE ai_usage_item ADD COLUMN request_count INTEGER CHECK (request_count IS NULL OR request_count >= 0);
ALTER TABLE ai_usage_item ADD COLUMN message_count INTEGER CHECK (message_count IS NULL OR message_count >= 0);
ALTER TABLE ai_usage_item ADD COLUMN service_tier TEXT;
ALTER TABLE ai_usage_item ADD COLUMN reasoning_output_tokens INTEGER CHECK (reasoning_output_tokens IS NULL OR reasoning_output_tokens >= 0);
ALTER TABLE ai_usage_item ADD COLUMN tiered_input_tokens INTEGER CHECK (tiered_input_tokens IS NULL OR tiered_input_tokens >= 0);
ALTER TABLE ai_usage_item ADD COLUMN tiered_output_tokens INTEGER CHECK (tiered_output_tokens IS NULL OR tiered_output_tokens >= 0);
ALTER TABLE ai_usage_item ADD COLUMN tiered_cache_creation_input_tokens INTEGER CHECK (tiered_cache_creation_input_tokens IS NULL OR tiered_cache_creation_input_tokens >= 0);
ALTER TABLE ai_usage_item ADD COLUMN tiered_cache_read_input_tokens INTEGER CHECK (tiered_cache_read_input_tokens IS NULL OR tiered_cache_read_input_tokens >= 0);
ALTER TABLE ai_usage_item ADD COLUMN pricing_revision_id TEXT REFERENCES meta_ai_pricing_revision (pricing_revision_id);
ALTER TABLE ai_usage_item ADD COLUMN pricing_status TEXT CHECK (pricing_status IS NULL OR pricing_status IN ('priced', 'historical', 'unknown', 'not-applicable'));

ALTER TABLE ai_usage_file ADD COLUMN turn_key TEXT;
ALTER TABLE ai_usage_file ADD COLUMN touch_count INTEGER NOT NULL DEFAULT 1 CHECK (touch_count >= 0);
ALTER TABLE ai_usage_file ADD COLUMN add_count INTEGER NOT NULL DEFAULT 0 CHECK (add_count >= 0);
ALTER TABLE ai_usage_file ADD COLUMN update_count INTEGER NOT NULL DEFAULT 0 CHECK (update_count >= 0);
ALTER TABLE ai_usage_file ADD COLUMN delete_count INTEGER NOT NULL DEFAULT 0 CHECK (delete_count >= 0);
ALTER TABLE ai_usage_file ADD COLUMN move_count INTEGER NOT NULL DEFAULT 0 CHECK (move_count >= 0);
ALTER TABLE ai_usage_file ADD COLUMN previous_file_path TEXT;

UPDATE ai_usage_item
SET provider = CASE WHEN message_id LIKE 'codex:%' THEN 'codex' ELSE 'claude' END,
    record_kind = CASE WHEN message_id LIKE 'codex:%' THEN 'legacy-token-event' ELSE 'claude-message' END,
    data_quality = 'legacy-db',
    source_key = session_id,
    parser_version = 'legacy',
    usage_date = date(timestamp, 'unixepoch', 'localtime'),
    end_timestamp = timestamp,
    turn_key = CASE
      WHEN message_id LIKE 'codex:%' THEN NULL
      ELSE 'claude:' || message_id
    END,
    request_count = CASE WHEN message_id LIKE 'codex:%' THEN NULL ELSE 1 END,
    message_count = CASE WHEN message_id LIKE 'codex:%' THEN NULL ELSE 1 END,
    pricing_status = CASE
      WHEN total_tokens = 0 THEN 'not-applicable'
      WHEN usd_cost = 0 THEN 'unknown'
      ELSE 'historical'
    END
WHERE is_stats = 0;

UPDATE ai_usage_file
SET turn_key = (
  SELECT ai_usage_item.turn_key
  FROM ai_usage_item
  WHERE ai_usage_item.message_id = ai_usage_file.message_id
)
WHERE EXISTS (
  SELECT 1
  FROM ai_usage_item
  WHERE ai_usage_item.message_id = ai_usage_file.message_id
    AND ai_usage_item.turn_key IS NOT NULL
);

CREATE TEMP TABLE ai_usage_stats_v2 AS
WITH stats_with_date AS (
  SELECT *,
         CASE
           WHEN substr(message_id, 1, 5) = 'fake_' THEN substr(message_id, 6, 10)
           ELSE date(timestamp, 'unixepoch', 'localtime')
         END AS stats_date
  FROM ai_usage_item
  WHERE is_stats = 1
)
SELECT 'stats:' || hostname || ':' || stats_date || ':' || model_name AS stable_id,
       hostname,
       stats_date,
       model_name,
       MIN(timestamp) AS first_timestamp,
       MAX(timestamp) AS last_timestamp,
       COUNT(*) AS source_message_count,
       SUM(total_tokens) AS total_tokens,
       SUM(usd_cost) AS usd_cost,
       SUM(input_tokens) AS input_tokens,
       SUM(output_tokens) AS output_tokens,
       SUM(cache_creation_input_tokens) AS cache_creation_input_tokens,
       SUM(cache_read_input_tokens) AS cache_read_input_tokens
FROM stats_with_date
GROUP BY hostname, stats_date, model_name;

DELETE FROM ai_usage_file
WHERE message_id IN (SELECT message_id FROM ai_usage_item WHERE is_stats = 1);

DELETE FROM ai_usage_item WHERE is_stats = 1;

INSERT INTO ai_usage_item (
  message_id,
  timestamp,
  session_id,
  model_name,
  total_tokens,
  usd_cost,
  is_stats,
  hostname,
  cwd,
  project_id,
  request_id,
  version,
  input_tokens,
  output_tokens,
  cache_creation_input_tokens,
  cache_read_input_tokens,
  provider,
  record_kind,
  data_quality,
  source_key,
  parser_version,
  usage_date,
  end_timestamp,
  turn_key,
  parent_session_id,
  request_count,
  message_count,
  service_tier,
  reasoning_output_tokens,
  tiered_input_tokens,
  tiered_output_tokens,
  tiered_cache_creation_input_tokens,
  tiered_cache_read_input_tokens,
  pricing_revision_id,
  pricing_status
)
SELECT stable_id,
       first_timestamp,
       'stats:' || hostname || ':' || stats_date,
       model_name,
       total_tokens,
       usd_cost,
       1,
       hostname,
       NULL,
       NULL,
       NULL,
       NULL,
       input_tokens,
       output_tokens,
       cache_creation_input_tokens,
       cache_read_input_tokens,
       'claude',
       'claude-stats',
       'estimated',
       stable_id,
       'migration-35',
       stats_date,
       last_timestamp,
       NULL,
       NULL,
       NULL,
       source_message_count,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       NULL,
       CASE
         WHEN total_tokens = 0 THEN 'not-applicable'
         WHEN usd_cost = 0 THEN 'unknown'
         ELSE 'historical'
       END
FROM ai_usage_stats_v2;

DROP TABLE ai_usage_stats_v2;

CREATE INDEX idx_ai_usage_host_provider_date ON ai_usage_item (hostname, provider, usage_date);
CREATE INDEX idx_ai_usage_host_provider_kind ON ai_usage_item (hostname, provider, record_kind);
CREATE INDEX idx_ai_usage_source ON ai_usage_item (hostname, provider, source_key);
CREATE INDEX idx_ai_usage_turn ON ai_usage_item (turn_key);
CREATE INDEX idx_ai_usage_model_date ON ai_usage_item (model_name, usage_date);
CREATE INDEX idx_ai_usage_file_turn_path ON ai_usage_file (turn_key, file_path);

-- Historical Claude stats are estimates used only to fill dates for which
-- raw Claude records are unavailable.  Keep every imported stats row, but
-- expose a non-overlapping relation for reports so a later raw-data import
-- cannot double-count the same host/date.
CREATE VIEW ai_usage_effective AS
SELECT estimated_or_observed.*
FROM ai_usage_item AS estimated_or_observed
WHERE estimated_or_observed.is_stats = 0
   OR NOT EXISTS (
     SELECT 1
     FROM ai_usage_item AS observed
     WHERE observed.hostname = estimated_or_observed.hostname
       AND observed.provider = 'claude'
       AND observed.is_stats = 0
       AND COALESCE(observed.usage_date,
                    date(observed.timestamp, 'unixepoch', 'localtime'))
           = COALESCE(estimated_or_observed.usage_date,
                      date(estimated_or_observed.timestamp,
                           'unixepoch', 'localtime'))
   );
