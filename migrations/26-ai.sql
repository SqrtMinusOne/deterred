CREATE TABLE ai_usage_item (
  -- message_id will have to be faked for stats JSON parsing
  message_id TEXT NOT NULL PRIMARY KEY,

  -- Data we can backfill from parsing the stats JSON is required
  timestamp INTEGER NOT NULL,
  session_id TEXT NOT NULL,
  model_name TEXT NOT NULL,
  total_tokens INTEGER NOT NULL,
  usd_cost INTEGER NOT NULL,
  -- I'll also add a flag to exclude this from certain aggregation
  -- queries. E.g. the timestamps would only show dates when parsed
  -- from stats JSON.
  is_stats INTEGER NOT NULL,
  hostname TEXT NOT NULL,

  -- Data we can only get from parsing the .claude folder
  cwd TEXT NULL,
  project_id TEXT NULL,
  request_id TEXT NULL,
  version TEXT NULL,
  input_tokens INTEGER NULL,
  output_tokens INTEGER NULL,
  cache_creation_input_tokens INTEGER NULL,
  cache_read_input_tokens INTEGER NULL,
  FOREIGN KEY (project_id) REFERENCES wakatime_projects (id)
) STRICT;

CREATE TABLE ai_usage_file (
  message_id TEXT NOT NULL,
  project_id TEXT NULL,
  file_path TEXT NOT NULL,
  lines_added INTEGER NULL,
  lines_removed INTEGER NULL,
  PRIMARY KEY (message_id, file_path),
  FOREIGN KEY (message_id) REFERENCES ai_usage_item (message_id),
  FOREIGN KEY (project_id) REFERENCES wakatime_projects (id)
) STRICT;
