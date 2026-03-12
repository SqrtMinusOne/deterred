CREATE TABLE ai_accepted_completions (
  timestamp INTEGER NOT NULL,
  hostname TEXT NOT NULL,
  filename TEXT NOT NULL,
  length INTEGER NOT NULL,
  provider TEXT NOT NULL,
  project_id TEXT NULL,
  PRIMARY KEY (hostname, timestamp),
  FOREIGN KEY (project_id) REFERENCES wakatime_projects(id)
) STRICT;
