CREATE TABLE wakatime_item (
  start_timestamp INTEGER NOT NULL,
  end_timestamp INTEGER NOT NULL,
  project_id TEXT NOT NULL,
  PRIMARY KEY (project_id, start_timestamp, end_timestamp),
  FOREIGN KEY (project_id) REFERENCES wakatime_projects(id)
) STRICT
