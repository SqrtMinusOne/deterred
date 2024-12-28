CREATE TABLE wakatime_projects (
  id TEXT PRIMARY KEY NOT NULL,
  name TEXT NOT NULL,
  project_root TEXT
) STRICT;

CREATE TABLE wakatime_branches (
  project_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  total_seconds REAL NOT NULL,
  name TEXT NOT NULL,
  PRIMARY KEY(project_id, timestamp, name),
  FOREIGN KEY (project_id) REFERENCES wakatime_projects(id)
) STRICT;

CREATE TABLE wakatime_categories (
  project_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  total_seconds REAL NOT NULL,
  name TEXT NOT NULL,
  PRIMARY KEY(project_id, timestamp, name),
  FOREIGN KEY (project_id) REFERENCES wakatime_projects(id)
) STRICT;

CREATE TABLE wakatime_editors (
  project_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  total_seconds REAL NOT NULL,
  name TEXT NOT NULL,
  PRIMARY KEY(project_id, timestamp, name),
  FOREIGN KEY (project_id) REFERENCES wakatime_projects(id)
) STRICT;

CREATE TABLE wakatime_entities (
  project_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  total_seconds REAL NOT NULL,
  name TEXT NOT NULL,
  type TEXT,
  PRIMARY KEY(project_id, timestamp, name),
  FOREIGN KEY (project_id) REFERENCES wakatime_projects(id)
) STRICT;

CREATE TABLE wakatime_grand_total (
  project_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  total_seconds REAL NOT NULL,
  PRIMARY KEY(project_id, timestamp),
  FOREIGN KEY (project_id) REFERENCES wakatime_projects(id)
) STRICT;

CREATE TABLE wakatime_languages (
  project_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  total_seconds REAL NOT NULL,
  name TEXT NOT NULL,
  type TEXT,
  PRIMARY KEY(project_id, timestamp, name),
  FOREIGN KEY (project_id) REFERENCES wakatime_projects(id)
) STRICT;

CREATE TABLE wakatime_machines (
  project_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  total_seconds REAL NOT NULL,
  name TEXT NOT NULL,
  type TEXT,
  PRIMARY KEY(project_id, timestamp, name),
  FOREIGN KEY (project_id) REFERENCES wakatime_projects(id)
) STRICT;

CREATE TABLE wakatime_operating_systems (
  project_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  total_seconds REAL NOT NULL,
  name TEXT NOT NULL,
  type TEXT,
  PRIMARY KEY(project_id, timestamp, name),
  FOREIGN KEY (project_id) REFERENCES wakatime_projects(id)
) STRICT;
