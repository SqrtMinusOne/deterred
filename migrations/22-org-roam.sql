CREATE TABLE org_roam_node (
  id TEXT NOT NULL,
  filename TEXT NOT NULL UNIQUE,
  title TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  timestamp_deleted INTEGER,
  PRIMARY KEY (id)
) STRICT;

CREATE TABLE org_roam_node_modification (
  node_id TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  PRIMARY KEY (node_id, timestamp),
  FOREIGN KEY (node_id) REFERENCES org_roam_node (id)
) STRICT;

CREATE TABLE org_roam_node_tag (
  node_id TEXT NOT NULL,
  tag TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  timestamp_deleted INTEGER,
  PRIMARY KEY (node_id, tag),
  FOREIGN KEY (node_id) REFERENCES org_roam_node (id)
) STRICT;
