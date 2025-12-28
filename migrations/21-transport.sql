CREATE TABLE transport_trips (
  id TEXT PRIMARY KEY NOT NULL,
  source TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  transport TEXT NOT NULL,
  route TEXT NOT NULL,
  cost INTEGER
) STRICT;
