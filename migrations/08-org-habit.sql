CREATE TABLE habit_record (
  habit TEXT NOT NULL,
  timestamp INTEGER NOT NULL,
  PRIMARY KEY (habit, timestamp)
) STRICT;
