CREATE TABLE digikam_album (
  id INT NOT NULL,
  path TEXT NOT NULL,
  PRIMARY KEY (id)
) STRICT;

CREATE TABLE digikam_photo (
  id INT NOT NULL,
  filename TEXT NOT NULL,
  album_id INT NOT NULL,
  timestamp INTEGER NOT NULL,
  lat REAL,
  lon REAL,
  alt REAL,
  camera TEXT,
  PRIMARY KEY (id)
) STRICT;
