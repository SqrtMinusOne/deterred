CREATE TABLE meta_table_updates (
    table_name TEXT PRIMARY KEY,
    last_updated INTEGER DEFAULT CURRENT_TIMESTAMP
) STRICT;

CREATE TABLE mpd_song (
	id BLOB PRIMARY KEY NOT NULL,
	file text NOT NULL UNIQUE,
	duration int NOT NULL,
	artist text NULL,
	album_artist text NOT NULL,
	album text NOT NULL,
	title text NOT NULL,
	year int NULL,
	musicbrainz_trackid text NULL
) STRICT;

CREATE TABLE mpd_song_listened (
  mpd_song_id BLOB NOT NULL,
  timestamp INTEGER,
  PRIMARY KEY(mpd_song_id, timestamp),
  FOREIGN KEY (mpd_song_id) REFERENCES mpd_song(id)
) STRICT;
