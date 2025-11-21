ALTER TABLE digikam_album ADD COLUMN location_id TEXT REFERENCES location(id);

ALTER TABLE digikam_photo ADD COLUMN location_id TEXT REFERENCES location(id);

CREATE VIEW digikam_photo_with_location AS
SELECT
  p.id,
  p.filename,
  p.album_id,
  p.timestamp,
  p.lat,
  p.lon,
  p.alt,
  p.camera,
  a.path album_path,
  COALESCE(a.location_id, p.location_id) location_id
FROM digikam_photo p
INNER JOIN digikam_album a ON p.album_id = a.id;
