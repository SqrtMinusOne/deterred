import argparse
import logging
import re
import socket
import sqlite3
import subprocess
import sys
import time
import uuid
from datetime import datetime, timedelta

MPD_NAMESPACE = uuid.UUID("c66d74fc-c243-4d8e-9e0a-72a88b78132b")


def parse_args():
    parser = argparse.ArgumentParser(description="Store MPD status in SQLite database")
    parser.add_argument(
        "--db", "-d", required=True, help="Path to SQLite database file"
    )
    return parser.parse_args()


def get_lock(process_name):
    get_lock._lock_socket = socket.socket(socket.AF_UNIX, socket.SOCK_DGRAM)
    try:
        get_lock._lock_socket.bind("\0" + process_name)
        logging.info("Got the lock")
    except socket.error:
        logging.info("Lock already exists, exiting")
        sys.exit()


def convert_time_to_seconds(string):
    if ":" in string:
        minutes, seconds = string.split(":")
        return int(minutes) * 60 + int(seconds)
    return 0


def get_mpd_status():
    cmd = [
        "mpc",
        "--format",
        "%file%\t%time%\t%artist%\t%albumartist%\t%album%\t%title%\t%date%",
    ]

    try:
        output = subprocess.check_output(cmd, text=True).strip().split("\n")
        result = {}

        # Check if anything is playing
        if len(output) > 1 and ("[playing]" in output[1] or "[paused]" in output[1]):
            # Parse the first line (track info)
            track_info = output[0].split("\t")
            result.update(
                {
                    "file": track_info[0],
                    "time": track_info[1],
                    "artist": track_info[2],
                    "albumartist": track_info[3],
                    "album": track_info[4],
                    "title": track_info[5],
                    "date": track_info[6],
                }
            )

            # Parse the second line (playing status and time)
            status_line = output[1]

            # Extract state
            state_match = re.search(r"\[(.*?)\]", status_line)
            result["state"] = state_match.group(1) if state_match else "unknown"

            # Extract elapsed time
            time_match = re.search(r"(\d+:\d+)/(\d+:\d+)", status_line)
            elapsed, total = 0, 0
            if time_match:
                elapsed = convert_time_to_seconds(time_match.group(1))
                total = convert_time_to_seconds(time_match.group(2))
            result.update(
                {
                    "elapsed": elapsed,
                    "total": total,
                    "start_time": datetime.now() - timedelta(seconds=elapsed),
                }
            )
        else:
            # Nothing is playing
            result = {
                "state": "stopped",
            }

        return result

    except subprocess.CalledProcessError:
        return {"error": "Failed to execute mpc command"}


def generate_song_id(file):
    return str(uuid.uuid3(MPD_NAMESPACE, file))


def extract_year(date):
    if len(date) >= 4:
        return int(date[:4])


def store_mpc_status(db_path, status):
    if "file" not in status:
        return

    time_listened = (datetime.now() - status["start_time"]).seconds
    if (time_listened / status["total"]) < 0:
        logging.info(f'Skipping: {time_listened} / {status["total"]}')
        return

    year = extract_year(status["date"])
    song_id = generate_song_id(status["file"])

    try:
        conn = sqlite3.connect(db_path)
        cursor = conn.cursor()

        cursor.execute(
            """
            INSERT INTO mpd_song (
                id, file, duration, artist, album_artist,
                album, title, year, musicbrainz_trackid
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, NULL)
            ON CONFLICT (id) DO UPDATE SET
                duration = excluded.duration,
                artist = excluded.artist,
                album_artist = excluded.album_artist,
                album = excluded.album,
                title = excluded.title,
                year = excluded.year
            WHERE duration != excluded.duration
               OR artist != excluded.artist
               OR album_artist != excluded.album_artist
               OR album != excluded.album
               OR title != excluded.title
               OR year != excluded.year
        """,
            (
                song_id,
                status["file"],
                status["total"],
                status["artist"],
                status["albumartist"],
                status["album"],
                status["title"],
                year,
            ),
        )
        rows_modified = cursor.rowcount

        cursor.execute(
            """
            INSERT INTO mpd_song_listened (mpd_song_id, timestamp)
            VALUES (?, ?)
            """,
            (song_id, int(status["start_time"].timestamp())),
        )
        if rows_modified > 0:
            cursor.execute(
                """
                INSERT INTO meta_table_updates (table_name, last_updated)
                VALUES ('mpd_song', unixepoch(CURRENT_TIMESTAMP))
                  ON CONFLICT (table_name)
                  DO UPDATE SET last_updated = unixepoch(CURRENT_TIMESTAMP)
                """
            )

        cursor.execute(
            """
            INSERT INTO meta_table_updates (table_name, last_updated)
            VALUES ('mpd_song_listened', unixepoch(CURRENT_TIMESTAMP))
              ON CONFLICT (table_name)
              DO UPDATE SET last_updated = unixepoch(CURRENT_TIMESTAMP)
            """
        )
        conn.commit()

    except sqlite3.Error as e:
        logging.error("Database error: %s", e)
        conn.rollback()
    finally:
        conn.close()


def wait_for_mpd():
    subprocess.run(["mpc", "idle", "player"])


def try_connect_to_mpd():
    countdown = 10
    while True:
        status = get_mpd_status()
        if "error" in status:
            print(f"Cannot connect to MPD [{countdown}/10]: {status}")
            countdown -= 1
            time.sleep(5)
        else:
            return


if __name__ == "__main__":
    get_lock("deterred_mpd")

    args = parse_args()
    current_status = None
    try_connect_to_mpd()
    while True:
        status = get_mpd_status()
        print(status)

        if status["state"] == "stopped":
            status = None

        if not current_status and status:
            current_status = status
        elif current_status and (
            status is None or (status["file"] != current_status["file"])
        ):
            store_mpc_status(args.db, current_status)
            current_status = status
        wait_for_mpd()
