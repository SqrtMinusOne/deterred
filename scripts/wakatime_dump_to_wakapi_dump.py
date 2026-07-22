#!/usr/bin/env python3
"""
Import a WakaTime JSON export file into a Wakapi instance via the API.

IMPORTANT: Before running this script, you must temporarily increase the
heartbeat_max_age setting on your Wakapi instance to accept old heartbeats.
Set the environment variable on your server:

    WAKAPI_HEARTBEAT_MAX_AGE=876000h

Then restart Wakapi. After the import is complete, you can revert this setting.
Without this change, heartbeats older than 7 days (the default) will be rejected.

To clear existing data before re-importing, use the "Clear Data" button on the
Wakapi settings page (/settings).
"""

import argparse
import base64
import json
import sys
import time

import requests

BATCH_SIZE = 500
DELAY_BETWEEN_BATCHES = 0.1  # seconds
WAKATIME_API_URL = "https://api.wakatime.com/api/v1"


def print_fetch_progress(resource_name, page, total_pages, collected):
    print(
        f"\r  {resource_name}: page {page}/{total_pages} | collected: {collected}",
        end="",
        flush=True,
    )


def fetch_wakatime_user_agents(wakatime_api_key):
    """Fetch user agent mappings from WakaTime API (id -> user agent string)."""
    print("Fetching user agents from WakaTime API...")
    agents = {}
    encoded = base64.b64encode(wakatime_api_key.encode()).decode()
    headers = {"Authorization": f"Basic {encoded}"}

    page = 1
    while True:
        url = f"{WAKATIME_API_URL}/users/current/user_agents?page={page}"
        resp = requests.get(url, headers=headers, timeout=10)
        resp.raise_for_status()
        data = resp.json()
        for entry in data.get("data", []):
            agents[entry["id"]] = {
                "value": entry.get("value", ""),
                "editor": entry.get("editor", ""),
                "os": entry.get("os", ""),
            }
        total_pages = data.get("total_pages", 1)
        print_fetch_progress("user agents", page, total_pages, len(agents))
        if page >= total_pages:
            break
        page += 1

    print()
    print(f"  Fetched {len(agents)} user agents")
    return agents


def fetch_wakatime_machine_names(wakatime_api_key):
    """Fetch machine name mappings from WakaTime API (id -> machine name)."""
    print("Fetching machine names from WakaTime API...")
    machines = {}
    encoded = base64.b64encode(wakatime_api_key.encode()).decode()
    headers = {"Authorization": f"Basic {encoded}"}

    page = 1
    while True:
        url = f"{WAKATIME_API_URL}/users/current/machine_names?page={page}"
        resp = requests.get(url, headers=headers, timeout=10)
        resp.raise_for_status()
        data = resp.json()
        for entry in data.get("data", []):
            machines[entry["id"]] = entry.get("value", "")
        total_pages = data.get("total_pages", 1)
        print_fetch_progress("machine names", page, total_pages, len(machines))
        if page >= total_pages:
            break
        page += 1

    print()
    print(f"  Fetched {len(machines)} machine names")
    return machines


def load_heartbeats(export_file, user_agents=None, machine_names=None):
    with open(export_file) as f:
        data = json.load(f)

    heartbeats = []
    for day in data["days"]:
        for hb in day["heartbeats"]:
            entry = {
                "entity": hb["entity"],
                "type": hb.get("type", "file"),
                "category": hb.get("category") or "",
                "project": hb.get("project") or "",
                "branch": hb.get("branch") or "",
                "language": hb.get("language") or "",
                "is_write": hb.get("is_write", False),
                "time": hb["time"],
                "lines": hb.get("lines") or 0,
                "lineno": hb.get("lineno") or 0,
                "cursorpos": hb.get("cursorpos") or 0,
                "line_additions": hb.get("line_additions") or 0,
                "line_deletions": hb.get("line_deletions") or 0,
            }

            # Resolve user agent UUID to actual user agent string
            ua_id = hb.get("user_agent_id", "")
            if user_agents and ua_id in user_agents:
                entry["user_agent"] = user_agents[ua_id]["value"]

            # Resolve machine name UUID to actual machine name
            machine_id = hb.get("machine_name_id", "")
            if machine_names and machine_id in machine_names:
                entry["machine"] = machine_names[machine_id]

            heartbeats.append(entry)
    return heartbeats


def send_batch(endpoint, headers, batch):
    resp = requests.post(endpoint, json=batch, headers=headers, timeout=60)
    if resp.status_code in (200, 201, 202):
        # Count individual successes from the response
        try:
            body = resp.json()
            responses = body.get("responses", [])
            ok = sum(1 for r in responses if len(r) >= 2 and r[1] in (200, 201, 202))
            bad = len(responses) - ok
            return ok, bad
        except (ValueError, KeyError):
            return len(batch), 0
    else:
        return 0, len(batch)


def main():
    parser = argparse.ArgumentParser(description="Import WakaTime export into Wakapi")
    parser.add_argument(
        "--file",
        required=True,
        help="Path to WakaTime JSON export file",
    )
    parser.add_argument(
        "--url",
        default="https://wakapi.sqrtminusone.xyz",
        help="Wakapi instance URL",
    )
    parser.add_argument(
        "--api-key",
        required=True,
        help="Wakapi API key",
    )
    parser.add_argument(
        "--wakatime-api-key",
        default=None,
        help="WakaTime API key (to resolve user agent and machine name UUIDs)",
    )
    parser.add_argument(
        "--batch-size",
        type=int,
        default=BATCH_SIZE,
        help=f"Number of heartbeats per request (default: {BATCH_SIZE})",
    )
    parser.add_argument(
        "--skip",
        type=int,
        default=0,
        help="Number of heartbeats to skip (to resume an interrupted import)",
    )
    args = parser.parse_args()

    # Optionally resolve user agent and machine name UUIDs
    user_agents = None
    machine_names = None
    if args.wakatime_api_key:
        try:
            user_agents = fetch_wakatime_user_agents(args.wakatime_api_key)
            machine_names = fetch_wakatime_machine_names(args.wakatime_api_key)
        except requests.RequestException as e:
            print(f"Warning: failed to fetch from WakaTime API: {e}")
            print("Continuing without user agent / machine name resolution.")
    else:
        print(
            "No --wakatime-api-key provided. Editor, OS, and machine info will not be "
            "imported.\nPass your WakaTime API key to resolve these fields."
        )

    print(f"Loading export file: {args.file}")
    heartbeats = load_heartbeats(args.file, user_agents, machine_names)
    total = len(heartbeats)
    print(f"Total heartbeats: {total}")

    if args.skip > 0:
        heartbeats = heartbeats[args.skip :]
        print(f"Skipping first {args.skip}, remaining: {len(heartbeats)}")

    endpoint = f"{args.url.rstrip('/')}/api/v1/users/current/heartbeats.bulk"
    encoded_key = base64.b64encode(args.api_key.encode()).decode()
    headers = {
        "Authorization": f"Bearer {encoded_key}",
        "Content-Type": "application/json",
    }

    # Quick connectivity check
    print(f"Testing connection to {args.url} ...")
    try:
        r = requests.get(f"{args.url.rstrip('/')}/api/health", timeout=10)
        if r.status_code != 200:
            print(f"Warning: health check returned {r.status_code}")
    except requests.RequestException as e:
        print(f"Error: cannot reach server: {e}")
        sys.exit(1)
    print("OK")

    imported = 0
    rejected = 0
    errors = 0
    start_time = time.time()

    remaining = len(heartbeats)
    for i in range(0, remaining, args.batch_size):
        batch = heartbeats[i : i + args.batch_size]

        try:
            ok, bad = send_batch(endpoint, headers, batch)
            imported += ok
            rejected += bad
        except requests.RequestException as e:
            print(f"\n  Request error at batch {i}: {e}")
            errors += len(batch)

        done = min(i + args.batch_size, remaining)
        elapsed = time.time() - start_time
        rate = done / elapsed if elapsed > 0 else 0
        eta = (remaining - done) / rate if rate > 0 else 0
        print(
            f"\r  [{done}/{remaining}] "
            f"{100 * done / remaining:.1f}% | "
            f"imported: {imported} | rejected: {rejected} | errors: {errors} | "
            f"ETA: {eta:.0f}s",
            end="",
            flush=True,
        )

        if DELAY_BETWEEN_BATCHES > 0:
            time.sleep(DELAY_BETWEEN_BATCHES)

    elapsed = time.time() - start_time
    print(f"\n\nDone in {elapsed:.1f}s")
    print(f"  Imported:  {imported}")
    print(f"  Rejected:  {rejected}")
    print(f"  Errors:    {errors}")

    if rejected > 0:
        print(
            "\nNote: Rejected heartbeats are likely due to heartbeat_max_age being "
            "too low. Set WAKAPI_HEARTBEAT_MAX_AGE=876000h on your server and retry."
        )


if __name__ == "__main__":
    main()
