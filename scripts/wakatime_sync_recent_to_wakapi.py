#!/usr/bin/env python3
"""
Incrementally sync recent heartbeats from WakaTime to Wakapi.

Workflow:
1. Fetch recent heartbeats from WakaTime (default: last 7 days).
2. Probe Wakapi to find the latest stored heartbeat.
3. Fetch recent heartbeats from Wakapi for diffing.
4. Insert only missing heartbeats into Wakapi.

Safety check:
- If Wakapi's latest heartbeat is older than the oldest fetched WakaTime
  heartbeat, the script aborts because missing data may exceed the available
  WakaTime window.
"""

import argparse
import base64
from datetime import datetime, timedelta, timezone
import os
import sys
import time

import requests

WAKATIME_API_URL = "https://api.wakatime.com/api/v1"
WAKAPI_COMPAT_HEARTBEATS_PATH = "/api/compat/wakatime/v1/users/current/heartbeats"
WAKAPI_BULK_HEARTBEATS_PATH = "/api/v1/users/current/heartbeats.bulk"

DEFAULT_LOOKBACK_DAYS = 7
DEFAULT_WAKAPI_LAST_SEARCH_DAYS = 30
DEFAULT_BATCH_SIZE = 500
REQUEST_RETRY_ATTEMPTS = 5
REQUEST_RETRY_BASE_DELAY_SEC = 1.0


def print_fetch_progress(resource_name, page, total_pages, collected):
    print(
        f"\r  {resource_name}: page {page}/{total_pages} | collected: {collected}",
        end="",
        flush=True,
    )


def encode_base64(value):
    return base64.b64encode(value.encode()).decode()


def build_basic_headers(api_key):
    return {"Authorization": f"Basic {encode_base64(api_key)}"}


def build_wakapi_bulk_headers(api_key):
    return {
        "Authorization": f"Bearer {encode_base64(api_key)}",
        "Content-Type": "application/json",
    }


def recent_date_strings(days):
    now = datetime.now(timezone.utc).date()
    return [(now - timedelta(days=offset)).isoformat() for offset in range(days)]


def heartbeat_time(hb):
    return float(hb.get("time") or 0.0)


def heartbeat_iso(hb):
    ts = heartbeat_time(hb)
    if ts <= 0:
        return "n/a"
    return datetime.fromtimestamp(ts, tz=timezone.utc).isoformat()


def as_str(value):
    return value if isinstance(value, str) else ""


def as_int(value):
    try:
        if value is None:
            return 0
        return int(value)
    except (TypeError, ValueError):
        return 0


def get_json_with_retries(endpoint, headers, params, timeout, label):
    last_error = None
    for attempt in range(1, REQUEST_RETRY_ATTEMPTS + 1):
        try:
            response = requests.get(
                endpoint,
                headers=headers,
                params=params,
                timeout=timeout,
            )
            response.raise_for_status()
            return response.json()
        except requests.RequestException as exc:
            last_error = exc
            if attempt >= REQUEST_RETRY_ATTEMPTS:
                break
            delay = REQUEST_RETRY_BASE_DELAY_SEC * (2 ** (attempt - 1))
            print(
                f"\n  Warning: {label} failed (attempt {attempt}/{REQUEST_RETRY_ATTEMPTS}): "
                f"{exc}. Retrying in {delay:.1f}s ...",
                flush=True,
            )
            time.sleep(delay)
    raise last_error


def fetch_wakatime_user_agents(wakatime_api_key, timeout):
    """Fetch user agent mappings from WakaTime API (id -> metadata)."""
    print("Fetching user agents from WakaTime API...")
    agents = {}
    headers = build_basic_headers(wakatime_api_key)

    page = 1
    while True:
        endpoint = f"{WAKATIME_API_URL}/users/current/user_agents"
        payload = get_json_with_retries(
            endpoint,
            headers,
            {"page": page},
            timeout,
            f"user agents page {page}",
        )
        for entry in payload.get("data", []):
            agents[entry["id"]] = {
                "value": entry.get("value", ""),
                "editor": entry.get("editor", ""),
                "os": entry.get("os", ""),
            }
        total_pages = payload.get("total_pages", 1)
        print_fetch_progress("user agents", page, total_pages, len(agents))
        if page >= total_pages:
            break
        page += 1

    print()
    print(f"  Fetched {len(agents)} user agents")
    return agents


def fetch_wakatime_machine_names(wakatime_api_key, timeout):
    """Fetch machine name mappings from WakaTime API (id -> machine name)."""
    print("Fetching machine names from WakaTime API...")
    machines = {}
    headers = build_basic_headers(wakatime_api_key)

    page = 1
    while True:
        endpoint = f"{WAKATIME_API_URL}/users/current/machine_names"
        payload = get_json_with_retries(
            endpoint,
            headers,
            {"page": page},
            timeout,
            f"machine names page {page}",
        )
        for entry in payload.get("data", []):
            machines[entry["id"]] = entry.get("value", "")
        total_pages = payload.get("total_pages", 1)
        print_fetch_progress("machine names", page, total_pages, len(machines))
        if page >= total_pages:
            break
        page += 1

    print()
    print(f"  Fetched {len(machines)} machine names")
    return machines


def fetch_day_heartbeats(endpoint, headers, date_string, timeout):
    payload = get_json_with_retries(
        endpoint,
        headers,
        {"date": date_string},
        timeout,
        f"{endpoint} date={date_string}",
    )
    return payload.get("data", [])


def fetch_recent_heartbeats(source_name, endpoint, headers, days, timeout):
    all_heartbeats = []
    dates = recent_date_strings(days)
    for idx, date_string in enumerate(dates, start=1):
        day_heartbeats = fetch_day_heartbeats(endpoint, headers, date_string, timeout)
        all_heartbeats.extend(day_heartbeats)
        print(
            f"\r{source_name}: {idx}/{days} | {date_string} -> {len(day_heartbeats)} | "
            f"total: {len(all_heartbeats)}",
            end="",
            flush=True,
        )
    print()
    return all_heartbeats


def fetch_latest_wakapi_heartbeat(wakapi_url, wakapi_headers, search_days, timeout):
    endpoint = f"{wakapi_url.rstrip('/')}{WAKAPI_COMPAT_HEARTBEATS_PATH}"
    dates = recent_date_strings(search_days)

    for idx, date_string in enumerate(dates, start=1):
        day_heartbeats = fetch_day_heartbeats(
            endpoint, wakapi_headers, date_string, timeout
        )
        print(
            f"\rWakapi latest probe: {idx}/{search_days} | {date_string} -> {len(day_heartbeats)}",
            end="",
            flush=True,
        )
        if day_heartbeats:
            print()
            return max(day_heartbeats, key=heartbeat_time)

    print()
    return None


def normalize_wakatime_heartbeat(hb, user_agents=None, machine_names=None):
    human_line_changes = hb.get("human_line_changes") or {}
    line_additions = hb.get("line_additions")
    if line_additions is None:
        line_additions = (
            human_line_changes.get("additions")
            or human_line_changes.get("added")
            or 0
        )
    line_deletions = hb.get("line_deletions")
    if line_deletions is None:
        line_deletions = (
            human_line_changes.get("deletions")
            or human_line_changes.get("deleted")
            or 0
        )

    normalized = {
        "entity": as_str(hb.get("entity")),
        "type": as_str(hb.get("type")) or "file",
        "category": as_str(hb.get("category")),
        "project": as_str(hb.get("project")),
        "branch": as_str(hb.get("branch")),
        "language": as_str(hb.get("language")),
        "is_write": bool(hb.get("is_write", False)),
        "time": heartbeat_time(hb),
        "lines": as_int(hb.get("lines")),
        "lineno": as_int(hb.get("lineno")),
        "cursorpos": as_int(hb.get("cursorpos")),
        "line_additions": as_int(line_additions),
        "line_deletions": as_int(line_deletions),
    }
    ua_id = as_str(hb.get("user_agent_id"))
    if user_agents and ua_id in user_agents:
        normalized["user_agent"] = user_agents[ua_id]["value"]

    machine_id = as_str(hb.get("machine_name_id"))
    if machine_names and machine_id in machine_names:
        normalized["machine"] = machine_names[machine_id]

    return normalized


def heartbeat_fingerprint(hb):
    return (
        int(heartbeat_time(hb)),
        as_str(hb.get("entity")),
        as_str(hb.get("type")),
        as_str(hb.get("project")),
        as_str(hb.get("branch")),
    )


def send_batch(endpoint, headers, batch, timeout):
    response = requests.post(endpoint, json=batch, headers=headers, timeout=timeout)
    if response.status_code in (200, 201, 202):
        try:
            body = response.json()
            responses = body.get("responses", [])
            ok = sum(1 for item in responses if len(item) >= 2 and item[1] in (200, 201, 202))
            bad = len(responses) - ok
            return ok, bad
        except (ValueError, KeyError, TypeError):
            return len(batch), 0
    return 0, len(batch)


def import_missing_heartbeats(wakapi_url, wakapi_api_key, heartbeats, batch_size, timeout):
    endpoint = f"{wakapi_url.rstrip('/')}{WAKAPI_BULK_HEARTBEATS_PATH}"
    headers = build_wakapi_bulk_headers(wakapi_api_key)
    total = len(heartbeats)

    imported = 0
    rejected = 0
    errors = 0
    start = time.time()

    for i in range(0, total, batch_size):
        batch = heartbeats[i : i + batch_size]
        try:
            ok, bad = send_batch(endpoint, headers, batch, timeout)
            imported += ok
            rejected += bad
        except requests.RequestException as exc:
            print(f"\n  Request error at batch starting {i}: {exc}")
            errors += len(batch)

        done = min(i + batch_size, total)
        elapsed = time.time() - start
        rate = done / elapsed if elapsed > 0 else 0.0
        eta = (total - done) / rate if rate > 0 else 0.0
        print(
            f"\rUpload: {done}/{total} | imported: {imported} | rejected: {rejected} | "
            f"errors: {errors} | ETA: {eta:.0f}s",
            end="",
            flush=True,
        )

    print()
    return imported, rejected, errors


def main():
    parser = argparse.ArgumentParser(
        description="Sync recent WakaTime heartbeats into Wakapi (missing-only)."
    )
    parser.add_argument(
        "--wakatime-api-key",
        default=os.getenv("WAKATIME_API_KEY"),
        help="WakaTime API key (or set WAKATIME_API_KEY).",
    )
    parser.add_argument(
        "--wakapi-api-key",
        default=os.getenv("WAKAPI_API_KEY"),
        help="Wakapi API key (or set WAKAPI_API_KEY).",
    )
    parser.add_argument(
        "--wakapi-url",
        default=os.getenv("WAKAPI_URL", "https://wakapi.sqrtminusone.xyz"),
        help="Wakapi instance URL.",
    )
    parser.add_argument(
        "--lookback-days",
        type=int,
        default=DEFAULT_LOOKBACK_DAYS,
        help=f"Number of recent days to fetch from WakaTime (default: {DEFAULT_LOOKBACK_DAYS}).",
    )
    parser.add_argument(
        "--wakapi-last-search-days",
        type=int,
        default=DEFAULT_WAKAPI_LAST_SEARCH_DAYS,
        help=(
            "How many days to scan backwards on Wakapi to find the latest heartbeat "
            f"(default: {DEFAULT_WAKAPI_LAST_SEARCH_DAYS})."
        ),
    )
    parser.add_argument(
        "--batch-size",
        type=int,
        default=DEFAULT_BATCH_SIZE,
        help=f"Bulk upload size for Wakapi import (default: {DEFAULT_BATCH_SIZE}).",
    )
    parser.add_argument(
        "--timeout",
        type=float,
        default=30.0,
        help="HTTP request timeout in seconds (default: 30).",
    )
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Compute and print missing heartbeats without uploading.",
    )
    parser.add_argument(
        "--allow-unresolved-user-agents",
        action="store_true",
        help=(
            "Allow continuing when some WakaTime user_agent_id values cannot be "
            "resolved. By default this is a hard error."
        ),
    )

    args = parser.parse_args()

    if not args.wakatime_api_key:
        parser.error("Missing WakaTime API key. Use --wakatime-api-key or WAKATIME_API_KEY.")
    if not args.wakapi_api_key:
        parser.error("Missing Wakapi API key. Use --wakapi-api-key or WAKAPI_API_KEY.")
    if args.lookback_days < 1:
        parser.error("--lookback-days must be >= 1.")
    if args.wakapi_last_search_days < 1:
        parser.error("--wakapi-last-search-days must be >= 1.")
    if args.batch_size < 1:
        parser.error("--batch-size must be >= 1.")

    wakatime_headers = build_basic_headers(args.wakatime_api_key)
    wakapi_basic_headers = build_basic_headers(args.wakapi_api_key)

    wakatime_endpoint = f"{WAKATIME_API_URL}/users/current/heartbeats"
    wakapi_compat_endpoint = (
        f"{args.wakapi_url.rstrip('/')}{WAKAPI_COMPAT_HEARTBEATS_PATH}"
    )

    user_agents = fetch_wakatime_user_agents(args.wakatime_api_key, args.timeout)
    machine_names = fetch_wakatime_machine_names(args.wakatime_api_key, args.timeout)

    print(
        f"Fetching WakaTime heartbeats for last {args.lookback_days} day(s) "
        f"from {wakatime_endpoint} ..."
    )
    wakatime_heartbeats = fetch_recent_heartbeats(
        "WakaTime",
        wakatime_endpoint,
        wakatime_headers,
        args.lookback_days,
        args.timeout,
    )
    if not wakatime_heartbeats:
        print("No WakaTime heartbeats found in the requested window. Nothing to sync.")
        return

    wakatime_heartbeats.sort(key=heartbeat_time)
    oldest_wakatime = wakatime_heartbeats[0]
    newest_wakatime = wakatime_heartbeats[-1]
    print(
        f"WakaTime window: {len(wakatime_heartbeats)} heartbeats | "
        f"{heartbeat_iso(oldest_wakatime)} .. {heartbeat_iso(newest_wakatime)}"
    )

    unresolved_user_agent_ids = sorted(
        {
            as_str(hb.get("user_agent_id"))
            for hb in wakatime_heartbeats
            if as_str(hb.get("user_agent_id"))
            and as_str(hb.get("user_agent_id")) not in user_agents
        }
    )
    missing_user_agent_id_count = sum(
        1 for hb in wakatime_heartbeats if not as_str(hb.get("user_agent_id"))
    )
    if unresolved_user_agent_ids or missing_user_agent_id_count > 0:
        print("Error: could not fully resolve WakaTime user agents.")
        print(
            f"  Unresolved user_agent_id values: {len(unresolved_user_agent_ids)}\n"
            f"  Heartbeats with empty user_agent_id: {missing_user_agent_id_count}"
        )
        if unresolved_user_agent_ids:
            preview = ", ".join(unresolved_user_agent_ids[:10])
            print(f"  Sample unresolved IDs: {preview}")
        if not args.allow_unresolved_user_agents:
            print("Aborting. Re-run with --allow-unresolved-user-agents to override.")
            sys.exit(3)
        print("Continuing because --allow-unresolved-user-agents is set.")

    print(
        f"Finding latest Wakapi heartbeat (up to {args.wakapi_last_search_days} day(s) back) ..."
    )
    latest_wakapi = fetch_latest_wakapi_heartbeat(
        args.wakapi_url,
        wakapi_basic_headers,
        args.wakapi_last_search_days,
        args.timeout,
    )
    if latest_wakapi is None:
        print("Wakapi latest heartbeat: not found in search window.")
    else:
        print(
            "Wakapi latest heartbeat: "
            f"{heartbeat_iso(latest_wakapi)} | entity={latest_wakapi.get('entity', '')}"
        )

    print(f"Fetching Wakapi heartbeats for last {args.lookback_days} day(s) for diffing ...")
    wakapi_recent_heartbeats = fetch_recent_heartbeats(
        "Wakapi",
        wakapi_compat_endpoint,
        wakapi_basic_headers,
        args.lookback_days,
        args.timeout,
    )

    if latest_wakapi is not None:
        latest_wakapi_ts = heartbeat_time(latest_wakapi)
        oldest_wakatime_ts = heartbeat_time(oldest_wakatime)
        if latest_wakapi_ts < oldest_wakatime_ts:
            gap_days = (oldest_wakatime_ts - latest_wakapi_ts) / 86400.0
            print(
                "Error: Wakapi appears to be missing heartbeats older than the fetched "
                f"WakaTime window ({args.lookback_days} day(s))."
            )
            print(
                f"  Latest Wakapi heartbeat: {heartbeat_iso(latest_wakapi)}\n"
                f"  Oldest fetched WakaTime heartbeat: {heartbeat_iso(oldest_wakatime)}\n"
                f"  Gap: {gap_days:.2f} day(s)"
            )
            sys.exit(2)

    wakapi_fingerprints = {heartbeat_fingerprint(hb) for hb in wakapi_recent_heartbeats}
    missing = []
    missing_fingerprints = set()

    for hb in wakatime_heartbeats:
        normalized = normalize_wakatime_heartbeat(hb, user_agents, machine_names)
        fingerprint = heartbeat_fingerprint(normalized)
        if fingerprint in wakapi_fingerprints or fingerprint in missing_fingerprints:
            continue
        missing.append(normalized)
        missing_fingerprints.add(fingerprint)

    if not missing:
        print("No missing heartbeats found. Wakapi is up to date for this window.")
        return

    print(
        f"Missing heartbeats to import: {len(missing)} | "
        f"{heartbeat_iso(missing[0])} .. {heartbeat_iso(missing[-1])}"
    )

    if args.dry_run:
        print("Dry run enabled. No data was uploaded.")
        return

    print(f"Uploading missing heartbeats to {args.wakapi_url} ...")
    imported, rejected, errors = import_missing_heartbeats(
        args.wakapi_url,
        args.wakapi_api_key,
        missing,
        args.batch_size,
        args.timeout,
    )
    print(
        f"Done. imported={imported} rejected={rejected} errors={errors} "
        f"requested={len(missing)}"
    )
    if rejected > 0:
        print(
            "Note: rejected heartbeats are often caused by heartbeat_max_age being too low."
        )
    if rejected > 0 or errors > 0:
        sys.exit(1)


if __name__ == "__main__":
    main()
