#!/usr/bin/env python3

import json
import sys
from pathlib import Path
from struct import unpack_from


FIT_EPOCH = 631065600

BASE_TYPES = {
    0: ("B", 0xFF),
    1: ("b", 0x7F),
    2: ("B", 0xFF),
    7: (None, None),
    10: ("B", 0),
    13: ("B", 0xFF),
    131: ("h", 0x7FFF),
    132: ("H", 0xFFFF),
    133: ("i", 0x7FFFFFFF),
    134: ("I", 0xFFFFFFFF),
    136: ("f", None),
    137: ("d", None),
    139: ("H", 0),
    140: ("I", 0),
    142: ("q", 0x7FFFFFFFFFFFFFFF),
    143: ("Q", 0xFFFFFFFFFFFFFFFF),
}

SPORTS = {
    0: "generic",
    1: "running",
    2: "cycling",
    3: "transition",
    4: "fitness_equipment",
    5: "swimming",
    6: "basketball",
    7: "soccer",
    8: "tennis",
    9: "american_football",
    10: "training",
    11: "walking",
    12: "cross_country_skiing",
    13: "alpine_skiing",
    14: "snowboarding",
    15: "rowing",
    16: "mountaineering",
    17: "hiking",
    18: "multisport",
    19: "paddling",
    254: "all",
}

MESSAGE_NAMES = {
    0: "file_id",
    12: "sport",
    18: "session",
    20: "record",
    35: "software",
}

FIELD_NAMES = {
    0: {
        0: "type",
        1: "manufacturer",
        2: "product",
        3: "serial_number",
        4: "time_created",
        8: "product_name",
    },
    12: {
        0: "sport",
        1: "sub_sport",
        3: "name",
    },
    18: {
        2: "start_time",
        7: "total_elapsed_time",
        8: "total_timer_time",
        9: "total_distance",
        14: "avg_speed",
        5: "sport",
        6: "sub_sport",
    },
    20: {
        253: "timestamp",
        0: "position_lat",
        1: "position_long",
        5: "distance",
        6: "speed",
    },
    35: {
        3: "version",
        5: "part_number",
    },
}


def semicircles_to_degrees(value):
    return value * (180.0 / 2**31)


def decode_value(base_type, raw, architecture):
    if base_type == 7:
        value = raw.split(b"\x00", 1)[0].decode("utf-8", "ignore")
        return value or None

    fmt_info = BASE_TYPES.get(base_type)
    if fmt_info is None:
        return None

    fmt, invalid = fmt_info
    value = unpack_from((">" if architecture else "<") + fmt, raw)[0]
    if invalid is not None and value == invalid:
        return None
    return value


def first_non_null(records, key):
    for record in records:
        value = record.get(key)
        if value is not None:
            return value
    return None


def last_non_null(records, key):
    for record in reversed(records):
        value = record.get(key)
        if value is not None:
            return value
    return None


def parse_fit(path):
    data = path.read_bytes()
    header_size = data[0]
    data_size = unpack_from("<I", data, 4)[0]
    pos = header_size
    end = header_size + data_size
    local_defs = {}
    last_timestamp = None

    parsed = {
        "file_id": {},
        "sport": {},
        "session": {},
        "software": {},
        "records": [],
    }

    while pos < end:
        header = data[pos]
        pos += 1

        compressed_ts = bool(header & 0x80)
        if compressed_ts:
            is_definition = False
            local_message = (header >> 5) & 0x03
            time_offset = header & 0x1F
        else:
            is_definition = bool(header & 0x40)
            local_message = header & 0x0F

        if is_definition:
            architecture = data[pos + 1]
            global_message = unpack_from(
                ">H" if architecture else "<H", data, pos + 2
            )[0]
            fields_count = data[pos + 4]
            pos += 5

            fields = []
            for _ in range(fields_count):
                field_num, size, base_type = data[pos : pos + 3]
                pos += 3
                fields.append((field_num, size, base_type))

            developer_fields = []
            if header & 0x20:
                developer_fields_count = data[pos]
                pos += 1
                for _ in range(developer_fields_count):
                    field_num, size, dev_data_index = data[pos : pos + 3]
                    pos += 3
                    developer_fields.append((field_num, size, dev_data_index))

            local_defs[local_message] = (
                architecture,
                global_message,
                fields,
                developer_fields,
            )
            continue

        architecture, global_message, fields, developer_fields = local_defs[local_message]
        values = {}

        for field_num, size, base_type in fields:
            raw = data[pos : pos + size]
            pos += size
            values[field_num] = decode_value(base_type, raw, architecture)

        for _, size, _ in developer_fields:
            pos += size

        if compressed_ts:
            timestamp = ((last_timestamp or 0) & ~0x1F) | time_offset
            if last_timestamp is not None and timestamp <= last_timestamp:
                timestamp += 0x20
            values[253] = timestamp

        if values.get(253) is not None:
            last_timestamp = values[253]

        message_name = MESSAGE_NAMES.get(global_message)
        if message_name == "record":
            parsed["records"].append(
                {FIELD_NAMES[20].get(key, key): value for key, value in values.items()}
            )
        elif message_name is not None and not parsed[message_name]:
            parsed[message_name] = {
                FIELD_NAMES[global_message].get(key, key): value
                for key, value in values.items()
            }

    records = [record for record in parsed["records"] if record.get("timestamp") is not None]
    if not records:
        return None
    records.sort(key=lambda record: record["timestamp"])

    file_id = parsed["file_id"]
    sport = parsed["sport"]
    session = parsed["session"]
    software = parsed["software"]

    start_record = records[0]
    end_record = records[-1]

    start_lat = first_non_null(records, "position_lat")
    end_lat = last_non_null(records, "position_lat")
    distance = last_non_null(records, "distance")

    average_speed = session.get("avg_speed")
    if average_speed is not None:
        average_speed = average_speed / 1000.0 * 3.6
    elif distance is not None and end_record["timestamp"] > start_record["timestamp"]:
        average_speed = (
            (distance / 100.0) / (end_record["timestamp"] - start_record["timestamp"])
        ) * 3.6

    version = software.get("version")
    if version is not None:
        version = version / 100.0

    return {
        "file_id_key": "|".join(
            str(file_id.get(key))
            for key in (
                "serial_number",
                "time_created",
                "manufacturer",
                "product",
                "type",
                "product_name",
            )
        ),
        "software": file_id.get("product_name") or software.get("part_number"),
        "sport_name": SPORTS.get(session.get("sport")) or sport.get("name"),
        "version": version,
        "part_number": software.get("part_number"),
        "start_timestamp": start_record["timestamp"] + FIT_EPOCH,
        "end_timestamp": end_record["timestamp"] + FIT_EPOCH,
        "start_lat": (
            semicircles_to_degrees(start_lat) if start_lat is not None else None
        ),
        "end_lat": semicircles_to_degrees(end_lat) if end_lat is not None else None,
        "distance": distance / 100.0 if distance is not None else None,
        "average_speed": average_speed,
    }


def iter_fit_files(path):
    if path.is_file():
        return [path]
    return sorted(path.rglob("*.fit"))


def main(argv):
    if len(argv) != 2:
        print("Usage: deterred-fit-parse.py <fit-dir-or-file>", file=sys.stderr)
        return 2

    root = Path(argv[1]).expanduser()
    items = []
    for fit_file in iter_fit_files(root):
        try:
            parsed = parse_fit(fit_file)
        except Exception as exc:
            print(f"Failed to parse {fit_file}: {exc}", file=sys.stderr)
            return 1
        if parsed is not None:
            items.append(parsed)

    json.dump(items, sys.stdout, ensure_ascii=True)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
