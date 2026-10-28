CREATE TABLE google_health_session (
    id TEXT NOT NULL PRIMARY KEY,
    kind TEXT NOT NULL CHECK (
        kind IN ('sleep', 'exercise', 'sedentary', 'meditation')
    ),
    activity_type TEXT,
    name TEXT,
    start_timestamp INTEGER NOT NULL,
    end_timestamp INTEGER NOT NULL,
    start_utc_offset INTEGER NOT NULL,
    end_utc_offset INTEGER NOT NULL,
    local_date TEXT NOT NULL,
    data_source TEXT,
    recording_method TEXT,
    algorithm_version TEXT,
    source_updated_at INTEGER,
    minutes_asleep REAL,
    minutes_awake REAL,
    sleep_score REAL,
    restlessness_normalized REAL,
    hr_below_resting_hr REAL,
    steps INTEGER,
    distance_m REAL,
    energy_kcal REAL,
    altitude_m REAL,
    cardio_load REAL,
    heart_rate_avg_bpm REAL,
    heart_rate_max_bpm REAL,
    CHECK (end_timestamp > start_timestamp)
) STRICT;

CREATE TABLE google_health_sleep_stage (
    id TEXT NOT NULL PRIMARY KEY,
    session_id TEXT NOT NULL
        REFERENCES google_health_session(id) ON DELETE CASCADE,
    stage TEXT NOT NULL,
    start_timestamp INTEGER NOT NULL,
    end_timestamp INTEGER NOT NULL,
    start_utc_offset INTEGER NOT NULL,
    end_utc_offset INTEGER NOT NULL,
    algorithm_version TEXT,
    CHECK (end_timestamp > start_timestamp)
) STRICT;

CREATE TABLE google_health_daily (
    date TEXT NOT NULL PRIMARY KEY,
    timezone TEXT NOT NULL,
    steps INTEGER,
    energy_kcal REAL,
    active_energy_kcal REAL,
    distance_m REAL,
    floors INTEGER,
    active_minutes REAL,
    active_zone_minutes REAL,
    resting_heart_rate_bpm REAL,
    hrv_rmssd_ms REAL,
    hrv_deep_rmssd_ms REAL,
    non_rem_heart_rate_bpm REAL,
    respiratory_rate REAL,
    spo2_percent REAL,
    spo2_lower_percent REAL,
    spo2_upper_percent REAL,
    readiness_score REAL,
    readiness_sleep TEXT,
    readiness_hrv TEXT,
    readiness_resting_heart_rate TEXT,
    sleep_breathing_quality TEXT,
    energy_sample_minutes INTEGER
) STRICT;

CREATE INDEX google_health_session_kind_date
    ON google_health_session (kind, local_date);
CREATE INDEX google_health_session_time
    ON google_health_session (start_timestamp, end_timestamp);
CREATE INDEX google_health_stage_session_time
    ON google_health_sleep_stage (session_id, start_timestamp);

CREATE VIEW google_health_session_effective AS
SELECT session.* FROM google_health_session AS session
WHERE session.kind <> 'sleep'
   OR session.algorithm_version IS NULL
   OR session.algorithm_version <> 'sleep_session_v1'
   OR NOT EXISTS (
       SELECT 1 FROM google_health_session AS preferred
       WHERE preferred.kind = 'sleep'
         AND preferred.algorithm_version = 'sleep_session_v2'
         AND preferred.start_timestamp < session.end_timestamp
         AND preferred.end_timestamp > session.start_timestamp
   );
