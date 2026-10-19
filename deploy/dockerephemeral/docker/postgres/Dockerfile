# Dev postgres with pg_partman (>= 5.5.0, needed for UUIDv7 time decoding and
# part_config.maintenance_last_run). PGDG (preconfigured in the official
# Debian-based image) ships postgresql-17-partman 5.5.0 for trixie.
FROM postgres:17.11-trixie

RUN apt-get update \
    && apt-get install -y --no-install-recommends postgresql-17-partman \
    && rm -rf /var/lib/apt/lists/*
