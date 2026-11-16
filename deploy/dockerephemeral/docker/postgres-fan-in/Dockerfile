FROM postgres:17.11-trixie

RUN apt-get update \
    && apt-get install -y --no-install-recommends postgresql-17-partman \
    && dpkg --compare-versions "$(dpkg-query -W -f='${Version}' postgresql-17-partman)" ge 5.5.0 \
    && rm -rf /var/lib/apt/lists/*
