ARG HASKELL_IMAGE=haskell:9.6.6-slim-bullseye@sha256:d7e9bf8afe0474eaeeceb0dd723f6e33f2bd34d8c999a8b56ad521381ad0e161
FROM ${HASKELL_IMAGE} AS haskell-tools

FROM node:22-bookworm-slim@sha256:83f487e0a63425e5b4d146fb5e5be574bcbe1b7b843d3ebafdd95eaf7767a7e5 AS frontend
RUN npm install -g elm@0.19.1-6

FROM debian:bookworm-slim@sha256:7c7b2c966bc9ee8cedfeef67e0e279108992c77681fa595db4a9d65c06ccc587 AS backend
# Use the existing compiler without installing packages from Bullseye repositories.
COPY --from=haskell-tools /opt/ghc /opt/ghc
COPY --from=haskell-tools /usr/local/bin/cabal /usr/local/bin/cabal
ENV LANG=C.UTF-8 PATH=/opt/ghc/9.6.6/bin:${PATH}
RUN rm -rf /var/lib/apt/lists/* \
 && apt-get -o Acquire::Retries=3 -o APT::Update::Error-Mode=any update \
 && apt-get upgrade -y \
 && apt-get install -y --no-install-recommends build-essential ca-certificates curl git libffi-dev libgmp-dev libnuma-dev libtinfo-dev zlib1g-dev libsqlite3-dev libpq-dev pkg-config python3 python3-venv chromium \
 && install -d /usr/share/postgresql-common/pgdg \
 && curl --fail --silent --show-error https://www.postgresql.org/media/keys/ACCC4CF8.asc -o /usr/share/postgresql-common/pgdg/apt.postgresql.org.asc \
 && echo 'deb [signed-by=/usr/share/postgresql-common/pgdg/apt.postgresql.org.asc] https://apt.postgresql.org/pub/repos/apt bookworm-pgdg main' > /etc/apt/sources.list.d/pgdg.list \
 && apt-get -o Acquire::Retries=3 -o APT::Update::Error-Mode=any update \
 && apt-get install -y --no-install-recommends postgresql-17 \
 && rm -rf /var/lib/apt/lists/*
RUN ghc --version && cabal --version && ghc -e 'putStrLn "Compiler ready"'
COPY --from=frontend /usr/local/bin/node /usr/local/bin/node
COPY --from=frontend /usr/local/lib/node_modules /usr/local/lib/node_modules
RUN ln -s /usr/local/lib/node_modules/elm/bin/elm /usr/local/bin/elm
WORKDIR /src/api
COPY api/ecc-crm-api.cabal api/cabal.project api/cabal.project.freeze ./
RUN cabal update && cabal build all --only-dependencies
COPY api/app ./app
COPY api/src ./src
COPY api/migrations ./migrations
COPY api/test ./test
COPY api/scripts/*.py api/scripts/*.cjs api/scripts/*.sh api/scripts/*.txt ./scripts/
RUN chmod +x scripts/chromium-ci.sh
COPY api/deploy ./deploy
COPY api/docs ./docs
COPY api/web/elm.json ./web/elm.json
COPY api/web/src ./web/src
COPY api/web/public ./web/public
RUN python3 -m venv /opt/ecc-tests \
 && /opt/ecc-tests/bin/pip install --no-cache-dir -r scripts/requirements.txt
ENV PATH="/opt/ecc-tests/bin:${PATH}" \
    CHROME_PATH=/src/api/scripts/chromium-ci.sh \
    ELM_BIN=/usr/local/bin/elm \
    PG_DUMP_BIN=/usr/lib/postgresql/17/bin/pg_dump \
    PG_RESTORE_BIN=/usr/lib/postgresql/17/bin/pg_restore
# The release manifest is sealed only after the existing complete gate succeeds.
# This database is disposable and never uses the hosted database or runtime secrets.
RUN set -eu; \
    install -d -o postgres -g postgres /tmp/ecc-quality-pg /tmp/ecc-quality-socket; \
    runuser -u postgres -- /usr/lib/postgresql/17/bin/initdb -D /tmp/ecc-quality-pg --auth=trust --no-locale --encoding=UTF8 --username=ecc_test; \
    runuser -u postgres -- /usr/lib/postgresql/17/bin/pg_ctl -D /tmp/ecc-quality-pg -l /tmp/ecc-quality-postgres.log -o '-h 127.0.0.1 -p 55432 -k /tmp/ecc-quality-socket' -w start; \
    trap 'runuser -u postgres -- /usr/lib/postgresql/17/bin/pg_ctl -D /tmp/ecc-quality-pg -m fast -w stop' EXIT; \
    /usr/lib/postgresql/17/bin/createdb -h 127.0.0.1 -p 55432 -U ecc_test ecc_test; \
    export ECC_TEST_DATABASE_URL='postgresql://ecc_test@127.0.0.1:55432/ecc_test?sslmode=disable'; \
    bash scripts/check.sh; \
    cp "$(cabal list-bin exe:ecc-crm-api)" /usr/local/bin/ecc-crm-api; \
    WEB_DIST=web/dist ecc-crm-api verify-release

FROM debian:bookworm-slim@sha256:7c7b2c966bc9ee8cedfeef67e0e279108992c77681fa595db4a9d65c06ccc587
RUN apt-get update && apt-get upgrade -y && apt-get install -y --no-install-recommends libsqlite3-0 libpq5 libgmp10 libffi8 zlib1g ca-certificates curl python3 && rm -rf /var/lib/apt/lists/* && useradd --system --uid 10001 app && mkdir -p /data && chown app /data
WORKDIR /app
COPY --from=backend /usr/local/bin/ecc-crm-api /usr/local/bin/
COPY --from=backend /src/api/web/dist ./web
COPY api/migrations ./migrations
COPY scripts/backup.sh scripts/restore.sh scripts/sqlite_backup.py ./scripts/
ENV APP_ENV=production PORT=4000 WEB_DIST=/app/web SEED_DEMO=false DATABASE_PATH=/data/ecc-crm.sqlite3
VOLUME ["/data"]
USER app
EXPOSE 4000
HEALTHCHECK --interval=30s --timeout=5s --start-period=30s CMD curl --fail http://127.0.0.1:${PORT:-4000}/api/ready || exit 1
ENTRYPOINT ["ecc-crm-api"]
