"IO tests for PostgREST started on the big schema."

import pytest

from config import *
from util import *
from postgrest import *


def test_requests_wait_for_schema_cache_reload(defaultenv):
    "requests that use the schema cache (e.g. resource embedding) wait for the schema cache to reload"

    env = {
        **defaultenv,
        "PGRST_DB_SCHEMAS": "apflora",
        "PGRST_DB_POOL": "2",
        "PGRST_DB_ANON_ROLE": "postgrest_test_anonymous",
        "PGRST_SERVER_TIMING_ENABLED": "true",
    }

    with run(env=env, wait_max_seconds=30) as postgrest:
        # reload the schema cache
        response = postgrest.session.get("/rpc/notify_pgrst")
        assert response.status_code == 204

        postgrest.wait_until_scache_starts_loading()

        response = postgrest.session.get("/tpopmassn?select=*,tpop(*)")
        assert response.status_code == 200

        plan_dur = parse_server_timings_header(response.headers["Server-Timing"])[
            "plan"
        ]
        assert plan_dur > 10000.0


# See: https://github.com/PostgREST/postgrest/issues/3329
def test_should_not_fail_with_stack_overflow(defaultenv):
    "requesting a non-existent relationship should not fail with stack overflow due to fuzzy search of candidates"

    env = {
        **defaultenv,
        "PGRST_DB_SCHEMAS": "apflora",
        "PGRST_DB_POOL": "2",
        "PGRST_DB_ANON_ROLE": "postgrest_test_anonymous",
    }

    with run(env=env, wait_max_seconds=30) as postgrest:
        response = postgrest.session.get("/unknown-table?select=unknown-rel(*)")
        assert response.status_code == 400
        data = response.json()
        assert data["code"] == "PGRST200"
