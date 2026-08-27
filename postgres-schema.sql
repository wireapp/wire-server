-- automatically generated with `make postgres-schema`

------------------------------------------------------------------------------------------
-- Database: backendA

--
-- PostgreSQL database dump
--

\restrict 79bbfb4630959c48307653a5cd3d83f2582b3c2210f75f10d79e3ebf0015620

-- Dumped from database version 17.10
-- Dumped by pg_dump version 17.10

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET transaction_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: arbiter; Type: SCHEMA; Schema: -; Owner: wire-server
--

CREATE SCHEMA arbiter;


ALTER SCHEMA arbiter OWNER TO "wire-server";

--
-- Name: public; Type: SCHEMA; Schema: -; Owner: wire-server
--

-- *not* creating schema, since initdb creates it


ALTER SCHEMA public OWNER TO "wire-server";

--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: wire-server
--

COMMENT ON SCHEMA public IS '';


--
-- Name: recurrence_frequency; Type: TYPE; Schema: public; Owner: wire-server
--

CREATE TYPE public.recurrence_frequency AS ENUM (
    'daily',
    'weekly',
    'monthly',
    'yearly'
);


ALTER TYPE public.recurrence_frequency OWNER TO "wire-server";

--
-- Name: maintain_conversations_groups_delete(); Type: FUNCTION; Schema: arbiter; Owner: wire-server
--

CREATE FUNCTION arbiter.maintain_conversations_groups_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM old_table WHERE group_key IS NOT NULL LIMIT 1) THEN
    RETURN NULL;
  END IF;

  -- Lock group rows in group_key order to avoid deadlock with concurrent triggers.
  PERFORM 1 FROM "arbiter"."conversations_groups" g
  WHERE g.group_key IN (SELECT group_key FROM old_table WHERE group_key IS NOT NULL)
  ORDER BY g.group_key
  FOR UPDATE;

  UPDATE "arbiter"."conversations_groups" g
  SET job_count = g.job_count - sub.removed_count,
      min_priority = COALESCE(sub.new_min_priority, g.min_priority),
      min_id = COALESCE(sub.new_min_id, g.min_id),
      ready_count = GREATEST(0, g.ready_count - sub.removed_ready_count),
      next_due = sub.new_next_due,
      in_flight_until = CASE
        WHEN sub.had_inflight THEN sub.surviving_ift
        ELSE g.in_flight_until
      END
  FROM (
    SELECT d.group_key, d.removed_count, d.removed_ready_count, d.had_inflight,
      MIN(t.priority) AS new_min_priority,
      MIN(t.id) AS new_min_id,
      MIN(t.not_visible_until) FILTER (WHERE t.not_visible_until IS NOT NULL AND NOT t.suspended) AS new_next_due,
      MAX(t.not_visible_until) FILTER (WHERE t.not_visible_until > NOW() AND NOT t.suspended AND (t.attempts > 0 OR t.throttled_until > NOW())) AS surviving_ift
    FROM (
      SELECT group_key, COUNT(*) AS removed_count,
        COUNT(*) FILTER (WHERE not_visible_until IS NULL AND NOT suspended) AS removed_ready_count,
        bool_or(not_visible_until > NOW() AND NOT suspended AND (attempts > 0 OR throttled_until > NOW())) AS had_inflight
      FROM old_table
      WHERE group_key IS NOT NULL
      GROUP BY group_key
    ) d
    LEFT JOIN "arbiter"."conversations" t ON t.group_key = d.group_key
    GROUP BY d.group_key, d.removed_count, d.removed_ready_count, d.had_inflight
  ) sub
  WHERE g.group_key = sub.group_key;

  DELETE FROM "arbiter"."conversations_groups"
  WHERE job_count <= 0
    AND group_key IN (SELECT group_key FROM old_table WHERE group_key IS NOT NULL);

  RETURN NULL;
END;
$$;


ALTER FUNCTION arbiter.maintain_conversations_groups_delete() OWNER TO "wire-server";

--
-- Name: maintain_conversations_groups_insert(); Type: FUNCTION; Schema: arbiter; Owner: wire-server
--

CREATE FUNCTION arbiter.maintain_conversations_groups_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM new_table WHERE group_key IS NOT NULL LIMIT 1) THEN
    RETURN NULL;
  END IF;

  -- Lock group rows in group_key order to avoid deadlock with concurrent triggers.
  PERFORM 1 FROM "arbiter"."conversations_groups" g
  WHERE g.group_key IN (SELECT group_key FROM new_table WHERE group_key IS NOT NULL)
  ORDER BY g.group_key
  FOR UPDATE;

  INSERT INTO "arbiter"."conversations_groups" (group_key, min_priority, min_id, job_count, ready_count, next_due)
  SELECT group_key,
    MIN(priority),
    MIN(id),
    COUNT(*),
    COUNT(*) FILTER (WHERE not_visible_until IS NULL AND NOT suspended),
    MIN(not_visible_until) FILTER (WHERE not_visible_until IS NOT NULL AND NOT suspended)
  FROM new_table
  WHERE group_key IS NOT NULL
  GROUP BY group_key
  ORDER BY group_key
  ON CONFLICT (group_key) DO UPDATE SET
    min_priority = LEAST("arbiter"."conversations_groups".min_priority, EXCLUDED.min_priority),
    min_id = LEAST("arbiter"."conversations_groups".min_id, EXCLUDED.min_id),
    job_count = "arbiter"."conversations_groups".job_count + EXCLUDED.job_count,
    ready_count = "arbiter"."conversations_groups".ready_count + EXCLUDED.ready_count,
    next_due = LEAST("arbiter"."conversations_groups".next_due, EXCLUDED.next_due),
    in_flight_until = CASE WHEN "arbiter"."conversations_groups".in_flight_until <= NOW()
      THEN NULL ELSE "arbiter"."conversations_groups".in_flight_until END;

  RETURN NULL;
END;
$$;


ALTER FUNCTION arbiter.maintain_conversations_groups_insert() OWNER TO "wire-server";

--
-- Name: maintain_conversations_groups_update(); Type: FUNCTION; Schema: arbiter; Owner: wire-server
--

CREATE FUNCTION arbiter.maintain_conversations_groups_update() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM new_table WHERE group_key IS NOT NULL LIMIT 1
  ) AND NOT EXISTS (
    SELECT 1 FROM old_table WHERE group_key IS NOT NULL LIMIT 1
  ) THEN
    RETURN NULL;
  END IF;

  -- Lock group rows (old and new) in group_key order to avoid deadlock with concurrent triggers.
  PERFORM 1 FROM "arbiter"."conversations_groups" g
  WHERE g.group_key IN (
    SELECT group_key FROM new_table WHERE group_key IS NOT NULL
    UNION
    SELECT group_key FROM old_table WHERE group_key IS NOT NULL
  )
  ORDER BY g.group_key
  FOR UPDATE;

  -- Step 1: Full rescan - recompute in_flight_until when not_visible_until decreases or suspended changes
  UPDATE "arbiter"."conversations_groups" g
  SET in_flight_until = sub.new_ift
  FROM (
    SELECT t.group_key,
      MAX(t.not_visible_until) FILTER (
        WHERE t.not_visible_until > NOW() AND NOT t.suspended AND (t.attempts > 0 OR t.throttled_until > NOW())
      ) AS new_ift
    FROM "arbiter"."conversations" t
    WHERE t.group_key IN (
      SELECT n.group_key FROM new_table n
      JOIN old_table o ON o.id = n.id
      WHERE n.group_key IS NOT NULL
        AND (o.not_visible_until IS DISTINCT FROM n.not_visible_until
             OR o.suspended IS DISTINCT FROM n.suspended
             OR o.attempts IS DISTINCT FROM n.attempts)
        AND (
          n.not_visible_until > NOW() AND NOT n.suspended AND n.attempts > 0
          AND (o.not_visible_until IS NULL OR o.not_visible_until <= NOW()
               OR n.not_visible_until > o.not_visible_until)
        ) IS NOT TRUE
    )
    GROUP BY t.group_key
  ) sub
  WHERE g.group_key = sub.group_key
    AND g.in_flight_until IS DISTINCT FROM sub.new_ift;

  -- Step 2: group_key change (dedup replace) - remove from old group
  UPDATE "arbiter"."conversations_groups" g
  SET job_count = g.job_count - sub.cnt,
      min_priority = COALESCE(sub.new_min_priority, g.min_priority),
      min_id = COALESCE(sub.new_min_id, g.min_id),
      ready_count = GREATEST(0, g.ready_count - sub.removed_ready_count),
      next_due = sub.new_next_due,
      in_flight_until = CASE
        WHEN sub.had_inflight THEN sub.surviving_ift
        ELSE g.in_flight_until
      END
  FROM (
    SELECT d.group_key, d.cnt, d.removed_ready_count, d.had_inflight,
      MIN(t.priority) AS new_min_priority, MIN(t.id) AS new_min_id,
      MIN(t.not_visible_until) FILTER (WHERE t.not_visible_until IS NOT NULL AND NOT t.suspended) AS new_next_due,
      MAX(t.not_visible_until) FILTER (WHERE t.not_visible_until > NOW() AND NOT t.suspended AND (t.attempts > 0 OR t.throttled_until > NOW())) AS surviving_ift
    FROM (
      SELECT o.group_key, COUNT(*) AS cnt,
        COUNT(*) FILTER (WHERE o.not_visible_until IS NULL AND NOT o.suspended) AS removed_ready_count,
        bool_or(o.not_visible_until > NOW() AND NOT o.suspended AND (o.attempts > 0 OR o.throttled_until > NOW())) AS had_inflight
      FROM old_table o
      JOIN new_table n ON o.id = n.id
      WHERE o.group_key IS NOT NULL
        AND o.group_key IS DISTINCT FROM n.group_key
      GROUP BY o.group_key
    ) d
    LEFT JOIN "arbiter"."conversations" t ON t.group_key = d.group_key
    GROUP BY d.group_key, d.cnt, d.removed_ready_count, d.had_inflight
  ) sub
  WHERE g.group_key = sub.group_key;

  DELETE FROM "arbiter"."conversations_groups"
  WHERE job_count <= 0
    AND group_key IN (
      SELECT o.group_key FROM old_table o
      JOIN new_table n ON o.id = n.id
      WHERE o.group_key IS NOT NULL
        AND o.group_key IS DISTINCT FROM n.group_key
    );

  -- Step 3: group_key change - add to new group
  INSERT INTO "arbiter"."conversations_groups" (group_key, min_priority, min_id, job_count, ready_count, next_due)
  SELECT n.group_key, MIN(n.priority), MIN(n.id), COUNT(*),
    COUNT(*) FILTER (WHERE n.not_visible_until IS NULL AND NOT n.suspended),
    MIN(n.not_visible_until) FILTER (WHERE n.not_visible_until IS NOT NULL AND NOT n.suspended)
  FROM new_table n
  JOIN old_table o ON o.id = n.id
  WHERE n.group_key IS NOT NULL
    AND o.group_key IS DISTINCT FROM n.group_key
  GROUP BY n.group_key
  ORDER BY n.group_key
  ON CONFLICT (group_key) DO UPDATE SET
    min_priority = LEAST("arbiter"."conversations_groups".min_priority, EXCLUDED.min_priority),
    min_id = LEAST("arbiter"."conversations_groups".min_id, EXCLUDED.min_id),
    job_count = "arbiter"."conversations_groups".job_count + EXCLUDED.job_count,
    ready_count = "arbiter"."conversations_groups".ready_count + EXCLUDED.ready_count,
    next_due = LEAST("arbiter"."conversations_groups".next_due, EXCLUDED.next_due);

  -- Step 4: same-group ordering/visibility change - recompute min and next_due.
  UPDATE "arbiter"."conversations_groups" g
  SET min_priority = sub.new_min_priority,
      min_id = sub.new_min_id,
      next_due = sub.new_next_due
  FROM (
    SELECT d.group_key,
      MIN(t.priority) AS new_min_priority,
      MIN(t.id) AS new_min_id,
      MIN(t.not_visible_until) FILTER (WHERE t.not_visible_until IS NOT NULL AND NOT t.suspended) AS new_next_due
    FROM (
      SELECT DISTINCT n.group_key
      FROM new_table n
      JOIN old_table o ON o.id = n.id
      WHERE n.group_key IS NOT NULL
        AND n.group_key IS NOT DISTINCT FROM o.group_key
        AND (n.priority IS DISTINCT FROM o.priority
             OR o.not_visible_until IS DISTINCT FROM n.not_visible_until
             OR o.suspended IS DISTINCT FROM n.suspended)
    ) d
    LEFT JOIN "arbiter"."conversations" t ON t.group_key = d.group_key
    GROUP BY d.group_key
  ) sub
  WHERE g.group_key = sub.group_key
    AND (g.min_priority IS DISTINCT FROM sub.new_min_priority
         OR g.min_id IS DISTINCT FROM sub.new_min_id
         OR g.next_due IS DISTINCT FROM sub.new_next_due);

  -- Step 5: commutative in_flight_until extend and ready_count delta in one write.
  UPDATE "arbiter"."conversations_groups" g
  SET in_flight_until = GREATEST(g.in_flight_until, s.new_ift),
      ready_count = GREATEST(0, g.ready_count + COALESCE(s.delta, 0))
  FROM (
    SELECT COALESCE(ift.group_key, rc.group_key) AS group_key, ift.new_ift, rc.delta
    FROM (
      SELECT n.group_key, MAX(n.not_visible_until) AS new_ift
      FROM new_table n
      JOIN old_table o ON o.id = n.id
      WHERE n.group_key IS NOT NULL
        AND n.not_visible_until > NOW()
        AND NOT n.suspended
        AND n.attempts > 0
        AND (o.not_visible_until IS NULL OR o.not_visible_until <= NOW()
             OR n.not_visible_until > o.not_visible_until)
      GROUP BY n.group_key
    ) ift
    FULL OUTER JOIN (
      SELECT group_key, delta FROM (
        SELECT n.group_key,
          SUM(
            (CASE WHEN n.not_visible_until IS NULL AND NOT n.suspended THEN 1 ELSE 0 END)
            - (CASE WHEN o.not_visible_until IS NULL AND NOT o.suspended THEN 1 ELSE 0 END)
          )::int AS delta
        FROM new_table n
        JOIN old_table o ON o.id = n.id
        WHERE n.group_key IS NOT NULL
          AND n.group_key IS NOT DISTINCT FROM o.group_key
        GROUP BY n.group_key
      ) z
      WHERE delta <> 0
    ) rc ON ift.group_key = rc.group_key
  ) s
  WHERE g.group_key = s.group_key;

  RETURN NULL;
END;
$$;


ALTER FUNCTION arbiter.maintain_conversations_groups_update() OWNER TO "wire-server";

--
-- Name: maintain_meetings_groups_delete(); Type: FUNCTION; Schema: arbiter; Owner: wire-server
--

CREATE FUNCTION arbiter.maintain_meetings_groups_delete() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM old_table WHERE group_key IS NOT NULL LIMIT 1) THEN
    RETURN NULL;
  END IF;

  -- Lock group rows in group_key order to avoid deadlock with concurrent triggers.
  PERFORM 1 FROM "arbiter"."meetings_groups" g
  WHERE g.group_key IN (SELECT group_key FROM old_table WHERE group_key IS NOT NULL)
  ORDER BY g.group_key
  FOR UPDATE;

  UPDATE "arbiter"."meetings_groups" g
  SET job_count = g.job_count - sub.removed_count,
      min_priority = COALESCE(sub.new_min_priority, g.min_priority),
      min_id = COALESCE(sub.new_min_id, g.min_id),
      ready_count = GREATEST(0, g.ready_count - sub.removed_ready_count),
      next_due = sub.new_next_due,
      in_flight_until = CASE
        WHEN sub.had_inflight THEN sub.surviving_ift
        ELSE g.in_flight_until
      END
  FROM (
    SELECT d.group_key, d.removed_count, d.removed_ready_count, d.had_inflight,
      MIN(t.priority) AS new_min_priority,
      MIN(t.id) AS new_min_id,
      MIN(t.not_visible_until) FILTER (WHERE t.not_visible_until IS NOT NULL AND NOT t.suspended) AS new_next_due,
      MAX(t.not_visible_until) FILTER (WHERE t.not_visible_until > NOW() AND NOT t.suspended AND (t.attempts > 0 OR t.throttled_until > NOW())) AS surviving_ift
    FROM (
      SELECT group_key, COUNT(*) AS removed_count,
        COUNT(*) FILTER (WHERE not_visible_until IS NULL AND NOT suspended) AS removed_ready_count,
        bool_or(not_visible_until > NOW() AND NOT suspended AND (attempts > 0 OR throttled_until > NOW())) AS had_inflight
      FROM old_table
      WHERE group_key IS NOT NULL
      GROUP BY group_key
    ) d
    LEFT JOIN "arbiter"."meetings" t ON t.group_key = d.group_key
    GROUP BY d.group_key, d.removed_count, d.removed_ready_count, d.had_inflight
  ) sub
  WHERE g.group_key = sub.group_key;

  DELETE FROM "arbiter"."meetings_groups"
  WHERE job_count <= 0
    AND group_key IN (SELECT group_key FROM old_table WHERE group_key IS NOT NULL);

  RETURN NULL;
END;
$$;


ALTER FUNCTION arbiter.maintain_meetings_groups_delete() OWNER TO "wire-server";

--
-- Name: maintain_meetings_groups_insert(); Type: FUNCTION; Schema: arbiter; Owner: wire-server
--

CREATE FUNCTION arbiter.maintain_meetings_groups_insert() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM new_table WHERE group_key IS NOT NULL LIMIT 1) THEN
    RETURN NULL;
  END IF;

  -- Lock group rows in group_key order to avoid deadlock with concurrent triggers.
  PERFORM 1 FROM "arbiter"."meetings_groups" g
  WHERE g.group_key IN (SELECT group_key FROM new_table WHERE group_key IS NOT NULL)
  ORDER BY g.group_key
  FOR UPDATE;

  INSERT INTO "arbiter"."meetings_groups" (group_key, min_priority, min_id, job_count, ready_count, next_due)
  SELECT group_key,
    MIN(priority),
    MIN(id),
    COUNT(*),
    COUNT(*) FILTER (WHERE not_visible_until IS NULL AND NOT suspended),
    MIN(not_visible_until) FILTER (WHERE not_visible_until IS NOT NULL AND NOT suspended)
  FROM new_table
  WHERE group_key IS NOT NULL
  GROUP BY group_key
  ORDER BY group_key
  ON CONFLICT (group_key) DO UPDATE SET
    min_priority = LEAST("arbiter"."meetings_groups".min_priority, EXCLUDED.min_priority),
    min_id = LEAST("arbiter"."meetings_groups".min_id, EXCLUDED.min_id),
    job_count = "arbiter"."meetings_groups".job_count + EXCLUDED.job_count,
    ready_count = "arbiter"."meetings_groups".ready_count + EXCLUDED.ready_count,
    next_due = LEAST("arbiter"."meetings_groups".next_due, EXCLUDED.next_due),
    in_flight_until = CASE WHEN "arbiter"."meetings_groups".in_flight_until <= NOW()
      THEN NULL ELSE "arbiter"."meetings_groups".in_flight_until END;

  RETURN NULL;
END;
$$;


ALTER FUNCTION arbiter.maintain_meetings_groups_insert() OWNER TO "wire-server";

--
-- Name: maintain_meetings_groups_update(); Type: FUNCTION; Schema: arbiter; Owner: wire-server
--

CREATE FUNCTION arbiter.maintain_meetings_groups_update() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM new_table WHERE group_key IS NOT NULL LIMIT 1
  ) AND NOT EXISTS (
    SELECT 1 FROM old_table WHERE group_key IS NOT NULL LIMIT 1
  ) THEN
    RETURN NULL;
  END IF;

  -- Lock group rows (old and new) in group_key order to avoid deadlock with concurrent triggers.
  PERFORM 1 FROM "arbiter"."meetings_groups" g
  WHERE g.group_key IN (
    SELECT group_key FROM new_table WHERE group_key IS NOT NULL
    UNION
    SELECT group_key FROM old_table WHERE group_key IS NOT NULL
  )
  ORDER BY g.group_key
  FOR UPDATE;

  -- Step 1: Full rescan - recompute in_flight_until when not_visible_until decreases or suspended changes
  UPDATE "arbiter"."meetings_groups" g
  SET in_flight_until = sub.new_ift
  FROM (
    SELECT t.group_key,
      MAX(t.not_visible_until) FILTER (
        WHERE t.not_visible_until > NOW() AND NOT t.suspended AND (t.attempts > 0 OR t.throttled_until > NOW())
      ) AS new_ift
    FROM "arbiter"."meetings" t
    WHERE t.group_key IN (
      SELECT n.group_key FROM new_table n
      JOIN old_table o ON o.id = n.id
      WHERE n.group_key IS NOT NULL
        AND (o.not_visible_until IS DISTINCT FROM n.not_visible_until
             OR o.suspended IS DISTINCT FROM n.suspended
             OR o.attempts IS DISTINCT FROM n.attempts)
        AND (
          n.not_visible_until > NOW() AND NOT n.suspended AND n.attempts > 0
          AND (o.not_visible_until IS NULL OR o.not_visible_until <= NOW()
               OR n.not_visible_until > o.not_visible_until)
        ) IS NOT TRUE
    )
    GROUP BY t.group_key
  ) sub
  WHERE g.group_key = sub.group_key
    AND g.in_flight_until IS DISTINCT FROM sub.new_ift;

  -- Step 2: group_key change (dedup replace) - remove from old group
  UPDATE "arbiter"."meetings_groups" g
  SET job_count = g.job_count - sub.cnt,
      min_priority = COALESCE(sub.new_min_priority, g.min_priority),
      min_id = COALESCE(sub.new_min_id, g.min_id),
      ready_count = GREATEST(0, g.ready_count - sub.removed_ready_count),
      next_due = sub.new_next_due,
      in_flight_until = CASE
        WHEN sub.had_inflight THEN sub.surviving_ift
        ELSE g.in_flight_until
      END
  FROM (
    SELECT d.group_key, d.cnt, d.removed_ready_count, d.had_inflight,
      MIN(t.priority) AS new_min_priority, MIN(t.id) AS new_min_id,
      MIN(t.not_visible_until) FILTER (WHERE t.not_visible_until IS NOT NULL AND NOT t.suspended) AS new_next_due,
      MAX(t.not_visible_until) FILTER (WHERE t.not_visible_until > NOW() AND NOT t.suspended AND (t.attempts > 0 OR t.throttled_until > NOW())) AS surviving_ift
    FROM (
      SELECT o.group_key, COUNT(*) AS cnt,
        COUNT(*) FILTER (WHERE o.not_visible_until IS NULL AND NOT o.suspended) AS removed_ready_count,
        bool_or(o.not_visible_until > NOW() AND NOT o.suspended AND (o.attempts > 0 OR o.throttled_until > NOW())) AS had_inflight
      FROM old_table o
      JOIN new_table n ON o.id = n.id
      WHERE o.group_key IS NOT NULL
        AND o.group_key IS DISTINCT FROM n.group_key
      GROUP BY o.group_key
    ) d
    LEFT JOIN "arbiter"."meetings" t ON t.group_key = d.group_key
    GROUP BY d.group_key, d.cnt, d.removed_ready_count, d.had_inflight
  ) sub
  WHERE g.group_key = sub.group_key;

  DELETE FROM "arbiter"."meetings_groups"
  WHERE job_count <= 0
    AND group_key IN (
      SELECT o.group_key FROM old_table o
      JOIN new_table n ON o.id = n.id
      WHERE o.group_key IS NOT NULL
        AND o.group_key IS DISTINCT FROM n.group_key
    );

  -- Step 3: group_key change - add to new group
  INSERT INTO "arbiter"."meetings_groups" (group_key, min_priority, min_id, job_count, ready_count, next_due)
  SELECT n.group_key, MIN(n.priority), MIN(n.id), COUNT(*),
    COUNT(*) FILTER (WHERE n.not_visible_until IS NULL AND NOT n.suspended),
    MIN(n.not_visible_until) FILTER (WHERE n.not_visible_until IS NOT NULL AND NOT n.suspended)
  FROM new_table n
  JOIN old_table o ON o.id = n.id
  WHERE n.group_key IS NOT NULL
    AND o.group_key IS DISTINCT FROM n.group_key
  GROUP BY n.group_key
  ORDER BY n.group_key
  ON CONFLICT (group_key) DO UPDATE SET
    min_priority = LEAST("arbiter"."meetings_groups".min_priority, EXCLUDED.min_priority),
    min_id = LEAST("arbiter"."meetings_groups".min_id, EXCLUDED.min_id),
    job_count = "arbiter"."meetings_groups".job_count + EXCLUDED.job_count,
    ready_count = "arbiter"."meetings_groups".ready_count + EXCLUDED.ready_count,
    next_due = LEAST("arbiter"."meetings_groups".next_due, EXCLUDED.next_due);

  -- Step 4: same-group ordering/visibility change - recompute min and next_due.
  UPDATE "arbiter"."meetings_groups" g
  SET min_priority = sub.new_min_priority,
      min_id = sub.new_min_id,
      next_due = sub.new_next_due
  FROM (
    SELECT d.group_key,
      MIN(t.priority) AS new_min_priority,
      MIN(t.id) AS new_min_id,
      MIN(t.not_visible_until) FILTER (WHERE t.not_visible_until IS NOT NULL AND NOT t.suspended) AS new_next_due
    FROM (
      SELECT DISTINCT n.group_key
      FROM new_table n
      JOIN old_table o ON o.id = n.id
      WHERE n.group_key IS NOT NULL
        AND n.group_key IS NOT DISTINCT FROM o.group_key
        AND (n.priority IS DISTINCT FROM o.priority
             OR o.not_visible_until IS DISTINCT FROM n.not_visible_until
             OR o.suspended IS DISTINCT FROM n.suspended)
    ) d
    LEFT JOIN "arbiter"."meetings" t ON t.group_key = d.group_key
    GROUP BY d.group_key
  ) sub
  WHERE g.group_key = sub.group_key
    AND (g.min_priority IS DISTINCT FROM sub.new_min_priority
         OR g.min_id IS DISTINCT FROM sub.new_min_id
         OR g.next_due IS DISTINCT FROM sub.new_next_due);

  -- Step 5: commutative in_flight_until extend and ready_count delta in one write.
  UPDATE "arbiter"."meetings_groups" g
  SET in_flight_until = GREATEST(g.in_flight_until, s.new_ift),
      ready_count = GREATEST(0, g.ready_count + COALESCE(s.delta, 0))
  FROM (
    SELECT COALESCE(ift.group_key, rc.group_key) AS group_key, ift.new_ift, rc.delta
    FROM (
      SELECT n.group_key, MAX(n.not_visible_until) AS new_ift
      FROM new_table n
      JOIN old_table o ON o.id = n.id
      WHERE n.group_key IS NOT NULL
        AND n.not_visible_until > NOW()
        AND NOT n.suspended
        AND n.attempts > 0
        AND (o.not_visible_until IS NULL OR o.not_visible_until <= NOW()
             OR n.not_visible_until > o.not_visible_until)
      GROUP BY n.group_key
    ) ift
    FULL OUTER JOIN (
      SELECT group_key, delta FROM (
        SELECT n.group_key,
          SUM(
            (CASE WHEN n.not_visible_until IS NULL AND NOT n.suspended THEN 1 ELSE 0 END)
            - (CASE WHEN o.not_visible_until IS NULL AND NOT o.suspended THEN 1 ELSE 0 END)
          )::int AS delta
        FROM new_table n
        JOIN old_table o ON o.id = n.id
        WHERE n.group_key IS NOT NULL
          AND n.group_key IS NOT DISTINCT FROM o.group_key
        GROUP BY n.group_key
      ) z
      WHERE delta <> 0
    ) rc ON ift.group_key = rc.group_key
  ) s
  WHERE g.group_key = s.group_key;

  RETURN NULL;
END;
$$;


ALTER FUNCTION arbiter.maintain_meetings_groups_update() OWNER TO "wire-server";

--
-- Name: notify_conversations_created(); Type: FUNCTION; Schema: arbiter; Owner: wire-server
--

CREATE FUNCTION arbiter.notify_conversations_created() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  PERFORM pg_notify('conversations_created', '');
  RETURN NEW;
END;
$$;


ALTER FUNCTION arbiter.notify_conversations_created() OWNER TO "wire-server";

--
-- Name: notify_meetings_created(); Type: FUNCTION; Schema: arbiter; Owner: wire-server
--

CREATE FUNCTION arbiter.notify_meetings_created() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  PERFORM pg_notify('meetings_created', '');
  RETURN NEW;
END;
$$;


ALTER FUNCTION arbiter.notify_meetings_created() OWNER TO "wire-server";

--
-- Name: update_updated_at(); Type: FUNCTION; Schema: public; Owner: wire-server
--

CREATE FUNCTION public.update_updated_at() RETURNS trigger
    LANGUAGE plpgsql
    AS $$
BEGIN
  NEW.updated_at = now();
  RETURN NEW;
END;
$$;


ALTER FUNCTION public.update_updated_at() OWNER TO "wire-server";

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: arbiter_concurrency; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE UNLOGGED TABLE arbiter.arbiter_concurrency (
    concurrency_key text NOT NULL,
    concurrency_prefix text NOT NULL,
    in_flight integer DEFAULT 0 NOT NULL
)
WITH (fillfactor='80');


ALTER TABLE arbiter.arbiter_concurrency OWNER TO "wire-server";

--
-- Name: arbiter_concurrency_policies; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.arbiter_concurrency_policies (
    prefix_id text NOT NULL,
    default_limit integer NOT NULL,
    override_limit integer,
    CONSTRAINT arbiter_concurrency_policies_default_limit_check CHECK ((default_limit > 0)),
    CONSTRAINT arbiter_concurrency_policies_override_limit_check CHECK ((override_limit >= 0))
);


ALTER TABLE arbiter.arbiter_concurrency_policies OWNER TO "wire-server";

--
-- Name: arbiter_gates; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.arbiter_gates (
    task_name text NOT NULL,
    last_run_at timestamp with time zone DEFAULT '1970-01-01 00:00:00+00'::timestamp with time zone NOT NULL
);


ALTER TABLE arbiter.arbiter_gates OWNER TO "wire-server";

--
-- Name: arbiter_queues; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.arbiter_queues (
    queue_name text NOT NULL,
    paused boolean DEFAULT false NOT NULL,
    paused_at timestamp with time zone,
    metadata jsonb,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL
);


ALTER TABLE arbiter.arbiter_queues OWNER TO "wire-server";

--
-- Name: arbiter_rate_limit_policies; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.arbiter_rate_limit_policies (
    prefix_id text NOT NULL,
    default_max_tokens double precision NOT NULL,
    default_refill_amount double precision NOT NULL,
    default_interval double precision NOT NULL,
    override_max_tokens double precision,
    override_refill_amount double precision,
    override_interval double precision,
    CONSTRAINT arbiter_rate_limit_policies_default_interval_check CHECK ((default_interval > (0)::double precision)),
    CONSTRAINT arbiter_rate_limit_policies_default_max_tokens_check CHECK ((default_max_tokens >= (0)::double precision)),
    CONSTRAINT arbiter_rate_limit_policies_default_refill_amount_check CHECK ((default_refill_amount >= (0)::double precision)),
    CONSTRAINT arbiter_rate_limit_policies_override_interval_check CHECK ((override_interval > (0)::double precision)),
    CONSTRAINT arbiter_rate_limit_policies_override_max_tokens_check CHECK ((override_max_tokens >= (0)::double precision)),
    CONSTRAINT arbiter_rate_limit_policies_override_refill_amount_check CHECK ((override_refill_amount >= (0)::double precision))
);


ALTER TABLE arbiter.arbiter_rate_limit_policies OWNER TO "wire-server";

--
-- Name: arbiter_rate_limits; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE UNLOGGED TABLE arbiter.arbiter_rate_limits (
    rate_limit_key text NOT NULL,
    policy_prefix text NOT NULL,
    tokens double precision NOT NULL,
    last_refill timestamp with time zone NOT NULL
)
WITH (fillfactor='80');


ALTER TABLE arbiter.arbiter_rate_limits OWNER TO "wire-server";

--
-- Name: arbiter_workers; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.arbiter_workers (
    worker_id uuid NOT NULL,
    queue_name text NOT NULL,
    host_name text,
    worker_count integer,
    started_at timestamp with time zone DEFAULT now() NOT NULL,
    last_heartbeat timestamp with time zone DEFAULT now() NOT NULL,
    shutting_down boolean DEFAULT false NOT NULL,
    paused boolean DEFAULT false NOT NULL,
    stale_threshold_secs double precision DEFAULT 300 NOT NULL,
    metadata jsonb
);


ALTER TABLE arbiter.arbiter_workers OWNER TO "wire-server";

--
-- Name: conversations; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.conversations (
    id bigint NOT NULL,
    payload jsonb NOT NULL,
    group_key text,
    inserted_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone,
    last_attempted_at timestamp with time zone,
    not_visible_until timestamp with time zone,
    attempts integer DEFAULT 0 NOT NULL,
    last_error text,
    priority integer DEFAULT 0 NOT NULL,
    dedup_key text,
    dedup_strategy text,
    max_attempts integer DEFAULT 10,
    parent_id bigint,
    parent_state jsonb,
    suspended boolean DEFAULT false NOT NULL,
    claimed_by uuid,
    rate_limit_key text,
    rate_limit_prefix text,
    throttled_until timestamp with time zone,
    concurrency_key text,
    concurrency_prefix text,
    rate_limit_cost double precision DEFAULT 1 NOT NULL,
    cancel_requested_at timestamp with time zone,
    archive_for integer
)
WITH (fillfactor='100');


ALTER TABLE arbiter.conversations OWNER TO "wire-server";

--
-- Name: conversations_archive; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.conversations_archive (
    id bigint NOT NULL,
    completed_at timestamp with time zone DEFAULT now() NOT NULL,
    archive_expires_at timestamp with time zone NOT NULL,
    job_id bigint NOT NULL,
    claimed_by uuid,
    archive_for integer,
    rate_limit_key text,
    rate_limit_prefix text,
    rate_limit_cost double precision,
    concurrency_key text,
    concurrency_prefix text,
    result jsonb,
    payload jsonb NOT NULL,
    group_key text,
    inserted_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone,
    last_attempted_at timestamp with time zone,
    not_visible_until timestamp with time zone,
    attempts integer DEFAULT 0 NOT NULL,
    last_error text,
    priority integer DEFAULT 0 NOT NULL,
    dedup_key text,
    dedup_strategy text,
    max_attempts integer,
    parent_id bigint,
    parent_state jsonb,
    suspended boolean DEFAULT false NOT NULL
);


ALTER TABLE arbiter.conversations_archive OWNER TO "wire-server";

--
-- Name: conversations_archive_id_seq; Type: SEQUENCE; Schema: arbiter; Owner: wire-server
--

CREATE SEQUENCE arbiter.conversations_archive_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE arbiter.conversations_archive_id_seq OWNER TO "wire-server";

--
-- Name: conversations_archive_id_seq; Type: SEQUENCE OWNED BY; Schema: arbiter; Owner: wire-server
--

ALTER SEQUENCE arbiter.conversations_archive_id_seq OWNED BY arbiter.conversations_archive.id;


--
-- Name: conversations_dlq; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.conversations_dlq (
    id bigint NOT NULL,
    failed_at timestamp with time zone DEFAULT now() NOT NULL,
    job_id bigint NOT NULL,
    payload jsonb NOT NULL,
    group_key text,
    inserted_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone,
    last_attempted_at timestamp with time zone,
    not_visible_until timestamp with time zone,
    attempts integer DEFAULT 0 NOT NULL,
    last_error text,
    priority integer DEFAULT 0 NOT NULL,
    dedup_key text,
    dedup_strategy text,
    max_attempts integer,
    parent_id bigint,
    parent_state jsonb,
    suspended boolean DEFAULT false NOT NULL,
    claimed_by uuid,
    rate_limit_key text,
    rate_limit_prefix text,
    concurrency_key text,
    concurrency_prefix text,
    rate_limit_cost double precision DEFAULT 1 NOT NULL,
    archive_for integer
);


ALTER TABLE arbiter.conversations_dlq OWNER TO "wire-server";

--
-- Name: conversations_dlq_id_seq; Type: SEQUENCE; Schema: arbiter; Owner: wire-server
--

CREATE SEQUENCE arbiter.conversations_dlq_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE arbiter.conversations_dlq_id_seq OWNER TO "wire-server";

--
-- Name: conversations_dlq_id_seq; Type: SEQUENCE OWNED BY; Schema: arbiter; Owner: wire-server
--

ALTER SEQUENCE arbiter.conversations_dlq_id_seq OWNED BY arbiter.conversations_dlq.id;


--
-- Name: conversations_groups; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.conversations_groups (
    group_key text NOT NULL,
    min_priority integer DEFAULT 0 NOT NULL,
    min_id bigint DEFAULT 0 NOT NULL,
    job_count integer DEFAULT 0 NOT NULL,
    in_flight_until timestamp with time zone,
    ready_count integer DEFAULT 0 NOT NULL,
    next_due timestamp with time zone
);


ALTER TABLE arbiter.conversations_groups OWNER TO "wire-server";

--
-- Name: conversations_id_seq; Type: SEQUENCE; Schema: arbiter; Owner: wire-server
--

CREATE SEQUENCE arbiter.conversations_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE arbiter.conversations_id_seq OWNER TO "wire-server";

--
-- Name: conversations_id_seq; Type: SEQUENCE OWNED BY; Schema: arbiter; Owner: wire-server
--

ALTER SEQUENCE arbiter.conversations_id_seq OWNED BY arbiter.conversations.id;


--
-- Name: conversations_results; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.conversations_results (
    parent_id bigint NOT NULL,
    child_id bigint NOT NULL,
    result jsonb NOT NULL
);


ALTER TABLE arbiter.conversations_results OWNER TO "wire-server";

--
-- Name: cron_schedules; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.cron_schedules (
    name text NOT NULL,
    default_expression text NOT NULL,
    default_overlap text NOT NULL,
    override_expression text,
    override_overlap text,
    enabled boolean DEFAULT true NOT NULL,
    last_fired_at timestamp with time zone,
    last_checked_at timestamp with time zone,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    default_timezone text,
    override_timezone text,
    queue_name text DEFAULT 'pre-migration'::text NOT NULL,
    run_requested_at timestamp with time zone,
    last_manual_run_at timestamp with time zone,
    CONSTRAINT cron_schedules_default_overlap_check CHECK ((default_overlap = ANY (ARRAY['SkipOverlap'::text, 'AllowOverlap'::text]))),
    CONSTRAINT cron_schedules_override_overlap_check CHECK (((override_overlap IS NULL) OR (override_overlap = ANY (ARRAY['SkipOverlap'::text, 'AllowOverlap'::text]))))
);


ALTER TABLE arbiter.cron_schedules OWNER TO "wire-server";

--
-- Name: meetings; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.meetings (
    id bigint NOT NULL,
    payload jsonb NOT NULL,
    group_key text,
    inserted_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone,
    last_attempted_at timestamp with time zone,
    not_visible_until timestamp with time zone,
    attempts integer DEFAULT 0 NOT NULL,
    last_error text,
    priority integer DEFAULT 0 NOT NULL,
    dedup_key text,
    dedup_strategy text,
    max_attempts integer DEFAULT 10,
    parent_id bigint,
    parent_state jsonb,
    suspended boolean DEFAULT false NOT NULL,
    claimed_by uuid,
    rate_limit_key text,
    rate_limit_prefix text,
    throttled_until timestamp with time zone,
    concurrency_key text,
    concurrency_prefix text,
    rate_limit_cost double precision DEFAULT 1 NOT NULL,
    cancel_requested_at timestamp with time zone,
    archive_for integer
)
WITH (fillfactor='100');


ALTER TABLE arbiter.meetings OWNER TO "wire-server";

--
-- Name: meetings_archive; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.meetings_archive (
    id bigint NOT NULL,
    completed_at timestamp with time zone DEFAULT now() NOT NULL,
    archive_expires_at timestamp with time zone NOT NULL,
    job_id bigint NOT NULL,
    claimed_by uuid,
    archive_for integer,
    rate_limit_key text,
    rate_limit_prefix text,
    rate_limit_cost double precision,
    concurrency_key text,
    concurrency_prefix text,
    result jsonb,
    payload jsonb NOT NULL,
    group_key text,
    inserted_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone,
    last_attempted_at timestamp with time zone,
    not_visible_until timestamp with time zone,
    attempts integer DEFAULT 0 NOT NULL,
    last_error text,
    priority integer DEFAULT 0 NOT NULL,
    dedup_key text,
    dedup_strategy text,
    max_attempts integer,
    parent_id bigint,
    parent_state jsonb,
    suspended boolean DEFAULT false NOT NULL
);


ALTER TABLE arbiter.meetings_archive OWNER TO "wire-server";

--
-- Name: meetings_archive_id_seq; Type: SEQUENCE; Schema: arbiter; Owner: wire-server
--

CREATE SEQUENCE arbiter.meetings_archive_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE arbiter.meetings_archive_id_seq OWNER TO "wire-server";

--
-- Name: meetings_archive_id_seq; Type: SEQUENCE OWNED BY; Schema: arbiter; Owner: wire-server
--

ALTER SEQUENCE arbiter.meetings_archive_id_seq OWNED BY arbiter.meetings_archive.id;


--
-- Name: meetings_dlq; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.meetings_dlq (
    id bigint NOT NULL,
    failed_at timestamp with time zone DEFAULT now() NOT NULL,
    job_id bigint NOT NULL,
    payload jsonb NOT NULL,
    group_key text,
    inserted_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone,
    last_attempted_at timestamp with time zone,
    not_visible_until timestamp with time zone,
    attempts integer DEFAULT 0 NOT NULL,
    last_error text,
    priority integer DEFAULT 0 NOT NULL,
    dedup_key text,
    dedup_strategy text,
    max_attempts integer,
    parent_id bigint,
    parent_state jsonb,
    suspended boolean DEFAULT false NOT NULL,
    claimed_by uuid,
    rate_limit_key text,
    rate_limit_prefix text,
    concurrency_key text,
    concurrency_prefix text,
    rate_limit_cost double precision DEFAULT 1 NOT NULL,
    archive_for integer
);


ALTER TABLE arbiter.meetings_dlq OWNER TO "wire-server";

--
-- Name: meetings_dlq_id_seq; Type: SEQUENCE; Schema: arbiter; Owner: wire-server
--

CREATE SEQUENCE arbiter.meetings_dlq_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE arbiter.meetings_dlq_id_seq OWNER TO "wire-server";

--
-- Name: meetings_dlq_id_seq; Type: SEQUENCE OWNED BY; Schema: arbiter; Owner: wire-server
--

ALTER SEQUENCE arbiter.meetings_dlq_id_seq OWNED BY arbiter.meetings_dlq.id;


--
-- Name: meetings_groups; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.meetings_groups (
    group_key text NOT NULL,
    min_priority integer DEFAULT 0 NOT NULL,
    min_id bigint DEFAULT 0 NOT NULL,
    job_count integer DEFAULT 0 NOT NULL,
    in_flight_until timestamp with time zone,
    ready_count integer DEFAULT 0 NOT NULL,
    next_due timestamp with time zone
);


ALTER TABLE arbiter.meetings_groups OWNER TO "wire-server";

--
-- Name: meetings_id_seq; Type: SEQUENCE; Schema: arbiter; Owner: wire-server
--

CREATE SEQUENCE arbiter.meetings_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER SEQUENCE arbiter.meetings_id_seq OWNER TO "wire-server";

--
-- Name: meetings_id_seq; Type: SEQUENCE OWNED BY; Schema: arbiter; Owner: wire-server
--

ALTER SEQUENCE arbiter.meetings_id_seq OWNED BY arbiter.meetings.id;


--
-- Name: meetings_results; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.meetings_results (
    parent_id bigint NOT NULL,
    child_id bigint NOT NULL,
    result jsonb NOT NULL
);


ALTER TABLE arbiter.meetings_results OWNER TO "wire-server";

--
-- Name: schema_migrations; Type: TABLE; Schema: arbiter; Owner: wire-server
--

CREATE TABLE arbiter.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE arbiter.schema_migrations OWNER TO "wire-server";

--
-- Name: activation_keys; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.activation_keys (
    key text NOT NULL,
    key_type text NOT NULL,
    key_text text NOT NULL,
    code text NOT NULL,
    user_id uuid,
    retries integer NOT NULL,
    expires_at timestamp with time zone NOT NULL
);


ALTER TABLE public.activation_keys OWNER TO "wire-server";

--
-- Name: apps; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.apps (
    user_id uuid NOT NULL,
    team_id uuid NOT NULL,
    metadata json,
    category text DEFAULT 'other'::text NOT NULL,
    description text DEFAULT ''::text NOT NULL,
    creator uuid NOT NULL
);


ALTER TABLE public.apps OWNER TO "wire-server";

--
-- Name: asset; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.asset (
    user_id uuid NOT NULL,
    typ integer NOT NULL,
    key text NOT NULL,
    size integer
);


ALTER TABLE public.asset OWNER TO "wire-server";

--
-- Name: bot_conv; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.bot_conv (
    id uuid NOT NULL,
    conv uuid NOT NULL,
    conv_team uuid
);


ALTER TABLE public.bot_conv OWNER TO "wire-server";

--
-- Name: collaborators; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.collaborators (
    user_id uuid NOT NULL,
    team_id uuid NOT NULL,
    permissions smallint[]
);


ALTER TABLE public.collaborators OWNER TO "wire-server";

--
-- Name: conversation; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.conversation (
    id uuid NOT NULL,
    access integer[],
    access_roles_v2 integer[],
    cells_state integer,
    channel_add_permission integer,
    cipher_suite integer,
    creator uuid,
    epoch bigint,
    epoch_timestamp timestamp with time zone,
    group_conv_type integer,
    group_id bytea,
    message_timer bigint,
    name text,
    protocol integer,
    public_group_state bytea,
    receipt_mode integer,
    team uuid,
    type integer NOT NULL,
    parent_conv uuid,
    history_depth bigint
);


ALTER TABLE public.conversation OWNER TO "wire-server";

--
-- Name: conversation_codes; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.conversation_codes (
    key text NOT NULL,
    conversation uuid NOT NULL,
    password bytea,
    value text NOT NULL,
    expires_at timestamp with time zone NOT NULL,
    target text DEFAULT 'conv'::text NOT NULL
);


ALTER TABLE public.conversation_codes OWNER TO "wire-server";

--
-- Name: conversation_member; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.conversation_member (
    conv uuid NOT NULL,
    "user" uuid NOT NULL,
    conversation_role text,
    hidden boolean,
    hidden_ref text,
    otr_archived boolean,
    otr_archived_ref text,
    otr_muted boolean,
    otr_muted_ref text,
    otr_muted_status integer,
    provider uuid,
    service uuid
);


ALTER TABLE public.conversation_member OWNER TO "wire-server";

--
-- Name: conversation_migration_pending_deletes; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.conversation_migration_pending_deletes (
    typ text NOT NULL,
    id uuid NOT NULL
);


ALTER TABLE public.conversation_migration_pending_deletes OWNER TO "wire-server";

--
-- Name: conversation_out_of_sync; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.conversation_out_of_sync (
    conv_id uuid NOT NULL,
    out_of_sync boolean NOT NULL
);


ALTER TABLE public.conversation_out_of_sync OWNER TO "wire-server";

--
-- Name: deleted_user; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.deleted_user (
    id uuid NOT NULL,
    team uuid,
    created_at timestamp with time zone NOT NULL,
    deleted_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE public.deleted_user OWNER TO "wire-server";

--
-- Name: domain_registration; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.domain_registration (
    domain text NOT NULL,
    authorized_team uuid,
    domain_redirect integer,
    team_invite integer,
    idp_id uuid,
    backend_url bytea,
    team uuid,
    dns_verification_token text,
    ownership_token_hash bytea,
    webapp_url bytea
);


ALTER TABLE public.domain_registration OWNER TO "wire-server";

--
-- Name: domain_registration_challenge; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.domain_registration_challenge (
    id uuid NOT NULL,
    domain text NOT NULL,
    challenge_token_hash bytea NOT NULL,
    dns_verification_token text NOT NULL,
    expires_at timestamp with time zone NOT NULL
);


ALTER TABLE public.domain_registration_challenge OWNER TO "wire-server";

--
-- Name: local_conversation_remote_member; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.local_conversation_remote_member (
    conv uuid NOT NULL,
    user_remote_domain text NOT NULL,
    user_remote_id uuid NOT NULL,
    conversation_role text
);


ALTER TABLE public.local_conversation_remote_member OWNER TO "wire-server";

--
-- Name: meetings; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.meetings (
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    title text NOT NULL,
    creator uuid NOT NULL,
    start_time timestamp with time zone NOT NULL,
    end_time timestamp with time zone NOT NULL,
    recurrence_frequency public.recurrence_frequency,
    recurrence_interval integer,
    recurrence_until timestamp with time zone,
    conversation_id uuid NOT NULL,
    invited_emails text[] DEFAULT '{}'::text[] NOT NULL,
    trial boolean DEFAULT false NOT NULL,
    created_at timestamp with time zone DEFAULT now() NOT NULL,
    updated_at timestamp with time zone DEFAULT now() NOT NULL,
    tzid text NOT NULL,
    CONSTRAINT meetings_recurrence_consistency CHECK ((((recurrence_frequency IS NULL) AND (recurrence_interval IS NULL) AND (recurrence_until IS NULL)) OR ((recurrence_frequency IS NOT NULL) AND (recurrence_interval IS NOT NULL)))),
    CONSTRAINT meetings_title_length CHECK ((length(title) <= 256)),
    CONSTRAINT meetings_title_not_empty CHECK ((length(TRIM(BOTH FROM title)) > 0)),
    CONSTRAINT meetings_valid_time_range CHECK ((end_time > start_time))
);


ALTER TABLE public.meetings OWNER TO "wire-server";

--
-- Name: mls_group_member_client; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.mls_group_member_client (
    group_id bytea NOT NULL,
    user_domain text NOT NULL,
    "user" uuid NOT NULL,
    client text NOT NULL,
    leaf_node_index integer NOT NULL,
    removal_pending boolean NOT NULL
);


ALTER TABLE public.mls_group_member_client OWNER TO "wire-server";

--
-- Name: mls_history_client; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.mls_history_client (
    group_id bytea NOT NULL,
    id uuid NOT NULL,
    leaf_node_index integer NOT NULL,
    removal_pending boolean NOT NULL
);


ALTER TABLE public.mls_history_client OWNER TO "wire-server";

--
-- Name: remote_conversation_local_member; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.remote_conversation_local_member (
    "user" uuid NOT NULL,
    conv_remote_domain text NOT NULL,
    conv_remote_id uuid NOT NULL,
    hidden boolean,
    hidden_ref text,
    otr_archived boolean,
    otr_archived_ref text,
    otr_muted_ref text,
    otr_muted_status integer
);


ALTER TABLE public.remote_conversation_local_member OWNER TO "wire-server";

--
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO "wire-server";

--
-- Name: subconversation; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.subconversation (
    conv_id uuid NOT NULL,
    subconv_id text NOT NULL,
    cipher_suite integer,
    epoch bigint NOT NULL,
    epoch_timestamp timestamp with time zone NOT NULL,
    group_id bytea NOT NULL,
    public_group_state bytea
);


ALTER TABLE public.subconversation OWNER TO "wire-server";

--
-- Name: team_features; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.team_features (
    team uuid NOT NULL,
    feature text NOT NULL,
    config jsonb,
    lock_status integer,
    status integer
);


ALTER TABLE public.team_features OWNER TO "wire-server";

--
-- Name: user_group; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.user_group (
    team_id uuid NOT NULL,
    id uuid DEFAULT gen_random_uuid() NOT NULL,
    name text NOT NULL,
    managed_by integer NOT NULL,
    created_at timestamp with time zone DEFAULT now()
);


ALTER TABLE public.user_group OWNER TO "wire-server";

--
-- Name: user_group_channel; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.user_group_channel (
    user_group_id uuid NOT NULL,
    conv_id uuid NOT NULL
);


ALTER TABLE public.user_group_channel OWNER TO "wire-server";

--
-- Name: user_group_member; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.user_group_member (
    user_group_id uuid NOT NULL,
    user_id uuid NOT NULL
);


ALTER TABLE public.user_group_member OWNER TO "wire-server";

--
-- Name: wire_user; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.wire_user (
    id uuid NOT NULL,
    user_type integer NOT NULL,
    accent_id integer NOT NULL,
    activated boolean NOT NULL,
    country text,
    email text,
    email_unvalidated text,
    expires timestamp with time zone,
    feature_conference_calling integer,
    handle text,
    language text,
    managed_by integer,
    name text NOT NULL,
    password text,
    picture jsonb,
    provider uuid,
    service uuid,
    searchable boolean,
    sso_id jsonb,
    account_status integer,
    supported_protocols integer,
    team uuid,
    text_status text,
    rich_info jsonb,
    created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
    updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
);


ALTER TABLE public.wire_user OWNER TO "wire-server";

--
-- Name: conversations id; Type: DEFAULT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.conversations ALTER COLUMN id SET DEFAULT nextval('arbiter.conversations_id_seq'::regclass);


--
-- Name: conversations_archive id; Type: DEFAULT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.conversations_archive ALTER COLUMN id SET DEFAULT nextval('arbiter.conversations_archive_id_seq'::regclass);


--
-- Name: conversations_dlq id; Type: DEFAULT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.conversations_dlq ALTER COLUMN id SET DEFAULT nextval('arbiter.conversations_dlq_id_seq'::regclass);


--
-- Name: meetings id; Type: DEFAULT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.meetings ALTER COLUMN id SET DEFAULT nextval('arbiter.meetings_id_seq'::regclass);


--
-- Name: meetings_archive id; Type: DEFAULT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.meetings_archive ALTER COLUMN id SET DEFAULT nextval('arbiter.meetings_archive_id_seq'::regclass);


--
-- Name: meetings_dlq id; Type: DEFAULT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.meetings_dlq ALTER COLUMN id SET DEFAULT nextval('arbiter.meetings_dlq_id_seq'::regclass);


--
-- Name: arbiter_concurrency arbiter_concurrency_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.arbiter_concurrency
    ADD CONSTRAINT arbiter_concurrency_pkey PRIMARY KEY (concurrency_key);


--
-- Name: arbiter_concurrency_policies arbiter_concurrency_policies_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.arbiter_concurrency_policies
    ADD CONSTRAINT arbiter_concurrency_policies_pkey PRIMARY KEY (prefix_id);


--
-- Name: arbiter_gates arbiter_gates_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.arbiter_gates
    ADD CONSTRAINT arbiter_gates_pkey PRIMARY KEY (task_name);


--
-- Name: arbiter_queues arbiter_queues_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.arbiter_queues
    ADD CONSTRAINT arbiter_queues_pkey PRIMARY KEY (queue_name);


--
-- Name: arbiter_rate_limit_policies arbiter_rate_limit_policies_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.arbiter_rate_limit_policies
    ADD CONSTRAINT arbiter_rate_limit_policies_pkey PRIMARY KEY (prefix_id);


--
-- Name: arbiter_rate_limits arbiter_rate_limits_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.arbiter_rate_limits
    ADD CONSTRAINT arbiter_rate_limits_pkey PRIMARY KEY (rate_limit_key);


--
-- Name: arbiter_workers arbiter_workers_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.arbiter_workers
    ADD CONSTRAINT arbiter_workers_pkey PRIMARY KEY (worker_id);


--
-- Name: conversations_archive conversations_archive_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.conversations_archive
    ADD CONSTRAINT conversations_archive_pkey PRIMARY KEY (id);


--
-- Name: conversations_dlq conversations_dlq_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.conversations_dlq
    ADD CONSTRAINT conversations_dlq_pkey PRIMARY KEY (id);


--
-- Name: conversations_groups conversations_groups_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.conversations_groups
    ADD CONSTRAINT conversations_groups_pkey PRIMARY KEY (group_key);


--
-- Name: conversations conversations_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.conversations
    ADD CONSTRAINT conversations_pkey PRIMARY KEY (id);


--
-- Name: conversations_results conversations_results_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.conversations_results
    ADD CONSTRAINT conversations_results_pkey PRIMARY KEY (parent_id, child_id);


--
-- Name: cron_schedules cron_schedules_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.cron_schedules
    ADD CONSTRAINT cron_schedules_pkey PRIMARY KEY (name);


--
-- Name: meetings_archive meetings_archive_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.meetings_archive
    ADD CONSTRAINT meetings_archive_pkey PRIMARY KEY (id);


--
-- Name: meetings_dlq meetings_dlq_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.meetings_dlq
    ADD CONSTRAINT meetings_dlq_pkey PRIMARY KEY (id);


--
-- Name: meetings_groups meetings_groups_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.meetings_groups
    ADD CONSTRAINT meetings_groups_pkey PRIMARY KEY (group_key);


--
-- Name: meetings meetings_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.meetings
    ADD CONSTRAINT meetings_pkey PRIMARY KEY (id);


--
-- Name: meetings_results meetings_results_pkey; Type: CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.meetings_results
    ADD CONSTRAINT meetings_results_pkey PRIMARY KEY (parent_id, child_id);


--
-- Name: activation_keys activation_keys_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.activation_keys
    ADD CONSTRAINT activation_keys_pkey PRIMARY KEY (key);


--
-- Name: apps apps_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.apps
    ADD CONSTRAINT apps_pkey PRIMARY KEY (user_id);


--
-- Name: bot_conv bot_conv_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.bot_conv
    ADD CONSTRAINT bot_conv_pkey PRIMARY KEY (id);


--
-- Name: collaborators collaborators_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.collaborators
    ADD CONSTRAINT collaborators_pkey PRIMARY KEY (user_id, team_id);


--
-- Name: conversation_codes conversation_codes_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.conversation_codes
    ADD CONSTRAINT conversation_codes_pkey PRIMARY KEY (key);


--
-- Name: conversation_member conversation_member_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.conversation_member
    ADD CONSTRAINT conversation_member_pkey PRIMARY KEY (conv, "user");


--
-- Name: conversation_migration_pending_deletes conversation_migration_pending_deletes_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.conversation_migration_pending_deletes
    ADD CONSTRAINT conversation_migration_pending_deletes_pkey PRIMARY KEY (typ, id);


--
-- Name: conversation_out_of_sync conversation_out_of_sync_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.conversation_out_of_sync
    ADD CONSTRAINT conversation_out_of_sync_pkey PRIMARY KEY (conv_id);


--
-- Name: conversation conversation_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.conversation
    ADD CONSTRAINT conversation_pkey PRIMARY KEY (id);


--
-- Name: deleted_user deleted_user_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.deleted_user
    ADD CONSTRAINT deleted_user_pkey PRIMARY KEY (id);


--
-- Name: domain_registration_challenge domain_registration_challenge_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.domain_registration_challenge
    ADD CONSTRAINT domain_registration_challenge_pkey PRIMARY KEY (id);


--
-- Name: domain_registration domain_registration_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.domain_registration
    ADD CONSTRAINT domain_registration_pkey PRIMARY KEY (domain);


--
-- Name: local_conversation_remote_member local_conversation_remote_member_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.local_conversation_remote_member
    ADD CONSTRAINT local_conversation_remote_member_pkey PRIMARY KEY (conv, user_remote_domain, user_remote_id);


--
-- Name: meetings meetings_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.meetings
    ADD CONSTRAINT meetings_pkey PRIMARY KEY (id);


--
-- Name: mls_group_member_client mls_group_member_client_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.mls_group_member_client
    ADD CONSTRAINT mls_group_member_client_pkey PRIMARY KEY (group_id, user_domain, "user", client);


--
-- Name: mls_history_client mls_history_client_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.mls_history_client
    ADD CONSTRAINT mls_history_client_pkey PRIMARY KEY (group_id, id);


--
-- Name: remote_conversation_local_member remote_conversation_local_member_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.remote_conversation_local_member
    ADD CONSTRAINT remote_conversation_local_member_pkey PRIMARY KEY ("user", conv_remote_domain, conv_remote_id);


--
-- Name: subconversation subconversation_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.subconversation
    ADD CONSTRAINT subconversation_pkey PRIMARY KEY (conv_id, subconv_id);


--
-- Name: team_features team_features_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.team_features
    ADD CONSTRAINT team_features_pkey PRIMARY KEY (team, feature);


--
-- Name: user_group_channel user_group_channel_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.user_group_channel
    ADD CONSTRAINT user_group_channel_pkey PRIMARY KEY (user_group_id, conv_id);


--
-- Name: user_group user_group_id_key; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.user_group
    ADD CONSTRAINT user_group_id_key UNIQUE (id);


--
-- Name: user_group_member user_group_member_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.user_group_member
    ADD CONSTRAINT user_group_member_pkey PRIMARY KEY (user_group_id, user_id);


--
-- Name: user_group user_group_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.user_group
    ADD CONSTRAINT user_group_pkey PRIMARY KEY (team_id, id);


--
-- Name: wire_user wire_user_handle_key; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.wire_user
    ADD CONSTRAINT wire_user_handle_key UNIQUE (handle);


--
-- Name: wire_user wire_user_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.wire_user
    ADD CONSTRAINT wire_user_pkey PRIMARY KEY (id);


--
-- Name: conversations_adminless_team_id_idx; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX conversations_adminless_team_id_idx ON arbiter.conversations USING btree (((payload #>> '{data,team_id}'::text[]))) WHERE ((claimed_by IS NULL) AND ((payload ->> 'type'::text) = ANY (ARRAY['adminless_setup'::text, 'adminless_deletion'::text, 'adminless_reminder'::text])));


--
-- Name: idx_conversations_archive_completed_at; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_archive_completed_at ON arbiter.conversations_archive USING btree (completed_at DESC);


--
-- Name: idx_conversations_archive_expires_at; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_archive_expires_at ON arbiter.conversations_archive USING btree (archive_expires_at);


--
-- Name: idx_conversations_archive_group_key; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_archive_group_key ON arbiter.conversations_archive USING btree (group_key);


--
-- Name: idx_conversations_archive_job_id; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_archive_job_id ON arbiter.conversations_archive USING btree (job_id);


--
-- Name: idx_conversations_archive_parent_id; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_archive_parent_id ON arbiter.conversations_archive USING btree (parent_id) WHERE (parent_id IS NOT NULL);


--
-- Name: idx_conversations_cancel_requested; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_cancel_requested ON arbiter.conversations USING btree (id) WHERE (cancel_requested_at IS NOT NULL);


--
-- Name: idx_conversations_concurrency; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_concurrency ON arbiter.conversations USING btree (concurrency_key) WHERE (concurrency_key IS NOT NULL);


--
-- Name: idx_conversations_dedup_key; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE UNIQUE INDEX idx_conversations_dedup_key ON arbiter.conversations USING btree (dedup_key) WHERE (dedup_key IS NOT NULL);


--
-- Name: idx_conversations_dlq_failed_at; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_dlq_failed_at ON arbiter.conversations_dlq USING btree (failed_at DESC);


--
-- Name: idx_conversations_dlq_group_key; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_dlq_group_key ON arbiter.conversations_dlq USING btree (group_key);


--
-- Name: idx_conversations_dlq_parent_id; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_dlq_parent_id ON arbiter.conversations_dlq USING btree (parent_id) WHERE (parent_id IS NOT NULL);


--
-- Name: idx_conversations_group_key; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_group_key ON arbiter.conversations USING btree (group_key, priority, id) WHERE (group_key IS NOT NULL);


--
-- Name: idx_conversations_groups_next_due; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_groups_next_due ON arbiter.conversations_groups USING btree (next_due) WHERE (next_due IS NOT NULL);


--
-- Name: idx_conversations_groups_ranking; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_groups_ranking ON arbiter.conversations_groups USING btree (min_priority, min_id) WHERE ((ready_count > 0) AND (in_flight_until IS NULL));


--
-- Name: idx_conversations_parent_id; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_parent_id ON arbiter.conversations USING btree (parent_id) WHERE (parent_id IS NOT NULL);


--
-- Name: idx_conversations_throttled; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_throttled ON arbiter.conversations USING btree (rate_limit_prefix, rate_limit_key) WHERE (throttled_until IS NOT NULL);


--
-- Name: idx_conversations_ungrouped_due; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_ungrouped_due ON arbiter.conversations USING btree (not_visible_until) WHERE ((group_key IS NULL) AND (not_visible_until IS NOT NULL) AND (NOT suspended));


--
-- Name: idx_conversations_ungrouped_ready_ranking; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_conversations_ungrouped_ready_ranking ON arbiter.conversations USING btree (priority, id) WHERE ((group_key IS NULL) AND (not_visible_until IS NULL) AND (NOT suspended));


--
-- Name: idx_meetings_archive_completed_at; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_archive_completed_at ON arbiter.meetings_archive USING btree (completed_at DESC);


--
-- Name: idx_meetings_archive_expires_at; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_archive_expires_at ON arbiter.meetings_archive USING btree (archive_expires_at);


--
-- Name: idx_meetings_archive_group_key; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_archive_group_key ON arbiter.meetings_archive USING btree (group_key);


--
-- Name: idx_meetings_archive_job_id; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_archive_job_id ON arbiter.meetings_archive USING btree (job_id);


--
-- Name: idx_meetings_archive_parent_id; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_archive_parent_id ON arbiter.meetings_archive USING btree (parent_id) WHERE (parent_id IS NOT NULL);


--
-- Name: idx_meetings_cancel_requested; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_cancel_requested ON arbiter.meetings USING btree (id) WHERE (cancel_requested_at IS NOT NULL);


--
-- Name: idx_meetings_concurrency; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_concurrency ON arbiter.meetings USING btree (concurrency_key) WHERE (concurrency_key IS NOT NULL);


--
-- Name: idx_meetings_dedup_key; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE UNIQUE INDEX idx_meetings_dedup_key ON arbiter.meetings USING btree (dedup_key) WHERE (dedup_key IS NOT NULL);


--
-- Name: idx_meetings_dlq_failed_at; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_dlq_failed_at ON arbiter.meetings_dlq USING btree (failed_at DESC);


--
-- Name: idx_meetings_dlq_group_key; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_dlq_group_key ON arbiter.meetings_dlq USING btree (group_key);


--
-- Name: idx_meetings_dlq_parent_id; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_dlq_parent_id ON arbiter.meetings_dlq USING btree (parent_id) WHERE (parent_id IS NOT NULL);


--
-- Name: idx_meetings_group_key; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_group_key ON arbiter.meetings USING btree (group_key, priority, id) WHERE (group_key IS NOT NULL);


--
-- Name: idx_meetings_groups_next_due; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_groups_next_due ON arbiter.meetings_groups USING btree (next_due) WHERE (next_due IS NOT NULL);


--
-- Name: idx_meetings_groups_ranking; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_groups_ranking ON arbiter.meetings_groups USING btree (min_priority, min_id) WHERE ((ready_count > 0) AND (in_flight_until IS NULL));


--
-- Name: idx_meetings_parent_id; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_parent_id ON arbiter.meetings USING btree (parent_id) WHERE (parent_id IS NOT NULL);


--
-- Name: idx_meetings_throttled; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_throttled ON arbiter.meetings USING btree (rate_limit_prefix, rate_limit_key) WHERE (throttled_until IS NOT NULL);


--
-- Name: idx_meetings_ungrouped_due; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_ungrouped_due ON arbiter.meetings USING btree (not_visible_until) WHERE ((group_key IS NULL) AND (not_visible_until IS NOT NULL) AND (NOT suspended));


--
-- Name: idx_meetings_ungrouped_ready_ranking; Type: INDEX; Schema: arbiter; Owner: wire-server
--

CREATE INDEX idx_meetings_ungrouped_ready_ranking ON arbiter.meetings USING btree (priority, id) WHERE ((group_key IS NULL) AND (not_visible_until IS NULL) AND (NOT suspended));


--
-- Name: activation_keys_expires_at_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX activation_keys_expires_at_idx ON public.activation_keys USING btree (expires_at);


--
-- Name: asset_user_id_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX asset_user_id_idx ON public.asset USING btree (user_id);


--
-- Name: bot_conv_conv_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX bot_conv_conv_idx ON public.bot_conv USING btree (conv);


--
-- Name: bot_conv_team_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX bot_conv_team_idx ON public.bot_conv USING btree (conv_team);


--
-- Name: collaborators_team_id_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX collaborators_team_id_idx ON public.collaborators USING btree (team_id);


--
-- Name: collaborators_user_id_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX collaborators_user_id_idx ON public.collaborators USING btree (user_id);


--
-- Name: conversation_codes_expires_at_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX conversation_codes_expires_at_idx ON public.conversation_codes USING btree (expires_at);


--
-- Name: conversation_codes_key_expires_at_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX conversation_codes_key_expires_at_idx ON public.conversation_codes USING btree (key, expires_at);


--
-- Name: conversation_member_user_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX conversation_member_user_idx ON public.conversation_member USING btree ("user");


--
-- Name: conversation_parent_conv_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX conversation_parent_conv_idx ON public.conversation USING btree (parent_conv);


--
-- Name: conversation_team_group_type_lower_name_id_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX conversation_team_group_type_lower_name_id_idx ON public.conversation USING btree (team, group_conv_type, lower(name), id);


--
-- Name: conversation_team_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX conversation_team_idx ON public.conversation USING btree (team);


--
-- Name: domain_registration_authorized_team_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX domain_registration_authorized_team_idx ON public.domain_registration USING btree (authorized_team);


--
-- Name: domain_registration_challenge_expires_at_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX domain_registration_challenge_expires_at_idx ON public.domain_registration_challenge USING btree (expires_at);


--
-- Name: idx_meetings_conversation; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX idx_meetings_conversation ON public.meetings USING btree (conversation_id);


--
-- Name: idx_meetings_creator; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX idx_meetings_creator ON public.meetings USING btree (creator);


--
-- Name: idx_meetings_end_time; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX idx_meetings_end_time ON public.meetings USING btree (end_time);


--
-- Name: idx_meetings_end_time_nonrecurring; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX idx_meetings_end_time_nonrecurring ON public.meetings USING btree (end_time) WHERE (recurrence_frequency IS NULL);


--
-- Name: idx_meetings_recurrence_eff_end; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX idx_meetings_recurrence_eff_end ON public.meetings USING btree (GREATEST(end_time, recurrence_until)) WHERE ((recurrence_frequency IS NOT NULL) AND (recurrence_interval IS NOT NULL) AND (recurrence_until IS NOT NULL));


--
-- Name: idx_meetings_start_time; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX idx_meetings_start_time ON public.meetings USING btree (start_time);


--
-- Name: user_group_member_user_id_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX user_group_member_user_id_idx ON public.user_group_member USING btree (user_id);


--
-- Name: wire_user_service_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX wire_user_service_idx ON public.wire_user USING btree (provider, service);


--
-- Name: conversations conversations_notify_trigger; Type: TRIGGER; Schema: arbiter; Owner: wire-server
--

CREATE TRIGGER conversations_notify_trigger AFTER INSERT ON arbiter.conversations FOR EACH ROW EXECUTE FUNCTION arbiter.notify_conversations_created();


--
-- Name: conversations maintain_conversations_groups_delete; Type: TRIGGER; Schema: arbiter; Owner: wire-server
--

CREATE TRIGGER maintain_conversations_groups_delete AFTER DELETE ON arbiter.conversations REFERENCING OLD TABLE AS old_table FOR EACH STATEMENT EXECUTE FUNCTION arbiter.maintain_conversations_groups_delete();


--
-- Name: conversations maintain_conversations_groups_insert; Type: TRIGGER; Schema: arbiter; Owner: wire-server
--

CREATE TRIGGER maintain_conversations_groups_insert AFTER INSERT ON arbiter.conversations REFERENCING NEW TABLE AS new_table FOR EACH STATEMENT EXECUTE FUNCTION arbiter.maintain_conversations_groups_insert();


--
-- Name: conversations maintain_conversations_groups_update; Type: TRIGGER; Schema: arbiter; Owner: wire-server
--

CREATE TRIGGER maintain_conversations_groups_update AFTER UPDATE ON arbiter.conversations REFERENCING OLD TABLE AS old_table NEW TABLE AS new_table FOR EACH STATEMENT EXECUTE FUNCTION arbiter.maintain_conversations_groups_update();


--
-- Name: meetings maintain_meetings_groups_delete; Type: TRIGGER; Schema: arbiter; Owner: wire-server
--

CREATE TRIGGER maintain_meetings_groups_delete AFTER DELETE ON arbiter.meetings REFERENCING OLD TABLE AS old_table FOR EACH STATEMENT EXECUTE FUNCTION arbiter.maintain_meetings_groups_delete();


--
-- Name: meetings maintain_meetings_groups_insert; Type: TRIGGER; Schema: arbiter; Owner: wire-server
--

CREATE TRIGGER maintain_meetings_groups_insert AFTER INSERT ON arbiter.meetings REFERENCING NEW TABLE AS new_table FOR EACH STATEMENT EXECUTE FUNCTION arbiter.maintain_meetings_groups_insert();


--
-- Name: meetings maintain_meetings_groups_update; Type: TRIGGER; Schema: arbiter; Owner: wire-server
--

CREATE TRIGGER maintain_meetings_groups_update AFTER UPDATE ON arbiter.meetings REFERENCING OLD TABLE AS old_table NEW TABLE AS new_table FOR EACH STATEMENT EXECUTE FUNCTION arbiter.maintain_meetings_groups_update();


--
-- Name: meetings meetings_notify_trigger; Type: TRIGGER; Schema: arbiter; Owner: wire-server
--

CREATE TRIGGER meetings_notify_trigger AFTER INSERT ON arbiter.meetings FOR EACH ROW EXECUTE FUNCTION arbiter.notify_meetings_created();


--
-- Name: wire_user update_user_updated_at; Type: TRIGGER; Schema: public; Owner: wire-server
--

CREATE TRIGGER update_user_updated_at BEFORE UPDATE ON public.wire_user FOR EACH ROW EXECUTE FUNCTION public.update_updated_at();


--
-- Name: conversations_results conversations_results_parent_id_fkey; Type: FK CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.conversations_results
    ADD CONSTRAINT conversations_results_parent_id_fkey FOREIGN KEY (parent_id) REFERENCES arbiter.conversations(id) ON DELETE CASCADE;


--
-- Name: meetings_results meetings_results_parent_id_fkey; Type: FK CONSTRAINT; Schema: arbiter; Owner: wire-server
--

ALTER TABLE ONLY arbiter.meetings_results
    ADD CONSTRAINT meetings_results_parent_id_fkey FOREIGN KEY (parent_id) REFERENCES arbiter.meetings(id) ON DELETE CASCADE;


--
-- Name: bot_conv bot_conv_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.bot_conv
    ADD CONSTRAINT bot_conv_id_fkey FOREIGN KEY (id) REFERENCES public.wire_user(id) ON DELETE CASCADE;


--
-- Name: conversation_member conversation_member_conv_fkey; Type: FK CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.conversation_member
    ADD CONSTRAINT conversation_member_conv_fkey FOREIGN KEY (conv) REFERENCES public.conversation(id) ON DELETE CASCADE;


--
-- Name: conversation_out_of_sync conversation_out_of_sync_conv_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.conversation_out_of_sync
    ADD CONSTRAINT conversation_out_of_sync_conv_id_fkey FOREIGN KEY (conv_id) REFERENCES public.conversation(id) ON DELETE CASCADE;


--
-- Name: conversation conversation_parent_conv_fkey; Type: FK CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.conversation
    ADD CONSTRAINT conversation_parent_conv_fkey FOREIGN KEY (parent_conv) REFERENCES public.conversation(id) ON DELETE CASCADE;


--
-- Name: user_group_member fk_user_group; Type: FK CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.user_group_member
    ADD CONSTRAINT fk_user_group FOREIGN KEY (user_group_id) REFERENCES public.user_group(id) ON DELETE CASCADE;


--
-- Name: user_group_channel fk_user_group_channel; Type: FK CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.user_group_channel
    ADD CONSTRAINT fk_user_group_channel FOREIGN KEY (user_group_id) REFERENCES public.user_group(id) ON DELETE CASCADE;


--
-- Name: local_conversation_remote_member local_conversation_remote_member_conv_fkey; Type: FK CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.local_conversation_remote_member
    ADD CONSTRAINT local_conversation_remote_member_conv_fkey FOREIGN KEY (conv) REFERENCES public.conversation(id) ON DELETE CASCADE;


--
-- Name: subconversation subconversation_conv_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.subconversation
    ADD CONSTRAINT subconversation_conv_id_fkey FOREIGN KEY (conv_id) REFERENCES public.conversation(id) ON DELETE CASCADE;


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: wire-server
--

REVOKE USAGE ON SCHEMA public FROM PUBLIC;


--
-- PostgreSQL database dump complete
--

\unrestrict 79bbfb4630959c48307653a5cd3d83f2582b3c2210f75f10d79e3ebf0015620
