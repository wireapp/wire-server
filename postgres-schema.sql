-- automatically generated with `make postgres-schema`

------------------------------------------------------------------------------------------
-- Database: backendA

--
-- PostgreSQL database dump
--

-- Dumped from database version 17.5
-- Dumped by pg_dump version 17.5

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
-- Name: public; Type: SCHEMA; Schema: -; Owner: wire-server
--

-- *not* creating schema, since initdb creates it


ALTER SCHEMA public OWNER TO "wire-server";

--
-- Name: SCHEMA public; Type: COMMENT; Schema: -; Owner: wire-server
--

COMMENT ON SCHEMA public IS '';


SET default_tablespace = '';

SET default_table_access_method = heap;

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
-- Name: schema_migrations; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.schema_migrations (
    filename character varying(512) NOT NULL,
    checksum character varying(32) NOT NULL,
    executed_at timestamp without time zone DEFAULT now() NOT NULL
);


ALTER TABLE public.schema_migrations OWNER TO "wire-server";

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
-- Name: user_group_member; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.user_group_member (
    user_group_id uuid NOT NULL,
    user_id uuid NOT NULL
);

--
-- Name: user_group_channels; Type: TABLE; Schema: public; Owner: wire-server
--

CREATE TABLE public.user_group_channels (
    user_group_id uuid NOT NULL,
    channel_ids uuid[] NOT NULL
);


ALTER TABLE public.user_group_channels OWNER TO "wire-server";

--
-- Name: collaborators collaborators_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.collaborators
    ADD CONSTRAINT collaborators_pkey PRIMARY KEY (user_id, team_id);


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
-- Name: user_group_channels user_group_member_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.user_group_channels
    ADD CONSTRAINT user_group_channels_pkey PRIMARY KEY (user_group_id);


--
-- Name: user_group user_group_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.user_group
    ADD CONSTRAINT user_group_pkey PRIMARY KEY (team_id, id);


--
-- Name: collaborators_team_id_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX collaborators_team_id_idx ON public.collaborators USING btree (team_id);


--
-- Name: collaborators_user_id_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX collaborators_user_id_idx ON public.collaborators USING btree (user_id);


--
-- Name: user_group_member fk_user_group; Type: FK CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.user_group_member
    ADD CONSTRAINT fk_user_group FOREIGN KEY (user_group_id) REFERENCES public.user_group(id) ON DELETE CASCADE;


--
-- Name: user_group_channels fk_user_group; Type: FK CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.user_group_channels
    ADD CONSTRAINT fk_user_group_channels FOREIGN KEY (user_group_id) REFERENCES public.user_group(id) ON DELETE CASCADE;


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: wire-server
--

REVOKE USAGE ON SCHEMA public FROM PUBLIC;


--
-- PostgreSQL database dump complete
--
