-- automatically generated with `make postgres-schema`

------------------------------------------------------------------------------------------
-- Database: backendA

--
-- PostgreSQL database dump
--

\restrict 79bbfb4630959c48307653a5cd3d83f2582b3c2210f75f10d79e3ebf0015620

-- Dumped from database version 17.6
-- Dumped by pg_dump version 17.6

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
    parent_conv uuid
);


ALTER TABLE public.conversation OWNER TO "wire-server";

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
    id uuid NOT NULL,
    domain text NOT NULL,
    title text NOT NULL,
    creator uuid NOT NULL,
    start_date timestamp with time zone NOT NULL,
    end_date timestamp with time zone NOT NULL,
    schedule text,
    conversation_id uuid NOT NULL,
    conversation_domain text NOT NULL,
    invited_emails text[] DEFAULT '{}'::text[],
    trial boolean DEFAULT false,
    created_at timestamp with time zone DEFAULT now(),
    updated_at timestamp with time zone DEFAULT now()
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
-- Name: apps apps_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.apps
    ADD CONSTRAINT apps_pkey PRIMARY KEY (user_id);


--
-- Name: collaborators collaborators_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.collaborators
    ADD CONSTRAINT collaborators_pkey PRIMARY KEY (user_id, team_id);


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
-- Name: meetings meetings_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.meetings
    ADD CONSTRAINT meetings_pkey PRIMARY KEY (domain, id);


--
-- Name: local_conversation_remote_member local_conversation_remote_member_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.local_conversation_remote_member
    ADD CONSTRAINT local_conversation_remote_member_pkey PRIMARY KEY (conv, user_remote_domain, user_remote_id);


--
-- Name: mls_group_member_client mls_group_member_client_pkey; Type: CONSTRAINT; Schema: public; Owner: wire-server
--

ALTER TABLE ONLY public.mls_group_member_client
    ADD CONSTRAINT mls_group_member_client_pkey PRIMARY KEY (group_id, user_domain, "user", client);


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
-- Name: collaborators_team_id_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX collaborators_team_id_idx ON public.collaborators USING btree (team_id);


--
-- Name: collaborators_user_id_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX collaborators_user_id_idx ON public.collaborators USING btree (user_id);


--
-- Name: conversation_member_user_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX conversation_member_user_idx ON public.conversation_member USING btree ("user");


--
-- Name: conversation_team_group_type_lower_name_id_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX conversation_team_group_type_lower_name_id_idx ON public.conversation USING btree (team, group_conv_type, lower(name), id);


--
-- Name: conversation_team_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX conversation_team_idx ON public.conversation USING btree (team);


--
-- Name: idx_meetings_conversation; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX idx_meetings_conversation ON public.meetings USING btree (conversation_id, conversation_domain);


--
-- Name: idx_meetings_creator; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX idx_meetings_creator ON public.meetings USING btree (creator);


--
-- Name: idx_meetings_end_date; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX idx_meetings_end_date ON public.meetings USING btree (end_date);


--
-- Name: idx_meetings_start_date; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX idx_meetings_start_date ON public.meetings USING btree (start_date);


--
-- Name: user_group_member_user_id_idx; Type: INDEX; Schema: public; Owner: wire-server
--

CREATE INDEX user_group_member_user_id_idx ON public.user_group_member USING btree (user_id);


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
