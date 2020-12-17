--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: company_room; Type: TABLE; Schema: public; Owner: antbuddy; Tablespace: 
--

CREATE TABLE company_room (
    id bigint NOT NULL,
    company_id character varying(250) NOT NULL,
    room_id character varying(250) NOT NULL,
    default_flag smallint NOT NULL
);


ALTER TABLE public.company_room OWNER TO antbuddy;

--
-- Name: company_room_id_seq; Type: SEQUENCE; Schema: public; Owner: antbuddy
--

CREATE SEQUENCE company_room_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.company_room_id_seq OWNER TO antbuddy;

--
-- Name: company_room_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: antbuddy
--

ALTER SEQUENCE company_room_id_seq OWNED BY company_room.id;


--
-- Name: user_active_room; Type: TABLE; Schema: public; Owner: antbuddy; Tablespace: 
--

CREATE TABLE user_active_room (
    userjid character varying(250) NOT NULL,
    resource character varying(250),
    object_jid character varying(250) NOT NULL,
    status_object smallint NOT NULL,
    update_timestamp timestamp with time zone DEFAULT timezone('utc'::text, now()),
    data_timestamp character varying(30) NOT NULL,
    id bigint NOT NULL,
    server_name character varying(100) NOT NULL
);


ALTER TABLE public.user_active_room OWNER TO antbuddy;

--
-- Name: user_active_room_id_seq; Type: SEQUENCE; Schema: public; Owner: antbuddy
--

CREATE SEQUENCE user_active_room_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_active_room_id_seq OWNER TO antbuddy;

--
-- Name: user_active_room_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: antbuddy
--

ALTER SEQUENCE user_active_room_id_seq OWNED BY user_active_room.id;


--
-- Name: user_room; Type: TABLE; Schema: public; Owner: antbuddy; Tablespace: 
--

CREATE TABLE user_room (
    id bigint NOT NULL,
    user_id character varying(250) NOT NULL,
    server character varying(250) NOT NULL,
    room_id character varying(250) NOT NULL,
    service character varying(250) NOT NULL,
    affiliation character varying(250) NOT NULL
);


ALTER TABLE public.user_room OWNER TO antbuddy;

--
-- Name: user_room_id_seq; Type: SEQUENCE; Schema: public; Owner: antbuddy
--

CREATE SEQUENCE user_room_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_room_id_seq OWNER TO antbuddy;

--
-- Name: user_room_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: antbuddy
--

ALTER SEQUENCE user_room_id_seq OWNED BY user_room.id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: antbuddy
--

ALTER TABLE ONLY company_room ALTER COLUMN id SET DEFAULT nextval('company_room_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: antbuddy
--

ALTER TABLE ONLY user_active_room ALTER COLUMN id SET DEFAULT nextval('user_active_room_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: antbuddy
--

ALTER TABLE ONLY user_room ALTER COLUMN id SET DEFAULT nextval('user_room_id_seq'::regclass);


--
-- Name: company_room_pkey; Type: CONSTRAINT; Schema: public; Owner: antbuddy; Tablespace: 
--

ALTER TABLE ONLY company_room
    ADD CONSTRAINT company_room_pkey PRIMARY KEY (id);


--
-- Name: user_active_room_pkey; Type: CONSTRAINT; Schema: public; Owner: antbuddy; Tablespace: 
--

ALTER TABLE ONLY user_active_room
    ADD CONSTRAINT user_active_room_pkey PRIMARY KEY (id);


--
-- Name: user_room_pkey; Type: CONSTRAINT; Schema: public; Owner: antbuddy; Tablespace: 
--

ALTER TABLE ONLY user_room
    ADD CONSTRAINT user_room_pkey PRIMARY KEY (id);


--
-- Name: company_id; Type: INDEX; Schema: public; Owner: antbuddy; Tablespace: 
--

CREATE INDEX company_id ON company_room USING btree (company_id);


--
-- Name: idx_us_object; Type: INDEX; Schema: public; Owner: antbuddy; Tablespace: 
--

CREATE INDEX idx_us_object ON user_active_room USING btree (userjid, server_name, object_jid);


--
-- Name: room_service; Type: INDEX; Schema: public; Owner: antbuddy; Tablespace: 
--

CREATE INDEX room_service ON user_room USING btree (room_id, service);


--
-- Name: user_server; Type: INDEX; Schema: public; Owner: antbuddy; Tablespace: 
--

CREATE INDEX user_server ON user_room USING btree (user_id, server);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

