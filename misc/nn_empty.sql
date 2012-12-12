DROP TABLE IF EXISTS nn_request CASCADE;
DROP TABLE IF EXISTS nn_genre CASCADE;
DROP TABLE IF EXISTS nn_map_genre CASCADE;
DROP TABLE IF EXISTS nn_map_author CASCADE;
DROP TABLE IF EXISTS nn_map_area CASCADE;
DROP TABLE IF EXISTS nn_map_engine CASCADE;
DROP TABLE IF EXISTS nn_map_mechanic CASCADE;
DROP TABLE IF EXISTS nn_map_publisher CASCADE;
DROP TABLE IF EXISTS nn_map_side CASCADE;
DROP TABLE IF EXISTS nn_map_party CASCADE;
DROP TABLE IF EXISTS nn_map_nation CASCADE;
DROP TABLE IF EXISTS nn_map_session CASCADE;
DROP TABLE IF EXISTS nn_map_theme CASCADE;
DROP TABLE IF EXISTS nn_game CASCADE;
DROP TABLE IF EXISTS nn_area CASCADE;
DROP TABLE IF EXISTS nn_author CASCADE;
DROP TABLE IF EXISTS nn_engine CASCADE;
DROP TABLE IF EXISTS nn_mechanic CASCADE;
DROP TABLE IF EXISTS nn_publisher CASCADE;
DROP TABLE IF EXISTS nn_side CASCADE;
DROP TABLE IF EXISTS nn_party CASCADE;
DROP TABLE IF EXISTS nn_theme CASCADE;
DROP TABLE IF EXISTS nn_session CASCADE;
DROP TABLE IF EXISTS nn_result CASCADE;
DROP TABLE IF EXISTS nn_user CASCADE;

DROP SEQUENCE IF EXISTS nn_result_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_session_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_genre_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_author_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_area_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_engine_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_game_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_mechanic_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_publisher_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_request_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_side_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_party_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_theme_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_user_id_seq CASCADE;

CREATE SEQUENCE nn_user_id_seq;
ALTER TABLE public.nn_user_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_result_id_seq;
ALTER TABLE public.nn_result_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_session_id_seq;
ALTER TABLE public.nn_session_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_genre_id_seq;
ALTER TABLE public.nn_genre_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_author_id_seq;
ALTER TABLE public.nn_author_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_area_id_seq;
ALTER TABLE public.nn_area_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_engine_id_seq;
ALTER TABLE public.nn_engine_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_game_id_seq;
ALTER TABLE public.nn_game_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_mechanic_id_seq;
ALTER TABLE public.nn_mechanic_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_publisher_id_seq;
ALTER TABLE public.nn_publisher_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_request_id_seq;
ALTER TABLE public.nn_request_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_party_id_seq;
ALTER TABLE public.nn_party_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_side_id_seq;
ALTER TABLE public.nn_side_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_theme_id_seq;
ALTER TABLE public.nn_theme_id_seq OWNER TO nemesis;

CREATE TABLE nn_user (
  id integer PRIMARY KEY DEFAULT nextval('nn_user_id_seq'),
  username varchar(255) NOT NULL default '',
  realname varchar(255) NOT NULL default '',
  email varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_user OWNER TO nemesis;

CREATE TABLE nn_game (
  id integer PRIMARY KEY DEFAULT nextval('nn_game_id_seq'),
  game varchar(255) NOT NULL default '',
  subtitle varchar(255) NOT NULL default '',
  num_players_min integer DEFAULT 0 NOT NULL,
  num_players_max integer DEFAULT 0 NOT NULL,
  gametime_start integer DEFAULT 0 NOT NULL,
  gametime_end integer DEFAULT 0 NOT NULL,
  id_bgg varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_game OWNER TO nemesis;

CREATE TABLE nn_request (
  id integer PRIMARY KEY DEFAULT nextval('nn_request_id_seq'),
  id_user integer NOT NULL REFERENCES nn_user(id),
  id_game integer NOT NULL REFERENCES nn_game(id),
  id_side integer NOT NULL,
  id_engine integer NOT NULL,
  level_min integer NOT NULL,
  level_max integer NOT NULL,
  freq_min integer NOT NULL,
  freq_max integer NOT NULL,
  active boolean NOT NULL default true,
  requested timestamp with time zone NOT NULL default now(),
  commentary varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_request OWNER TO nemesis;

CREATE TABLE nn_author (
  id integer PRIMARY KEY DEFAULT nextval('nn_author_id_seq'),
  author varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_author OWNER TO nemesis;

CREATE TABLE nn_map_author (
  id_game integer NOT NULL REFERENCES nn_game(id),
  id_author integer NOT NULL REFERENCES nn_author(id)
);

ALTER TABLE public.nn_map_author OWNER TO nemesis;

CREATE TABLE nn_genre (
  id integer PRIMARY KEY DEFAULT nextval('nn_genre_id_seq'),
  genre varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_genre OWNER TO nemesis;

CREATE TABLE nn_map_genre (
  id_game integer NOT NULL REFERENCES nn_game(id),
  id_genre integer NOT NULL REFERENCES nn_genre(id)
);

ALTER TABLE public.nn_map_genre OWNER TO nemesis;

CREATE TABLE nn_engine (
  id integer PRIMARY KEY DEFAULT nextval('nn_engine_id_seq'),
  engine varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_engine OWNER TO nemesis;

CREATE TABLE nn_map_engine (
  id_game integer NOT NULL REFERENCES nn_game(id),
  id_engine integer NOT NULL REFERENCES nn_engine(id),
  url varchar(255) NOT NULL default 'http://www.google.com'
);

ALTER TABLE public.nn_map_engine OWNER TO nemesis;

CREATE TABLE nn_theme (
  id integer PRIMARY KEY DEFAULT nextval('nn_theme_id_seq'),
  theme varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_theme OWNER TO nemesis;

CREATE TABLE nn_map_theme (
  id_game integer NOT NULL REFERENCES nn_game(id),
  id_theme integer NOT NULL REFERENCES nn_theme(id)
);

ALTER TABLE public.nn_map_theme OWNER TO nemesis;

CREATE TABLE nn_mechanic (
  id integer PRIMARY KEY DEFAULT nextval('nn_mechanic_id_seq'),
  mechanic varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_mechanic OWNER TO nemesis;

CREATE TABLE nn_map_mechanic (
  id_game integer NOT NULL REFERENCES nn_game(id),
  id_mechanic integer NOT NULL REFERENCES nn_mechanic(id)
);

ALTER TABLE public.nn_map_mechanic OWNER TO nemesis;

CREATE TABLE nn_side (
  id integer PRIMARY KEY DEFAULT nextval('nn_side_id_seq'),
  side varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_side OWNER TO nemesis;

CREATE TABLE nn_map_side (
  id_game integer NOT NULL REFERENCES nn_game(id),
  id_side integer NOT NULL REFERENCES nn_side(id)
);

ALTER TABLE public.nn_map_side OWNER TO nemesis;

CREATE TABLE nn_party (
  id integer PRIMARY KEY DEFAULT nextval('nn_party_id_seq'),
  party varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_party OWNER TO nemesis;

CREATE TABLE nn_map_party (
  id_game integer NOT NULL REFERENCES nn_game(id),
  num_players integer NOT NULL,
  id_party integer NOT NULL REFERENCES nn_party(id)
);

ALTER TABLE public.nn_map_party OWNER TO nemesis;

CREATE TABLE nn_publisher (
  id integer PRIMARY KEY DEFAULT nextval('nn_publisher_id_seq'),
  publisher varchar(255) NOT NULL default '',
  url varchar(255) NOT NULL default 'http://www.google.com'
);

ALTER TABLE public.nn_publisher OWNER TO nemesis;

CREATE TABLE nn_map_publisher (
  id_game integer NOT NULL REFERENCES nn_game(id),
  id_publisher integer NOT NULL REFERENCES nn_publisher(id)
);

ALTER TABLE public.nn_map_publisher OWNER TO nemesis;

CREATE TABLE nn_area (
  id integer PRIMARY KEY DEFAULT nextval('nn_area_id_seq'),
  area varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_area OWNER TO nemesis;

CREATE TABLE nn_map_area (
  id_game integer NOT NULL REFERENCES nn_game(id),
  id_area integer NOT NULL REFERENCES nn_area(id)
);

ALTER TABLE public.nn_map_area OWNER TO nemesis;

CREATE TABLE nn_result (
  id integer PRIMARY KEY DEFAULT nextval('nn_result_id_seq'),
  result varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_result OWNER TO nemesis;

CREATE TABLE nn_session (
  id integer PRIMARY KEY DEFAULT nextval('nn_session_id_seq'),
  id_game integer NOT NULL REFERENCES nn_game(id),
  title varchar(255) NOT NULL default '',
  report text NOT NULL default '',
  confirmed boolean NOT NULL DEFAULT false,
  played timestamp with time zone NOT NULL default now()
);

ALTER TABLE public.nn_session OWNER TO nemesis;

CREATE TABLE nn_map_session (
  id_session integer NOT NULL REFERENCES nn_session(id),
  id_user integer NOT NULL REFERENCES nn_user(id),
  id_party integer NOT NULL REFERENCES nn_party(id),
  id_result integer NOT NULL REFERENCES nn_result(id),
  confirmed boolean NOT NULL DEFAULT false,
  commentary varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_map_session OWNER TO nemesis;

INSERT INTO nn_user (username,realname,email)VALUES('user','Max Mustermann','email@domain.com');
INSERT INTO nn_user (username,realname,email)VALUES('FlashKorten','Sebastian Korten','bass@core10.de');
INSERT INTO nn_result (result)VALUES('Won');
INSERT INTO nn_result (result)VALUES('Draw');
INSERT INTO nn_result (result)VALUES('Lost');
