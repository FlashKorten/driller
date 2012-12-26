DROP INDEX IF EXISTS dr_author_index CASCADE;
DROP INDEX IF EXISTS dr_engine_index CASCADE;
DROP INDEX IF EXISTS dr_game_game_index CASCADE;
DROP INDEX IF EXISTS dr_game_year_from_index CASCADE;
DROP INDEX IF EXISTS dr_game_year_upto_index CASCADE;
DROP INDEX IF EXISTS dr_game_latitude_trunc_index CASCADE;
DROP INDEX IF EXISTS dr_game_longitude_trunc_index CASCADE;
DROP INDEX IF EXISTS dr_game_range_index CASCADE;
DROP INDEX IF EXISTS dr_genre_index CASCADE;
DROP INDEX IF EXISTS dr_mechanic_index CASCADE;
DROP INDEX IF EXISTS dr_party_index CASCADE;
DROP INDEX IF EXISTS dr_leader_index CASCADE;
DROP INDEX IF EXISTS dr_publisher_index CASCADE;
DROP INDEX IF EXISTS dr_request_index CASCADE;
DROP INDEX IF EXISTS dr_series_index CASCADE;
DROP INDEX IF EXISTS dr_session_index CASCADE;
DROP INDEX IF EXISTS dr_side_index CASCADE;
DROP INDEX IF EXISTS dr_special_index CASCADE;
DROP INDEX IF EXISTS dr_theme_index CASCADE;
DROP INDEX IF EXISTS dr_user_index CASCADE;

DROP TABLE IF EXISTS dr_author CASCADE;
DROP TABLE IF EXISTS dr_engine CASCADE;
DROP TABLE IF EXISTS dr_game CASCADE;
DROP TABLE IF EXISTS dr_genre CASCADE;
DROP TABLE IF EXISTS dr_map_author CASCADE;
DROP TABLE IF EXISTS dr_map_engine CASCADE;
DROP TABLE IF EXISTS dr_map_genre CASCADE;
DROP TABLE IF EXISTS dr_map_mechanic CASCADE;
DROP TABLE IF EXISTS dr_map_party CASCADE;
DROP TABLE IF EXISTS dr_map_leader CASCADE;
DROP TABLE IF EXISTS dr_map_publisher CASCADE;
DROP TABLE IF EXISTS dr_map_series CASCADE;
DROP TABLE IF EXISTS dr_map_special CASCADE;
DROP TABLE IF EXISTS dr_map_session CASCADE;
DROP TABLE IF EXISTS dr_map_side CASCADE;
DROP TABLE IF EXISTS dr_map_theme CASCADE;
DROP TABLE IF EXISTS dr_mechanic CASCADE;
DROP TABLE IF EXISTS dr_party CASCADE;
DROP TABLE IF EXISTS dr_leader CASCADE;
DROP TABLE IF EXISTS dr_publisher CASCADE;
DROP TABLE IF EXISTS dr_request CASCADE;
DROP TABLE IF EXISTS dr_result CASCADE;
DROP TABLE IF EXISTS dr_series CASCADE;
DROP TABLE IF EXISTS dr_session CASCADE;
DROP TABLE IF EXISTS dr_side CASCADE;
DROP TABLE IF EXISTS dr_special CASCADE;
DROP TABLE IF EXISTS dr_theme CASCADE;
DROP TABLE IF EXISTS dr_user CASCADE;

DROP SEQUENCE IF EXISTS dr_author_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_engine_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_game_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_genre_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_mechanic_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_party_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_leader_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_publisher_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_request_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_series_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_session_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_side_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_special_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_theme_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_user_id_seq CASCADE;

CREATE SEQUENCE dr_user_id_seq;
ALTER TABLE public.dr_user_id_seq OWNER TO nemesis;
CREATE SEQUENCE dr_session_id_seq;
ALTER TABLE public.dr_session_id_seq OWNER TO nemesis;
CREATE SEQUENCE dr_genre_id_seq;
ALTER TABLE public.dr_genre_id_seq OWNER TO nemesis;
CREATE SEQUENCE dr_author_id_seq;
ALTER TABLE public.dr_author_id_seq OWNER TO nemesis;
CREATE SEQUENCE dr_series_id_seq;
ALTER TABLE public.dr_series_id_seq OWNER TO nemesis;
CREATE SEQUENCE dr_engine_id_seq;
ALTER TABLE public.dr_engine_id_seq OWNER TO nemesis;
CREATE SEQUENCE dr_game_id_seq;
ALTER TABLE public.dr_game_id_seq OWNER TO nemesis;
CREATE SEQUENCE dr_mechanic_id_seq;
ALTER TABLE public.dr_mechanic_id_seq OWNER TO nemesis;
CREATE SEQUENCE dr_publisher_id_seq;
ALTER TABLE public.dr_publisher_id_seq OWNER TO nemesis;
CREATE SEQUENCE dr_request_id_seq;
ALTER TABLE public.dr_request_id_seq OWNER TO nemesis;
CREATE SEQUENCE dr_party_id_seq;
ALTER TABLE public.dr_party_id_seq OWNER TO nemesis;
CREATE SEQUENCE dr_leader_id_seq;
ALTER TABLE public.dr_leader_id_seq OWNER TO nemesis;
CREATE SEQUENCE dr_side_id_seq;
ALTER TABLE public.dr_side_id_seq OWNER TO nemesis;
CREATE SEQUENCE dr_special_id_seq;
ALTER TABLE public.dr_special_id_seq OWNER TO nemesis;
CREATE SEQUENCE dr_theme_id_seq;
ALTER TABLE public.dr_theme_id_seq OWNER TO nemesis;

CREATE TABLE dr_user (
  id integer PRIMARY KEY DEFAULT nextval('dr_user_id_seq'),
  username varchar(255) NOT NULL default '',
  realname varchar(255) NOT NULL default '',
  email varchar(255) NOT NULL default '',
  show_email boolean NOT NULL default false
);

ALTER TABLE public.dr_user OWNER TO nemesis;

CREATE INDEX dr_user_index ON dr_user(username ASC);
ALTER INDEX dr_user_index OWNER TO nemesis;

CREATE TABLE dr_game (
  id integer PRIMARY KEY DEFAULT nextval('dr_game_id_seq'),
  id_bgg varchar(255) NOT NULL default '',
  game varchar(255) NOT NULL default '',
  subtitle varchar(255) NOT NULL default '',
  description varchar(2000) NOT NULL default '',
  players_min integer DEFAULT 0 NOT NULL,
  players_max integer DEFAULT 0 NOT NULL,
  gametime_start date DEFAULT '0001-01-01' NOT NULL,
  year_from integer,
  gametime_end date DEFAULT '0001-01-01' NOT NULL,
  year_upto integer,
  latitude double precision,
  latitude_trunc integer,
  longitude double precision,
  longitude_trunc integer,
  range integer DEFAULT 0 NOT NULL,    -- range in kilometers around epicenter
  timescale integer DEFAULT 0 NOT NULL -- hours per turn
);

ALTER TABLE public.dr_game OWNER TO nemesis;

CREATE INDEX dr_game_game_index ON dr_game(game ASC);
ALTER INDEX dr_game_game_index OWNER TO nemesis;
CREATE INDEX dr_game_year_from_index ON dr_game(game ASC);
ALTER INDEX dr_game_year_from_index OWNER TO nemesis;
CREATE INDEX dr_game_year_upto_index ON dr_game(game ASC);
ALTER INDEX dr_game_year_upto_index OWNER TO nemesis;
CREATE INDEX dr_game_latitude_trunc_index ON dr_game(game ASC);
ALTER INDEX dr_game_latitude_trunc_index OWNER TO nemesis;
CREATE INDEX dr_game_longitude_trunc_index ON dr_game(game ASC);
ALTER INDEX dr_game_longitude_trunc_index OWNER TO nemesis;
CREATE INDEX dr_game_range_index ON dr_game(game ASC);
ALTER INDEX dr_game_range_index OWNER TO nemesis;

CREATE TABLE dr_request (
  id integer PRIMARY KEY DEFAULT nextval('dr_request_id_seq'),
  id_user integer NOT NULL REFERENCES dr_user(id),
  id_game integer NOT NULL REFERENCES dr_game(id),
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

ALTER TABLE public.dr_request OWNER TO nemesis;

CREATE INDEX dr_request_index ON dr_request(requested DESC);
ALTER INDEX dr_request_index OWNER TO nemesis;

CREATE TABLE dr_author (
  id integer PRIMARY KEY DEFAULT nextval('dr_author_id_seq'),
  author varchar(255) NOT NULL default ''
);

ALTER TABLE public.dr_author OWNER TO nemesis;

CREATE INDEX dr_author_index ON dr_author(author ASC);
ALTER INDEX dr_author_index OWNER TO nemesis;

CREATE TABLE dr_map_author (
  id_game integer NOT NULL REFERENCES dr_game(id),
  id_author integer NOT NULL REFERENCES dr_author(id)
);

ALTER TABLE public.dr_map_author OWNER TO nemesis;

CREATE TABLE dr_genre (
  id integer PRIMARY KEY DEFAULT nextval('dr_genre_id_seq'),
  genre varchar(255) NOT NULL default ''
);

ALTER TABLE public.dr_genre OWNER TO nemesis;

CREATE INDEX dr_genre_index ON dr_genre(genre ASC);
ALTER INDEX dr_genre_index OWNER TO nemesis;

CREATE TABLE dr_map_genre (
  id_game integer NOT NULL REFERENCES dr_game(id),
  id_genre integer NOT NULL REFERENCES dr_genre(id)
);

ALTER TABLE public.dr_map_genre OWNER TO nemesis;

CREATE TABLE dr_engine (
  id integer PRIMARY KEY DEFAULT nextval('dr_engine_id_seq'),
  engine varchar(255) NOT NULL default ''
);

ALTER TABLE public.dr_engine OWNER TO nemesis;

CREATE INDEX dr_engine_index ON dr_engine(engine ASC);
ALTER INDEX dr_engine_index OWNER TO nemesis;

CREATE TABLE dr_map_engine (
  id_game integer NOT NULL REFERENCES dr_game(id),
  id_engine integer NOT NULL REFERENCES dr_engine(id),
  url varchar(255) NOT NULL default 'http://www.google.com'
);

ALTER TABLE public.dr_map_engine OWNER TO nemesis;

CREATE TABLE dr_theme (
  id integer PRIMARY KEY DEFAULT nextval('dr_theme_id_seq'),
  theme varchar(255) NOT NULL default ''
);

ALTER TABLE public.dr_theme OWNER TO nemesis;

CREATE INDEX dr_theme_index ON dr_theme(theme ASC);
ALTER INDEX dr_theme_index OWNER TO nemesis;

CREATE TABLE dr_map_theme (
  id_game integer NOT NULL REFERENCES dr_game(id),
  id_theme integer NOT NULL REFERENCES dr_theme(id)
);

ALTER TABLE public.dr_map_theme OWNER TO nemesis;

CREATE TABLE dr_mechanic (
  id integer PRIMARY KEY DEFAULT nextval('dr_mechanic_id_seq'),
  mechanic varchar(255) NOT NULL default ''
);

ALTER TABLE public.dr_mechanic OWNER TO nemesis;

CREATE INDEX dr_mechanic_index ON dr_mechanic(mechanic ASC);
ALTER INDEX dr_mechanic_index OWNER TO nemesis;

CREATE TABLE dr_map_mechanic (
  id_game integer NOT NULL REFERENCES dr_game(id),
  id_mechanic integer NOT NULL REFERENCES dr_mechanic(id)
);

ALTER TABLE public.dr_map_mechanic OWNER TO nemesis;

CREATE TABLE dr_side (
  id integer PRIMARY KEY DEFAULT nextval('dr_side_id_seq'),
  side varchar(255) NOT NULL default ''
);

ALTER TABLE public.dr_side OWNER TO nemesis;

CREATE INDEX dr_side_index ON dr_side(side ASC);
ALTER INDEX dr_side_index OWNER TO nemesis;

CREATE TABLE dr_map_side (
  id_game integer NOT NULL REFERENCES dr_game(id),
  id_side integer NOT NULL REFERENCES dr_side(id)
);

ALTER TABLE public.dr_map_side OWNER TO nemesis;

CREATE TABLE dr_special (
  id integer PRIMARY KEY DEFAULT nextval('dr_special_id_seq'),
  special varchar(255) NOT NULL default ''
);

ALTER TABLE public.dr_special OWNER TO nemesis;

CREATE INDEX dr_special_index ON dr_special(special ASC);
ALTER INDEX dr_special_index OWNER TO nemesis;

CREATE TABLE dr_map_special (
  id_game integer NOT NULL REFERENCES dr_game(id),
  id_special integer NOT NULL REFERENCES dr_side(id)
);

ALTER TABLE public.dr_map_special OWNER TO nemesis;

CREATE TABLE dr_party (
  id integer PRIMARY KEY DEFAULT nextval('dr_party_id_seq'),
  party varchar(255) NOT NULL default ''
);

ALTER TABLE public.dr_party OWNER TO nemesis;

CREATE INDEX dr_party_index ON dr_party(party ASC);
ALTER INDEX dr_party_index OWNER TO nemesis;

CREATE TABLE dr_map_party (
  id_game integer NOT NULL REFERENCES dr_game(id),
  num_players integer NOT NULL,
  id_party integer NOT NULL REFERENCES dr_party(id)
);

ALTER TABLE public.dr_map_party OWNER TO nemesis;

CREATE TABLE dr_leader (
  id integer PRIMARY KEY DEFAULT nextval('dr_leader_id_seq'),
  leader varchar(255) NOT NULL default ''
);

ALTER TABLE public.dr_leader OWNER TO nemesis;

CREATE INDEX dr_leader_index ON dr_leader(leader ASC);
ALTER INDEX dr_leader_index OWNER TO nemesis;

CREATE TABLE dr_map_leader (
  id_game integer NOT NULL REFERENCES dr_game(id),
  id_leader integer NOT NULL REFERENCES dr_leader(id)
);

ALTER TABLE public.dr_map_leader OWNER TO nemesis;

CREATE TABLE dr_publisher (
  id integer PRIMARY KEY DEFAULT nextval('dr_publisher_id_seq'),
  publisher varchar(255) NOT NULL default ''
);

ALTER TABLE public.dr_publisher OWNER TO nemesis;

CREATE INDEX dr_publisher_index ON dr_publisher(publisher ASC);
ALTER INDEX dr_publisher_index OWNER TO nemesis;

CREATE TABLE dr_map_publisher (
  id_game integer NOT NULL REFERENCES dr_game(id),
  url varchar(255) NOT NULL default 'http://www.google.com',
  id_publisher integer NOT NULL REFERENCES dr_publisher(id)
);

ALTER TABLE public.dr_map_publisher OWNER TO nemesis;

CREATE TABLE dr_series (
  id integer PRIMARY KEY DEFAULT nextval('dr_series_id_seq'),
  series varchar(255) NOT NULL default ''
);

ALTER TABLE public.dr_series OWNER TO nemesis;

CREATE INDEX dr_series_index ON dr_series(series ASC);
ALTER INDEX dr_series_index OWNER TO nemesis;

CREATE TABLE dr_map_series (
  id_game integer NOT NULL REFERENCES dr_game(id),
  part varchar(255) NOT NULL default '',
  id_series integer NOT NULL REFERENCES dr_series(id)
);

ALTER TABLE public.dr_map_series OWNER TO nemesis;

CREATE TABLE dr_result (
  id integer PRIMARY KEY,
  result varchar(255) NOT NULL default ''
);

ALTER TABLE public.dr_result OWNER TO nemesis;

CREATE TABLE dr_session (
  id integer PRIMARY KEY DEFAULT nextval('dr_session_id_seq'),
  id_game integer NOT NULL REFERENCES dr_game(id),
  title varchar(255) NOT NULL default '',
  report text NOT NULL default '',
  confirmed boolean NOT NULL DEFAULT false,
  played timestamp with time zone NOT NULL default now()
);

ALTER TABLE public.dr_session OWNER TO nemesis;

CREATE INDEX dr_session_index ON dr_session(id_game DESC);
ALTER INDEX dr_session_index OWNER TO nemesis;

CREATE TABLE dr_map_session (
  id_session integer NOT NULL REFERENCES dr_session(id),
  id_user integer NOT NULL REFERENCES dr_user(id),
  id_party integer NOT NULL REFERENCES dr_party(id),
  id_result integer NOT NULL REFERENCES dr_result(id),
  confirmed boolean NOT NULL DEFAULT false,
  commentary varchar(255) NOT NULL default ''
);

ALTER TABLE public.dr_map_session OWNER TO nemesis;

INSERT INTO dr_user (username,realname,email)VALUES('user','Max Mustermann','email@domain.com');
INSERT INTO dr_user (username,realname,email)VALUES('FlashKorten','Sebastian Korten','bass@core10.de');
INSERT INTO dr_result (id,result)VALUES(1,'Won');
INSERT INTO dr_result (id,result)VALUES(2,'Draw');
INSERT INTO dr_result (id,result)VALUES(3,'Lost');
