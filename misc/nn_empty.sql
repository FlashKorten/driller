DROP INDEX IF EXISTS nn_author_index CASCADE;
DROP INDEX IF EXISTS nn_engine_index CASCADE;
DROP INDEX IF EXISTS nn_game_index CASCADE;
DROP INDEX IF EXISTS nn_genre_index CASCADE;
DROP INDEX IF EXISTS nn_mechanic_index CASCADE;
DROP INDEX IF EXISTS nn_party_index CASCADE;
DROP INDEX IF EXISTS nn_leader_index CASCADE;
DROP INDEX IF EXISTS nn_publisher_index CASCADE;
DROP INDEX IF EXISTS nn_request_index CASCADE;
DROP INDEX IF EXISTS nn_series_index CASCADE;
DROP INDEX IF EXISTS nn_session_index CASCADE;
DROP INDEX IF EXISTS nn_side_index CASCADE;
DROP INDEX IF EXISTS nn_special_index CASCADE;
DROP INDEX IF EXISTS nn_theme_index CASCADE;
DROP INDEX IF EXISTS nn_user_index CASCADE;

DROP TABLE IF EXISTS nn_author CASCADE;
DROP TABLE IF EXISTS nn_engine CASCADE;
DROP TABLE IF EXISTS nn_game CASCADE;
DROP TABLE IF EXISTS nn_genre CASCADE;
DROP TABLE IF EXISTS nn_map_author CASCADE;
DROP TABLE IF EXISTS nn_map_engine CASCADE;
DROP TABLE IF EXISTS nn_map_genre CASCADE;
DROP TABLE IF EXISTS nn_map_mechanic CASCADE;
DROP TABLE IF EXISTS nn_map_party CASCADE;
DROP TABLE IF EXISTS nn_map_leader CASCADE;
DROP TABLE IF EXISTS nn_map_publisher CASCADE;
DROP TABLE IF EXISTS nn_map_series CASCADE;
DROP TABLE IF EXISTS nn_map_session CASCADE;
DROP TABLE IF EXISTS nn_map_side CASCADE;
DROP TABLE IF EXISTS nn_map_theme CASCADE;
DROP TABLE IF EXISTS nn_mechanic CASCADE;
DROP TABLE IF EXISTS nn_party CASCADE;
DROP TABLE IF EXISTS nn_leader CASCADE;
DROP TABLE IF EXISTS nn_publisher CASCADE;
DROP TABLE IF EXISTS nn_request CASCADE;
DROP TABLE IF EXISTS nn_result CASCADE;
DROP TABLE IF EXISTS nn_series CASCADE;
DROP TABLE IF EXISTS nn_session CASCADE;
DROP TABLE IF EXISTS nn_side CASCADE;
DROP TABLE IF EXISTS nn_special CASCADE;
DROP TABLE IF EXISTS nn_theme CASCADE;
DROP TABLE IF EXISTS nn_user CASCADE;

DROP SEQUENCE IF EXISTS nn_author_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_engine_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_game_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_genre_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_mechanic_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_party_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_leader_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_publisher_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_request_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_series_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_session_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_side_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_special_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_theme_id_seq CASCADE;
DROP SEQUENCE IF EXISTS nn_user_id_seq CASCADE;

CREATE SEQUENCE nn_user_id_seq;
ALTER TABLE public.nn_user_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_session_id_seq;
ALTER TABLE public.nn_session_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_genre_id_seq;
ALTER TABLE public.nn_genre_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_author_id_seq;
ALTER TABLE public.nn_author_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_series_id_seq;
ALTER TABLE public.nn_series_id_seq OWNER TO nemesis;
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
CREATE SEQUENCE nn_leader_id_seq;
ALTER TABLE public.nn_leader_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_side_id_seq;
ALTER TABLE public.nn_side_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_special_id_seq;
ALTER TABLE public.nn_special_id_seq OWNER TO nemesis;
CREATE SEQUENCE nn_theme_id_seq;
ALTER TABLE public.nn_theme_id_seq OWNER TO nemesis;

CREATE TABLE nn_user (
  id integer PRIMARY KEY DEFAULT nextval('nn_user_id_seq'),
  username varchar(255) NOT NULL default '',
  realname varchar(255) NOT NULL default '',
  email varchar(255) NOT NULL default '',
  show_email boolean NOT NULL default false
);

ALTER TABLE public.nn_user OWNER TO nemesis;

CREATE INDEX nn_user_index ON nn_user(username ASC);
ALTER INDEX nn_user_index OWNER TO nemesis;

CREATE TABLE nn_game (
  id integer PRIMARY KEY DEFAULT nextval('nn_game_id_seq'),
  id_bgg varchar(255) NOT NULL default '',
  game varchar(255) NOT NULL default '',
  subtitle varchar(255) NOT NULL default '',
  description varchar(2000) NOT NULL default '',
  players_min integer DEFAULT 0 NOT NULL,
  players_max integer DEFAULT 0 NOT NULL,
  gametime_start date DEFAULT '0001-01-01' NOT NULL,
  gametime_start_trunc varchar(4),
  gametime_end date DEFAULT '0001-01-01' NOT NULL,
  gametime_end_trunc varchar(4),
  latitude double precision,
  latitude_trunc integer,
  longitude double precision,
  longitude_trunc integer,
  range integer DEFAULT 0 NOT NULL,    -- range in kilometers around epicenter
  timescale integer DEFAULT 0 NOT NULL -- hours per turn
);

ALTER TABLE public.nn_game OWNER TO nemesis;

CREATE INDEX nn_game_index ON nn_game(game ASC);
ALTER INDEX nn_game_index OWNER TO nemesis;

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

CREATE INDEX nn_request_index ON nn_request(requested DESC);
ALTER INDEX nn_request_index OWNER TO nemesis;

CREATE TABLE nn_author (
  id integer PRIMARY KEY DEFAULT nextval('nn_author_id_seq'),
  author varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_author OWNER TO nemesis;

CREATE INDEX nn_author_index ON nn_author(author ASC);
ALTER INDEX nn_author_index OWNER TO nemesis;

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

CREATE INDEX nn_genre_index ON nn_genre(genre ASC);
ALTER INDEX nn_genre_index OWNER TO nemesis;

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

CREATE INDEX nn_engine_index ON nn_engine(engine ASC);
ALTER INDEX nn_engine_index OWNER TO nemesis;

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

CREATE INDEX nn_theme_index ON nn_theme(theme ASC);
ALTER INDEX nn_theme_index OWNER TO nemesis;

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

CREATE INDEX nn_mechanic_index ON nn_mechanic(mechanic ASC);
ALTER INDEX nn_mechanic_index OWNER TO nemesis;

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

CREATE INDEX nn_side_index ON nn_side(side ASC);
ALTER INDEX nn_side_index OWNER TO nemesis;

CREATE TABLE nn_map_side (
  id_game integer NOT NULL REFERENCES nn_game(id),
  id_side integer NOT NULL REFERENCES nn_side(id)
);

ALTER TABLE public.nn_map_side OWNER TO nemesis;

CREATE TABLE nn_special (
  id integer PRIMARY KEY DEFAULT nextval('nn_special_id_seq'),
  special varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_special OWNER TO nemesis;

CREATE INDEX nn_special_index ON nn_special(special ASC);
ALTER INDEX nn_special_index OWNER TO nemesis;

CREATE TABLE nn_map_special (
  id_game integer NOT NULL REFERENCES nn_game(id),
  id_special integer NOT NULL REFERENCES nn_side(id)
);

ALTER TABLE public.nn_map_side OWNER TO nemesis;

CREATE TABLE nn_party (
  id integer PRIMARY KEY DEFAULT nextval('nn_party_id_seq'),
  party varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_party OWNER TO nemesis;

CREATE INDEX nn_party_index ON nn_party(party ASC);
ALTER INDEX nn_party_index OWNER TO nemesis;

CREATE TABLE nn_map_party (
  id_game integer NOT NULL REFERENCES nn_game(id),
  num_players integer NOT NULL,
  id_party integer NOT NULL REFERENCES nn_party(id)
);

ALTER TABLE public.nn_map_party OWNER TO nemesis;

CREATE TABLE nn_leader (
  id integer PRIMARY KEY DEFAULT nextval('nn_leader_id_seq'),
  leader varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_leader OWNER TO nemesis;

CREATE INDEX nn_leader_index ON nn_leader(leader ASC);
ALTER INDEX nn_leader_index OWNER TO nemesis;

CREATE TABLE nn_map_leader (
  id_game integer NOT NULL REFERENCES nn_game(id),
  id_leader integer NOT NULL REFERENCES nn_leader(id)
);

ALTER TABLE public.nn_map_leader OWNER TO nemesis;

CREATE TABLE nn_publisher (
  id integer PRIMARY KEY DEFAULT nextval('nn_publisher_id_seq'),
  publisher varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_publisher OWNER TO nemesis;

CREATE INDEX nn_publisher_index ON nn_publisher(publisher ASC);
ALTER INDEX nn_publisher_index OWNER TO nemesis;

CREATE TABLE nn_map_publisher (
  id_game integer NOT NULL REFERENCES nn_game(id),
  url varchar(255) NOT NULL default 'http://www.google.com',
  id_publisher integer NOT NULL REFERENCES nn_publisher(id)
);

ALTER TABLE public.nn_map_publisher OWNER TO nemesis;

CREATE TABLE nn_series (
  id integer PRIMARY KEY DEFAULT nextval('nn_series_id_seq'),
  series varchar(255) NOT NULL default ''
);

ALTER TABLE public.nn_series OWNER TO nemesis;

CREATE INDEX nn_series_index ON nn_series(series ASC);
ALTER INDEX nn_series_index OWNER TO nemesis;

CREATE TABLE nn_map_series (
  id_game integer NOT NULL REFERENCES nn_game(id),
  part varchar(255) NOT NULL default '',
  id_series integer NOT NULL REFERENCES nn_series(id)
);

ALTER TABLE public.nn_map_series OWNER TO nemesis;

CREATE TABLE nn_result (
  id integer PRIMARY KEY,
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

CREATE INDEX nn_session_index ON nn_session(id_game DESC);
ALTER INDEX nn_session_index OWNER TO nemesis;

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
INSERT INTO nn_result (id,result)VALUES(1,'Won');
INSERT INTO nn_result (id,result)VALUES(2,'Draw');
INSERT INTO nn_result (id,result)VALUES(3,'Lost');
