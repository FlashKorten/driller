DROP INDEX IF EXISTS dr_author_index CASCADE;
DROP INDEX IF EXISTS dr_engine_index CASCADE;
DROP INDEX IF EXISTS dr_scenario_data_title_index CASCADE;
DROP INDEX IF EXISTS dr_scenario_year_from_index CASCADE;
DROP INDEX IF EXISTS dr_scenario_year_upto_index CASCADE;
DROP INDEX IF EXISTS dr_scenario_latitude_trunc_index CASCADE;
DROP INDEX IF EXISTS dr_scenario_longitude_trunc_index CASCADE;
DROP INDEX IF EXISTS dr_scenario_range_index CASCADE;
DROP INDEX IF EXISTS dr_scenario_timescale_index CASCADE;
DROP INDEX IF EXISTS dr_genre_index CASCADE;
DROP INDEX IF EXISTS dr_mechanic_index CASCADE;
DROP INDEX IF EXISTS dr_party_index CASCADE;
DROP INDEX IF EXISTS dr_leader_index CASCADE;
DROP INDEX IF EXISTS dr_publisher_index CASCADE;
DROP INDEX IF EXISTS dr_series_index CASCADE;
DROP INDEX IF EXISTS dr_side_index CASCADE;
DROP INDEX IF EXISTS dr_special_index CASCADE;
DROP INDEX IF EXISTS dr_theme_index CASCADE;
DROP INDEX IF EXISTS dr_map_author_index CASCADE;
DROP INDEX IF EXISTS dr_map_author_scenario_index CASCADE;
DROP INDEX IF EXISTS dr_map_engine_index CASCADE;
DROP INDEX IF EXISTS dr_map_engine_game_index CASCADE;
DROP INDEX IF EXISTS dr_map_genre_index CASCADE;
DROP INDEX IF EXISTS dr_map_genre_game_index CASCADE;
DROP INDEX IF EXISTS dr_map_leader_index CASCADE;
DROP INDEX IF EXISTS dr_map_leader_game_index CASCADE;
DROP INDEX IF EXISTS dr_map_mechanic_index CASCADE;
DROP INDEX IF EXISTS dr_map_mechanic_game_index CASCADE;
DROP INDEX IF EXISTS dr_map_party_index CASCADE;
DROP INDEX IF EXISTS dr_map_party_scenario_index CASCADE;
DROP INDEX IF EXISTS dr_map_publisher_index CASCADE;
DROP INDEX IF EXISTS dr_map_publisher_game_index CASCADE;
DROP INDEX IF EXISTS dr_map_series_index CASCADE;
DROP INDEX IF EXISTS dr_map_series_game_index CASCADE;
DROP INDEX IF EXISTS dr_map_side_index CASCADE;
DROP INDEX IF EXISTS dr_map_side_scenario_index CASCADE;
DROP INDEX IF EXISTS dr_map_special_index CASCADE;
DROP INDEX IF EXISTS dr_map_special_scenario_index CASCADE;
DROP INDEX IF EXISTS dr_map_theme_index CASCADE;
DROP INDEX IF EXISTS dr_map_theme_game_index CASCADE;

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
DROP TABLE IF EXISTS dr_map_side CASCADE;
DROP TABLE IF EXISTS dr_map_theme CASCADE;
DROP TABLE IF EXISTS dr_mechanic CASCADE;
DROP TABLE IF EXISTS dr_party CASCADE;
DROP TABLE IF EXISTS dr_leader CASCADE;
DROP TABLE IF EXISTS dr_publisher CASCADE;
DROP TABLE IF EXISTS dr_series CASCADE;
DROP TABLE IF EXISTS dr_scenario_data CASCADE;
DROP TABLE IF EXISTS dr_scenario CASCADE;
DROP TABLE IF EXISTS dr_side CASCADE;
DROP TABLE IF EXISTS dr_special CASCADE;
DROP TABLE IF EXISTS dr_theme CASCADE;

DROP SEQUENCE IF EXISTS dr_author_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_engine_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_genre_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_mechanic_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_party_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_leader_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_publisher_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_series_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_side_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_special_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_scenario_id_seq CASCADE;
DROP SEQUENCE IF EXISTS dr_theme_id_seq CASCADE;

CREATE SEQUENCE dr_genre_id_seq;
ALTER TABLE public.dr_genre_id_seq OWNER TO driller;
CREATE SEQUENCE dr_author_id_seq;
ALTER TABLE public.dr_author_id_seq OWNER TO driller;
CREATE SEQUENCE dr_series_id_seq;
ALTER TABLE public.dr_series_id_seq OWNER TO driller;
CREATE SEQUENCE dr_engine_id_seq;
ALTER TABLE public.dr_engine_id_seq OWNER TO driller;
CREATE SEQUENCE dr_mechanic_id_seq;
ALTER TABLE public.dr_mechanic_id_seq OWNER TO driller;
CREATE SEQUENCE dr_publisher_id_seq;
ALTER TABLE public.dr_publisher_id_seq OWNER TO driller;
CREATE SEQUENCE dr_party_id_seq;
ALTER TABLE public.dr_party_id_seq OWNER TO driller;
CREATE SEQUENCE dr_leader_id_seq;
ALTER TABLE public.dr_leader_id_seq OWNER TO driller;
CREATE SEQUENCE dr_side_id_seq;
ALTER TABLE public.dr_side_id_seq OWNER TO driller;
CREATE SEQUENCE dr_special_id_seq;
ALTER TABLE public.dr_special_id_seq OWNER TO driller;
CREATE SEQUENCE dr_scenario_id_seq;
ALTER TABLE public.dr_scenario_id_seq OWNER TO driller;
CREATE SEQUENCE dr_theme_id_seq;
ALTER TABLE public.dr_theme_id_seq OWNER TO driller;

CREATE TABLE dr_game (
  id integer PRIMARY KEY,
  title varchar(255) NOT NULL DEFAULT '',
  subtitle varchar(255) NOT NULL DEFAULT '',
  description varchar(2000) NOT NULL DEFAULT '',
  gametime_start date,
  gametime_end date
);

ALTER TABLE public.dr_game OWNER TO driller;

CREATE TABLE dr_scenario (
  id integer PRIMARY KEY DEFAULT nextval('dr_scenario_id_seq'),
  id_game integer NOT NULL REFERENCES dr_game(id),
  year_from integer,
  year_upto integer,
  latitude_trunc integer,
  longitude_trunc integer,
  range integer DEFAULT 0 NOT NULL,
  timescale integer DEFAULT 0 NOT NULL,
  players_min integer DEFAULT 0 NOT NULL,
  players_max integer DEFAULT 0 NOT NULL
);

ALTER TABLE public.dr_scenario OWNER TO driller;

CREATE INDEX dr_scenario_year_from_index ON dr_scenario(year_from ASC);
ALTER INDEX dr_scenario_year_from_index OWNER TO driller;
CREATE INDEX dr_scenario_year_upto_index ON dr_scenario(year_upto ASC);
ALTER INDEX dr_scenario_year_upto_index OWNER TO driller;
CREATE INDEX dr_scenario_latitude_trunc_index ON dr_scenario(latitude_trunc ASC);
ALTER INDEX dr_scenario_latitude_trunc_index OWNER TO driller;
CREATE INDEX dr_scenario_longitude_trunc_index ON dr_scenario(longitude_trunc ASC);
ALTER INDEX dr_scenario_longitude_trunc_index OWNER TO driller;
CREATE INDEX dr_scenario_range_index ON dr_scenario(range ASC);
ALTER INDEX dr_scenario_range_index OWNER TO driller;
CREATE INDEX dr_scenario_timescale_index ON dr_scenario(timescale ASC);
ALTER INDEX dr_scenario_timescale_index OWNER TO driller;

CREATE TABLE dr_scenario_data (
  id_scenario integer PRIMARY KEY REFERENCES dr_scenario(id),
  title varchar(255) NOT NULL DEFAULT '',
  subtitle varchar(255) NOT NULL DEFAULT '',
  description varchar(2000) NOT NULL DEFAULT '',
  gametime_start date,
  gametime_end date,
  latitude double precision,
  longitude double precision
);

ALTER TABLE public.dr_scenario_data OWNER TO driller;

CREATE INDEX dr_scenario_data_title_index ON dr_scenario_data(title ASC);
ALTER INDEX dr_scenario_data_title_index OWNER TO driller;

CREATE TABLE dr_author (
  id integer PRIMARY KEY DEFAULT nextval('dr_author_id_seq'),
  author varchar(255) NOT NULL DEFAULT ''
);

ALTER TABLE public.dr_author OWNER TO driller;

CREATE INDEX dr_author_index ON dr_author(author ASC);
ALTER INDEX dr_author_index OWNER TO driller;

CREATE TABLE dr_map_author (
  id_scenario integer NOT NULL REFERENCES dr_scenario(id),
  id_author integer NOT NULL REFERENCES dr_author(id)
);

ALTER TABLE public.dr_map_author OWNER TO driller;

CREATE INDEX dr_map_author_index ON dr_map_author(id_author ASC);
CREATE INDEX dr_map_author_scenario_index ON dr_map_author(id_scenario ASC);
ALTER INDEX dr_map_author_index OWNER TO driller;
ALTER INDEX dr_map_author_scenario_index OWNER TO driller;

CREATE TABLE dr_genre (
  id integer PRIMARY KEY DEFAULT nextval('dr_genre_id_seq'),
  genre varchar(255) NOT NULL DEFAULT ''
);

ALTER TABLE public.dr_genre OWNER TO driller;

CREATE INDEX dr_genre_index ON dr_genre(genre ASC);
ALTER INDEX dr_genre_index OWNER TO driller;

CREATE TABLE dr_map_genre (
  id_game integer NOT NULL REFERENCES dr_game(id),
  id_genre integer NOT NULL REFERENCES dr_genre(id)
);

ALTER TABLE public.dr_map_genre OWNER TO driller;

CREATE INDEX dr_map_genre_index ON dr_map_genre(id_genre ASC);
CREATE INDEX dr_map_genre_game_index ON dr_map_genre(id_game ASC);
ALTER INDEX dr_map_genre_index OWNER TO driller;
ALTER INDEX dr_map_genre_game_index OWNER TO driller;

CREATE TABLE dr_engine (
  id integer PRIMARY KEY DEFAULT nextval('dr_engine_id_seq'),
  engine varchar(255) NOT NULL DEFAULT ''
);

ALTER TABLE public.dr_engine OWNER TO driller;

CREATE INDEX dr_engine_index ON dr_engine(engine ASC);
ALTER INDEX dr_engine_index OWNER TO driller;

CREATE TABLE dr_map_engine (
  id_game integer NOT NULL REFERENCES dr_game(id),
  id_engine integer NOT NULL REFERENCES dr_engine(id),
  url varchar(255) NOT NULL DEFAULT 'http://www.google.com'
);

ALTER TABLE public.dr_map_engine OWNER TO driller;

CREATE INDEX dr_map_engine_index ON dr_map_engine(id_engine ASC);
CREATE INDEX dr_map_engine_game_index ON dr_map_engine(id_game ASC);
ALTER INDEX dr_map_engine_index OWNER TO driller;
ALTER INDEX dr_map_engine_game_index OWNER TO driller;

CREATE TABLE dr_theme (
  id integer PRIMARY KEY DEFAULT nextval('dr_theme_id_seq'),
  theme varchar(255) NOT NULL DEFAULT ''
);

ALTER TABLE public.dr_theme OWNER TO driller;

CREATE INDEX dr_theme_index ON dr_theme(theme ASC);
ALTER INDEX dr_theme_index OWNER TO driller;

CREATE TABLE dr_map_theme (
  id_game integer NOT NULL REFERENCES dr_game(id),
  id_theme integer NOT NULL REFERENCES dr_theme(id)
);

ALTER TABLE public.dr_map_theme OWNER TO driller;

CREATE INDEX dr_map_theme_index ON dr_map_theme(id_theme ASC);
CREATE INDEX dr_map_theme_game_index ON dr_map_theme(id_game ASC);
ALTER INDEX dr_map_theme_index OWNER TO driller;
ALTER INDEX dr_map_theme_game_index OWNER TO driller;

CREATE TABLE dr_mechanic (
  id integer PRIMARY KEY DEFAULT nextval('dr_mechanic_id_seq'),
  mechanic varchar(255) NOT NULL DEFAULT ''
);

ALTER TABLE public.dr_mechanic OWNER TO driller;

CREATE INDEX dr_mechanic_index ON dr_mechanic(mechanic ASC);
ALTER INDEX dr_mechanic_index OWNER TO driller;

CREATE TABLE dr_map_mechanic (
  id_game integer NOT NULL REFERENCES dr_game(id),
  id_mechanic integer NOT NULL REFERENCES dr_mechanic(id)
);

ALTER TABLE public.dr_map_mechanic OWNER TO driller;

CREATE INDEX dr_map_mechanic_index ON dr_map_mechanic(id_mechanic ASC);
CREATE INDEX dr_map_mechanic_game_index ON dr_map_mechanic(id_game ASC);
ALTER INDEX dr_map_mechanic_index OWNER TO driller;
ALTER INDEX dr_map_mechanic_game_index OWNER TO driller;

CREATE TABLE dr_side (
  id integer PRIMARY KEY DEFAULT nextval('dr_side_id_seq'),
  side varchar(255) NOT NULL DEFAULT ''
);

ALTER TABLE public.dr_side OWNER TO driller;

CREATE INDEX dr_side_index ON dr_side(side ASC);
ALTER INDEX dr_side_index OWNER TO driller;

CREATE TABLE dr_map_side (
  id_scenario integer NOT NULL REFERENCES dr_scenario(id),
  id_side integer NOT NULL REFERENCES dr_side(id)
);

ALTER TABLE public.dr_map_side OWNER TO driller;

CREATE INDEX dr_map_side_index ON dr_map_side(id_side ASC);
CREATE INDEX dr_map_side_scenario_index ON dr_map_side(id_scenario ASC);
ALTER INDEX dr_map_side_index OWNER TO driller;
ALTER INDEX dr_map_side_scenario_index OWNER TO driller;

CREATE TABLE dr_special (
  id integer PRIMARY KEY DEFAULT nextval('dr_special_id_seq'),
  special varchar(255) NOT NULL DEFAULT ''
);

ALTER TABLE public.dr_special OWNER TO driller;

CREATE INDEX dr_special_index ON dr_special(special ASC);
ALTER INDEX dr_special_index OWNER TO driller;

CREATE TABLE dr_map_special (
  id_scenario integer NOT NULL REFERENCES dr_scenario(id),
  id_special integer NOT NULL REFERENCES dr_special(id)
);

ALTER TABLE public.dr_map_special OWNER TO driller;

CREATE INDEX dr_map_special_index ON dr_map_special(id_special ASC);
CREATE INDEX dr_map_special_scenario_index ON dr_map_special(id_scenario ASC);
ALTER INDEX dr_map_special_index OWNER TO driller;
ALTER INDEX dr_map_special_scenario_index OWNER TO driller;

CREATE TABLE dr_party (
  id integer PRIMARY KEY DEFAULT nextval('dr_party_id_seq'),
  party varchar(255) NOT NULL DEFAULT ''
);

ALTER TABLE public.dr_party OWNER TO driller;

CREATE INDEX dr_party_index ON dr_party(party ASC);
ALTER INDEX dr_party_index OWNER TO driller;

CREATE TABLE dr_map_party (
  id_scenario integer NOT NULL REFERENCES dr_scenario(id),
  num_players integer NOT NULL,
  id_party integer NOT NULL REFERENCES dr_party(id)
);

ALTER TABLE public.dr_map_party OWNER TO driller;

CREATE INDEX dr_map_party_index ON dr_map_party(id_party ASC);
CREATE INDEX dr_map_party_scenario_index ON dr_map_party(id_scenario ASC);
ALTER INDEX dr_map_party_index OWNER TO driller;
ALTER INDEX dr_map_party_scenario_index OWNER TO driller;

CREATE TABLE dr_leader (
  id integer PRIMARY KEY DEFAULT nextval('dr_leader_id_seq'),
  leader varchar(255) NOT NULL DEFAULT ''
);

ALTER TABLE public.dr_leader OWNER TO driller;

CREATE INDEX dr_leader_index ON dr_leader(leader ASC);
ALTER INDEX dr_leader_index OWNER TO driller;

CREATE TABLE dr_map_leader (
  id_scenario integer NOT NULL REFERENCES dr_scenario(id),
  id_leader integer NOT NULL REFERENCES dr_leader(id)
);

ALTER TABLE public.dr_map_leader OWNER TO driller;

CREATE INDEX dr_map_leader_index ON dr_map_leader(id_leader ASC);
CREATE INDEX dr_map_leader_game_index ON dr_map_leader(id_scenario ASC);
ALTER INDEX dr_map_leader_index OWNER TO driller;
ALTER INDEX dr_map_leader_game_index OWNER TO driller;

CREATE TABLE dr_publisher (
  id integer PRIMARY KEY DEFAULT nextval('dr_publisher_id_seq'),
  publisher varchar(255) NOT NULL DEFAULT ''
);

ALTER TABLE public.dr_publisher OWNER TO driller;

CREATE INDEX dr_publisher_index ON dr_publisher(publisher ASC);
ALTER INDEX dr_publisher_index OWNER TO driller;

CREATE TABLE dr_map_publisher (
  id_game integer NOT NULL REFERENCES dr_game(id),
  url varchar(255) NOT NULL DEFAULT 'http://www.google.com',
  id_publisher integer NOT NULL REFERENCES dr_publisher(id)
);

ALTER TABLE public.dr_map_publisher OWNER TO driller;

CREATE INDEX dr_map_publisher_index ON dr_map_publisher(id_publisher ASC);
CREATE INDEX dr_map_publisher_game_index ON dr_map_publisher(id_game ASC);
ALTER INDEX dr_map_publisher_index OWNER TO driller;
ALTER INDEX dr_map_publisher_game_index OWNER TO driller;

CREATE TABLE dr_series (
  id integer PRIMARY KEY DEFAULT nextval('dr_series_id_seq'),
  series varchar(255) NOT NULL DEFAULT ''
);

ALTER TABLE public.dr_series OWNER TO driller;

CREATE INDEX dr_series_index ON dr_series(series ASC);
ALTER INDEX dr_series_index OWNER TO driller;

CREATE TABLE dr_map_series (
  id_game integer NOT NULL REFERENCES dr_game(id),
  part varchar(255) NOT NULL DEFAULT '',
  id_series integer NOT NULL REFERENCES dr_series(id)
);

ALTER TABLE public.dr_map_series OWNER TO driller;

CREATE INDEX dr_map_series_index ON dr_map_series(id_series ASC);
CREATE INDEX dr_map_series_game_index ON dr_map_series(id_game ASC);
ALTER INDEX dr_map_series_index OWNER TO driller;
ALTER INDEX dr_map_series_game_index OWNER TO driller;

