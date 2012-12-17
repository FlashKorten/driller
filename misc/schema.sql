TABLE nn_game (
  id integer PRIMARY KEY DEFAULT nextval('nn_game_id_seq'),
  game varchar(255) NOT NULL default '',
  subtitle varchar(255) NOT NULL default '',
  description varchar(2000),
  num_players_min integer DEFAULT 0 NOT NULL,
  num_players_max integer DEFAULT 0 NOT NULL,
  id_bgg varchar(255) NOT NULL default ''
  --
  gametime_start integer DEFAULT 0 NOT NULL, -- should be changed to a date
  gametime_end integer DEFAULT 0 NOT NULL -- should be changed to a date
  position point (Latitude, Longitude in decimal degrees) -- new
  scale (radius in km surrounding center) -- new
  timescale (hours per turn) -- new
);

game maps to
  author varchar(255) NOT NULL default ''
  engine varchar(255) NOT NULL default '', url varchar(255) NOT NULL default 'http://www.google.com'
  genre varchar(255) NOT NULL default ''
  mechanic varchar(255) NOT NULL default ''
  party varchar(255) NOT NULL default '' -- i.e. germany, france
  side varchar(255) NOT NULL default '' -- i.e. axis, allies
  theme varchar(255) NOT NULL default ''
  --
  area varchar(255) NOT NULL default '' -- should be removed
  publisher varchar(255) NOT NULL default '', url varchar(255) NOT NULL default 'http://www.google.com' -- url like engine-url to game specific page
  series varchar(255) -- i.e. Memoir44, Combat Commander
);

TABLE nn_user (
  id integer PRIMARY KEY DEFAULT nextval('nn_user_id_seq'),
  position point (Latitude, Longitude in decimal degrees) -- new
  username varchar(255) NOT NULL default '',
  realname varchar(255) NOT NULL default '',
  email varchar(255) NOT NULL default '',
  showEmail boolean NOT NULL default false -- new
);

TABLE nn_request (
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

TABLE nn_session (
  id integer PRIMARY KEY DEFAULT nextval('nn_session_id_seq'),
  id_game integer NOT NULL REFERENCES nn_game(id),
  title varchar(255) NOT NULL default '',
  report text NOT NULL default '',
  confirmed boolean NOT NULL DEFAULT false,
  played timestamp with time zone NOT NULL default now()
);

TABLE nn_map_session (
  id_session integer NOT NULL REFERENCES nn_session(id),
  id_user integer NOT NULL REFERENCES nn_user(id),
  id_party integer NOT NULL REFERENCES nn_party(id),
  id_result integer NOT NULL REFERENCES nn_result(id),
  confirmed boolean NOT NULL DEFAULT false,
  commentary varchar(255) NOT NULL default ''
);

INSERT INTO nn_user (username,realname,email)VALUES('user','Max Mustermann','email@domain.com');
INSERT INTO nn_user (username,realname,email)VALUES('FlashKorten','Sebastian Korten','bass@core10.de');
INSERT INTO nn_result (id,result)VALUES(1,'Won');
INSERT INTO nn_result (id,result)VALUES(2,'Draw');
INSERT INTO nn_result (id,result)VALUES(3,'Lost');

create domain latitude_t as double precision not null check(value>=-90 and value<=90);
create domain longitude_t as double precision not null check(value>-180 and value<=180);
create type geocoord_t as (latitude latitude_t, longitude longitude_t);
