#!/usr/bin/perl

# Importfile:
# BGGID {
#  title{"TITLE[ - SUBTITLE]"}
#  description{<p>DESCRIPTION</p>}
#  publisher{"NAME"-"GAMEURL"[,+]}
#  engine{"SYSTEM"-"MODULEURL"[,+]}
#  genre{"GENRE"[,+]}
#  mechanic{"MECHANIC"[,+]}
#  theme{"THEME"[,+]}
#  series{"SERIES"-"PART"[,+]}
#  scenario{"TITLE[ - SUBTITLE]"}{
#    dates{FROM / UPTO} # FORMAT 1900-01-01 [BC]
#    position{LATITUDE / LONGITUDE} # FORMAT [-]12.34567
#    range{KILOMETER}
#    hoursPerTurn{5}
#    players{FROM / UPTO}
#    side{"SIDE"}
#    party{"PARTY"-"NUMBER OF PLAYERS"[,+]}
#    author{"LASTNAME, FIRSTNAME"}
#    description{<p>DESCRIPTION</p>}
#  }
#}
#

use DBI;
use warnings;
use strict;

# --------------------------------------------------------------------------
my $db = 'dr';
my $host = 'localhost';
my $port= '5432';
my $user = 'driller';
my $password = 'driller';
my $driver = "DBI:Pg:database=$db;host=$host;port=$port";
my $dbh = DBI->connect($driver,$user,$password) or die $DBI::errstr;
# --------------------------------------------------------------------------

die "USAGE: drillerImport.pl IMPORTFILE\n" unless(@ARGV == 1);

# --------------------------------------------------------------------------

my $importfile = shift @ARGV;

open(READ_FROM_FILE,"< $importfile") or die "Couldn't open file $importfile\n";

my $debug = 0;

my %game;
my $game_id;
my $id_bgg;
my $line;
my $scenario;

while(<READ_FROM_FILE>){
  if (m/^\s*$/) {next;}
  if (m/^\d+\s{\s*$/) {
    $game_id = &trim($_);
    $game{$game_id}{'id'} = $game_id;
  } elsif (m/^  scenario{".*"}/) {
    $scenario = &trim_named_field($_);
  } elsif (defined $scenario && m/^  }/) {
    undef $scenario;
  } elsif (defined $scenario) {
    $game{$game_id}{"scenario"}{$scenario}{&trim_to_brace($_)} = &trim_named_field($_);
  } else {
    $game{$game_id}{&trim_to_brace($_)} = &trim_named_field($_);
  }
}
close(READ_FROM_FILE);

my @tmp;
my $id_game;
my $id_scenario;
my $result;
my $query;
my $sth_select;

my $players_min;
my $players_max;
my $time_01;
my $time_02;
my $latitude;
my $longitude;
my $scen_title;
my $scen_subtitle;

my $sth_update_game = $dbh -> prepare(
    "UPDATE dr_game SET "
  . "title = ?, "
  . "subtitle = ?, "
  . "grp = ?, "
  . "description = ? "
  . "WHERE id = ?;");

my $sth_insert_game = $dbh -> prepare(
    "INSERT INTO dr_game ("
  . "id, "
  . "title, "
  . "subtitle, "
  . "grp, "
  . "description)"
  . "VALUES (?,?,?,?,?);");

my $sth_insert_scenario = $dbh -> prepare(
    "INSERT INTO dr_scenario ("
  . "id_game, "
  . "year_from, "
  . "year_upto, "
  . "year_from_group, "
  . "year_upto_group, "
  . "latitude_trunc, "
  . "longitude_trunc, "
  . "latitude_group, "
  . "longitude_group, "
  . "range, "
  . "range_group, "
  . "timescale, "
  . "timescale_group, "
  . "players_min, "
  . "players_max) "
  . "VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) "
  . "RETURNING id;");

my $sth_insert_scenario_data = $dbh -> prepare(
    "INSERT INTO dr_scenario_data ("
  . "id_scenario, "
  . "title, "
  . "subtitle, "
  . "description, "
  . "gametime_start, "
  . "gametime_end, "
  . "latitude, "
  . "longitude) "
  . "VALUES (?,?,?,?,?,?,?,?);");

my $sth_select_game_id = $dbh -> prepare(
    "SELECT id "
  . "FROM dr_game "
  . "WHERE id = ?;");

&clean_up_tables($dbh);
foreach my $key (sort(keys %game)){
  # Insert the game:

  if ($game{$key}{'title'} =~ m/ ~ /){
    ($game{$key}{'title'}, $game{$key}{'subtitle'}) = split(/\s+~\s+/, $game{$key}{'title'});
  } else {
    $game{$key}{'subtitle'} = "";
  }
  $sth_insert_game -> execute( $key,
                             , $game{$key}{'title'}
                             , $game{$key}{'subtitle'}
                             , substr($game{$key}{'title'}, 0, 1)
                             , $game{$key}{'description'});

  &insert_simple_field_for_game($key);

  foreach my $scen_key (sort(keys %{$game{$key}{'scenario'}})){
    ($players_min, $players_max) = split(/\s*\/\s*/, $game{$key}{'scenario'}{$scen_key}{'players'});
    ($time_01, $time_02) = split(/\s*\/\s*/, $game{$key}{'scenario'}{$scen_key}{'dates'});
    ($latitude, $longitude) = split(/\s*\/\s*/, $game{$key}{'scenario'}{$scen_key}{'position'});

    if ($scen_key =~ m/ ~ /){
      ($scen_title, $scen_subtitle) = split(/\s+~\s+/, $scen_key);
    } else {
      ($scen_title, $scen_subtitle) = ($scen_key, "");
    }

    my $year_from  = &get_year_from_date($time_01);
    my $year_upto  = &get_year_from_date($time_02);
    my $range      = $game{$key}{'scenario'}{$scen_key}{'range'};
    my $timescale  = $game{$key}{'scenario'}{$scen_key}{'timescale'};

    $sth_insert_scenario -> execute( $key
                                   , $year_from
                                   , $year_upto
                                   , &get_number_group($year_from, 50)
                                   , &get_number_group($year_upto, 50)
                                   , (split(/\./, $latitude))[0]
                                   , (split(/\./, $longitude))[0]
                                   , &get_number_group($latitude, 10)
                                   , &get_number_group($longitude, 10)
                                   , $range
                                   , &get_number_group($range, 25)
                                   , $timescale
                                   , &get_number_group($timescale, 24)
                                   , $players_min
                                   , $players_max);

   $result = $sth_insert_scenario->fetchrow_hashref();
   $id_scenario = $$result{"id"};

   $sth_insert_scenario_data -> execute( $id_scenario
                                       , $scen_title
                                       , $scen_subtitle
                                       , $game{$key}{'scenario'}{$scen_key}{'description'}
                                       , $time_01
                                       , $time_02
                                       , $latitude
                                       , $longitude);
    &insert_simple_field_for_scenario($key, $id_scenario, $scen_key);
  }
  # Done inserting the game...
}

my $sth_select_game_ids = $dbh -> prepare(
    "SELECT id "
  . "FROM dr_game;");

my $sth_update_gametime_start = $dbh -> prepare(
    "UPDATE dr_game SET gametime_start = ("
    . "SELECT min(gametime_start) "
    . "FROM dr_scenario_data AS d, dr_scenario AS s "
    . "WHERE d.id_scenario = s.id and s.id_game = ?) "
  . "WHERE id = ?;");

my $sth_update_gametime_end = $dbh -> prepare(
    "UPDATE dr_game SET gametime_end = ("
    . "SELECT max(gametime_end) "
    . "FROM dr_scenario_data AS d, dr_scenario AS s "
    . "WHERE d.id_scenario = s.id and s.id_game = ?) "
  . "WHERE id = ?;");

my $id;

$sth_select_game_ids->execute();
$sth_select_game_ids->bind_columns(\$id);

while($sth_select_game_ids->fetch()) {
  $sth_update_gametime_start->execute($id, $id);
  $sth_update_gametime_end->execute($id, $id);
}

$sth_update_gametime_start->finish();
$sth_update_gametime_end->finish();

sub insert_simple_field {
  my ($id_game, $label, $value, $mapped_to) = @_;
  &insert_field($id_game, $label, $label, $value, $mapped_to);
}

sub insert_field {
  my ($id_game, $label, $map_name, $value, $mapped_to) = @_;
  unless ($value) {
    return;
  }
  my ($insertQuery, $selectResult, $selectQuery, $selectStatement, $insertStatement);
  my @tmpFoo = split(/","/, $value);
  $debug && print "#instances of $label: ".@tmpFoo."\n";
  for (my $i = 0; $i < @tmpFoo; $i++){
    $selectQuery = "SELECT id FROM dr_$label WHERE lower($label) = lower(?);";
    $selectStatement = $dbh -> prepare($selectQuery);
    $selectStatement -> execute($tmpFoo[$i]);
    $selectResult = $selectStatement->fetchrow_array();
    if (!defined $selectResult){
      $insertQuery = "INSERT INTO dr_$label (grp, $label) VALUES (?,?) RETURNING id;";
      $insertStatement = $dbh -> prepare($insertQuery);
      $insertStatement -> execute(substr($tmpFoo[$i], 0, 1), $tmpFoo[$i]);
      my $foo_rec = $insertStatement->fetchrow_hashref();
      $insertStatement -> finish();
      $tmpFoo[$i] = $$foo_rec{"id"};
    } else {
      $tmpFoo[$i] = $selectResult;
    }
    $insertQuery = "INSERT INTO dr_map_$map_name (id_" . $mapped_to . ", id_$label) VALUES (?,?);";
    $insertStatement = $dbh -> prepare($insertQuery);
    $insertStatement -> execute($id_game, $tmpFoo[$i]);
    $insertStatement -> finish();
    $selectStatement -> finish();
  }
}

sub insert_field_with_attribute_in_map {
  my ($id_game, $label, $value, $additionalLabel, $mapped_to) = @_;
  unless ($value) {
    return;
  }
  my ($insertQuery, $selectResult, $selectQuery, $selectStatement, $insertStatement);
  my @tmpFoo = split(/","/, $value);
  $debug && print $value . "\n#instances of $label: ".@tmpFoo."\n";
  for (my $i = 0; $i < @tmpFoo; $i++){
    my ($valueFoo, $additionalFoo) = split(/"\s*-\s*"/, $tmpFoo[$i]);
    $selectQuery = "SELECT id FROM dr_$label WHERE lower($label) = lower(?);";
    $debug && print "Value: $valueFoo - additionalValue: $additionalFoo\n";
    $selectStatement = $dbh -> prepare($selectQuery);
    $selectStatement -> execute($valueFoo);
    $selectResult = $selectStatement->fetchrow_array();
    if (!defined $selectResult){
      $insertQuery = "INSERT INTO dr_$label (grp, $label) VALUES (?,?) RETURNING id;";
      $insertStatement = $dbh -> prepare($insertQuery);
      $insertStatement -> execute(substr($valueFoo, 0, 1), $valueFoo);
      my $foo_rec = $insertStatement->fetchrow_hashref();
      $insertStatement -> finish();
      $tmpFoo[$i] = $$foo_rec{"id"};
    } else {
      $tmpFoo[$i] = $selectResult;
    }
    $insertQuery = "INSERT INTO dr_map_$label (id_" . $mapped_to  . ", id_$label, $additionalLabel) VALUES (?,?,?);";
    $insertStatement = $dbh -> prepare($insertQuery);
    $insertStatement -> execute($id_game, $tmpFoo[$i], $additionalFoo);
    $insertStatement -> finish();
    $selectStatement -> finish();
  }
}

sub insert_simple_field_for_game {
  my ($game_id) = @_;
  &insert_simple_field($game_id,      "genre",     $game{$game_id}{"genre"},             "game");
  &insert_simple_field($game_id,      "theme",     $game{$game_id}{"theme"},             "game");
  &insert_simple_field($game_id,      "mechanic",  $game{$game_id}{"mechanic"},          "game");
  &insert_field_with_attribute_in_map($game_id, "publisher", $game{$game_id}{"publisher"}, "url",  "game");
  &insert_field_with_attribute_in_map($game_id, "engine",    $game{$game_id}{"engine"},    "url",  "game");
  &insert_field_with_attribute_in_map($game_id, "series",    $game{$game_id}{"series"},    "part", "game");
}

sub insert_simple_field_for_scenario {
  my ($game_id, $scenario_id, $scenario_name) = @_;
  &insert_simple_field($scenario_id,      "author",  $game{$game_id}{'scenario'}{$scenario_name}{"author"},                 "scenario");
  &insert_simple_field($scenario_id,      "side",    $game{$game_id}{'scenario'}{$scenario_name}{"side"},                   "scenario");
  &insert_simple_field($scenario_id,      "leader",  $game{$game_id}{'scenario'}{$scenario_name}{"leader"},                 "scenario");
  &insert_simple_field($scenario_id,      "special", $game{$game_id}{'scenario'}{$scenario_name}{"special"},                "scenario");
  &insert_field($scenario_id,      "side", "historical_victor", $game{$game_id}{'scenario'}{$scenario_name}{"victor"},      "scenario");
  &insert_field_with_attribute_in_map($scenario_id, "party",   $game{$game_id}{'scenario'}{$scenario_name}{"party"},   "num_players", "scenario");
}

sub clean_up_tables {
  my $handle = shift();
  &reset_scenario_sequence($handle);
  &clean_up_table($handle, "dr_map_genre");
  &clean_up_table($handle, "dr_map_theme");
  &clean_up_table($handle, "dr_map_mechanic");
  &clean_up_table($handle, "dr_map_publisher");
  &clean_up_table($handle, "dr_map_engine");
  &clean_up_table($handle, "dr_map_series");
  &clean_up_table($handle, "dr_map_author");
  &clean_up_table($handle, "dr_map_side");
  &clean_up_table($handle, "dr_map_leader");
  &clean_up_table($handle, "dr_map_special");
  &clean_up_table($handle, "dr_map_party");
  &clean_up_table($handle, "dr_map_historical_victor");
  &clean_up_table($handle, "dr_scenario_data");
  &clean_up_table($handle, "dr_scenario");
  &clean_up_table($handle, "dr_game");
}

sub clean_up_table {
  my ($handle, $tablename) = @_;
  my $sth = $handle -> prepare("DELETE FROM " . $tablename . ";");
  $sth -> execute();
  $sth -> finish();
}

sub reset_scenario_sequence {
  my $handle = shift();
  my $sth = $handle -> prepare("ALTER SEQUENCE dr_scenario_id_seq RESTART;");
  $sth -> execute();
  $sth -> finish();
}

sub trim {
  my $text = shift();
  chomp $text;
  $text =~ s/(^[\s}"]+|[\s{"]+$)//g;
  return $text;
}

sub trim_to_brace {
  my $text = shift();
  chomp $text;
  $text =~ s/(^\s*|{.*$)//g;
  return $text;
}

sub trim_named_field {
  my $text = shift();
  chomp $text;
  $text =~ s/(^[^{]+{"?|"?}.*$)//g;
  return $text;
}

sub get_year_from_date {
  my ($val) = @_;
  my $result = (split(/-/, $val))[0];
  if ($val =~ m/BC$/) {
    $result *= -1;
  }
  return $result;
}

sub get_number_group {
  my ($val, $base) = @_;
  $val /= $base;
  if ($val < 0) {
    $val -= 1;
  }
  $val =~ s/\..*//;
  return $val * $base;
}
