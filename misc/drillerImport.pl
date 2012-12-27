#!/usr/bin/perl

#
# Importfile:
# "NAME" {
#   id_bgg
#   desctiption
#   gametime_start / gametime_end
#   latitude / longitude
#   range
#   timescale
#   players_min / players_max
#   author{"a"[,"b"]*}
#   engine{"a"-"url"[,"b"-"url"]*}
#   genre{"a"[,"b"]*}
#   mechanic{"a"[,"b"]*}
#   leader{"a"[,"b"]*}
#   party{"a"-"n"[,"b"-"m"]*}
#   publisher{"a"-"url"[,"b"-"url"]*}
#   series{"a"[,"b"]*}
#   side{"a"[,"b"]*}
#   theme{"a"[,"b"]*}
# }

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

my $debug = 1;

my %game;
my $game;
my $id_bgg;
my $line;

while(<READ_FROM_FILE>){
  if (m/^\s*$/) {next;}
  if (m/^\s*".*"\s{\s*$/) {
    $game = &trim($_);
    $game{$game}{'name'} = $game;
    $line = <READ_FROM_FILE>;
    $game{$game}{'id_bgg'} = &trim($line);
    $line = <READ_FROM_FILE>;
    $game{$game}{'description'} = &trim($line);
    $line = <READ_FROM_FILE>;
    ($game{$game}{'time_01'}, $game{$game}{'time_02'}) = split(/\s*\/\s*/, &trim($line));
    $line = <READ_FROM_FILE>;
    ($game{$game}{'latitude'}, $game{$game}{'longitude'}) = split(/\s*\/\s*/, &trim($line));
    $line = <READ_FROM_FILE>;
    $game{$game}{'range'} = &trim($line);
    $line = <READ_FROM_FILE>;
    $game{$game}{'timescale'} = &trim($line);
    $line = <READ_FROM_FILE>;
    ($game{$game}{'players_min'}, $game{$game}{'players_max'}) = split(/\s*\/\s*/, &trim($line));
  } elsif (m/{.*}/) {
    $game{$game}{&trimToBrace($_)} = &trimMultifields($_);
  }
}
close(READ_FROM_FILE);

my @tmp;
my $id_game;
my $result;
my $query;
my $sth_select;
my $insert;
my $sth_insert;
my $update;
my $sth_update;

foreach my $key (sort(keys %game)){
  # Insert the game:

  if ($game{$key}{'name'} =~ m/ - /){
    ($game{$key}{'name'}, $game{$key}{'subtitle'}) = split(/\s+-\s+/, $game{$key}{'name'});
  } else {
    $game{$key}{'subtitle'} = "";
  }
  $query = "SELECT id_game FROM dr_game_data WHERE lower(title) = lower(?);";
  $sth_select = $dbh -> prepare($query);
  $sth_select -> execute($game{$key}{'name'});
  $result = $sth_select->fetchrow_hashref();
  if (defined $result) {
    $id_game = $$result{"id_game"};
    print "---------------------------------\n";
    print "Updating existing entry for: $key\n";
    print "---------------------------------\n";
    $update = "UPDATE dr_game SET "
            . "year_from = ?, year_upto = ?, "
            . "players_min = ?, players_max = ?, "
            . "latitude_trunc = ?, longitude_trunc = ?, "
            . "range = ?, timescale = ? WHERE id = ?;";
    $sth_update = $dbh -> prepare($update);
    $sth_update -> execute( &getYearFromDate($key, 'time_01')
                          , &getYearFromDate($key, 'time_02')
                          , $game{$key}{'players_min'}
                          , $game{$key}{'players_max'}
                          , (split(/\./, $game{$key}{'latitude'}))[0]
                          , (split(/\./, $game{$key}{'longitude'}))[0]
                          , $game{$key}{'range'}
                          , $game{$key}{'timescale'}
                          , $id_game);
    &clean_up_dependent_tables($dbh, $id_game);
  } else {
    $insert = "INSERT INTO dr_game "
            . "(year_from, year_upto, players_min, players_max, latitude_trunc, longitude_trunc, range, timescale) "
            . "VALUES (?,?,?,?,?,?,?,?) RETURNING id;";
    $sth_insert = $dbh -> prepare($insert);
    $sth_insert -> execute( &getYearFromDate($key, 'time_01')
                          , &getYearFromDate($key, 'time_02')
                          , $game{$key}{'players_min'}
                          , $game{$key}{'players_max'}
                          , (split(/\./, $game{$key}{'latitude'}))[0]
                          , (split(/\./, $game{$key}{'longitude'}))[0]
                          , $game{$key}{'range'}
                          , $game{$key}{'timescale'});
    $result = $sth_insert->fetchrow_hashref();
    $sth_insert -> finish();
    $id_game = $$result{"id"};
  }
  $insert = "INSERT INTO dr_game_data "
          . "(id_game, id_bgg, title, subtitle, description, gametime_start, gametime_end, latitude, longitude) "
          . "VALUES (?,?,?,?,?,?,?,?,?);";
  $sth_insert = $dbh -> prepare($insert);
  $sth_insert -> execute( $id_game
                        , $game{$key}{'id_bgg'}
                        , $game{$key}{'name'}
                        , $game{$key}{'subtitle'}
                        , $game{$key}{'description'}
                        , $game{$key}{'time_01'}
                        , $game{$key}{'time_02'}
                        , $game{$key}{'latitude'}
                        , $game{$key}{'longitude'});
  $sth_insert -> finish();
  $debug && print "Game $key has ID #$id_game;\n";
  # Done inserting the game...

  &insertSimpleField($key, "author");
  &insertSimpleField($key, "genre");
  &insertSimpleField($key, "theme");
  &insertSimpleField($key, "mechanic");
  &insertSimpleField($key, "side");
  &insertSimpleField($key, "leader");
  &insertSimpleField($key, "special");
  &insertFieldWithAttributeInMap($key, "publisher", "url");
  &insertFieldWithAttributeInMap($key, "engine", "url");
  &insertFieldWithAttributeInMap($key, "series", "part");
  &insertFieldWithAttributeInMap($key, "party", "num_players");
}

sub insertSimpleField {
  my ($game_name, $label) = @_;
  unless ($game{$game_name}{$label}) {
    return;
  }
  my ($insertQuery, $selectResult, $selectQuery, $selectStatement, $insertStatement);
  my @tmpFoo = split(/","/, $game{$game_name}{$label});
  $debug && print "#instances of $label: ".@tmpFoo."\n";
  for (my $i = 0; $i < @tmpFoo; $i++){
    $selectQuery = "SELECT id FROM dr_$label WHERE lower($label) = lower(?);";
    $selectStatement = $dbh -> prepare($selectQuery);
    $selectStatement -> execute($tmpFoo[$i]);
    $selectResult = $selectStatement->fetchrow_array();
    if (!defined $selectResult){
      $insertQuery = "INSERT INTO dr_$label ($label) VALUES (?) RETURNING id;";
      $insertStatement = $dbh -> prepare($insertQuery);
      $insertStatement -> execute($tmpFoo[$i]);
      my $foo_rec = $insertStatement->fetchrow_hashref();
      $insertStatement -> finish();
      $tmpFoo[$i] = $$foo_rec{"id"};
    } else {
      $tmpFoo[$i] = $selectResult;
    }
    $insertQuery = "INSERT INTO dr_map_$label (id_game, id_$label) VALUES (?,?);";
    $insertStatement = $dbh -> prepare($insertQuery);
    $insertStatement -> execute($id_game, $tmpFoo[$i]);
    $insertStatement -> finish();
    $selectStatement -> finish();
  }
}

sub insertFieldWithAttributeInMap {
  my ($game_name, $label, $additionalLabel) = @_;
  unless ($game{$game_name}{$label}) {
    return;
  }
  my ($insertQuery, $selectResult, $selectQuery, $selectStatement, $insertStatement);
  my @tmpFoo = split(/","/, $game{$game_name}{$label});
  $debug && print $game{$game_name}{$label}."\n#instances of $label: ".@tmpFoo."\n";
  for (my $i = 0; $i < @tmpFoo; $i++){
    my ($valueFoo, $additionalFoo) = split(/"\s*-\s*"/, $tmpFoo[$i]);
    $selectQuery = "SELECT id FROM dr_$label WHERE lower($label) = lower(?);";
    $debug && print "Value: $valueFoo - additionalValue: $additionalFoo\n";
    $selectStatement = $dbh -> prepare($selectQuery);
    $selectStatement -> execute($valueFoo);
    $selectResult = $selectStatement->fetchrow_array();
    if (!defined $selectResult){
      $insertQuery = "INSERT INTO dr_$label ($label) VALUES (?) RETURNING id;";
      $insertStatement = $dbh -> prepare($insertQuery);
      $insertStatement -> execute($valueFoo);
      my $foo_rec = $insertStatement->fetchrow_hashref();
      $insertStatement -> finish();
      $tmpFoo[$i] = $$foo_rec{"id"};
    } else {
      $tmpFoo[$i] = $selectResult;
    }
    $insertQuery = "INSERT INTO dr_map_$label (id_game, id_$label, $additionalLabel) VALUES (?,?,?);";
    $insertStatement = $dbh -> prepare($insertQuery);
    $insertStatement -> execute($id_game, $tmpFoo[$i], $additionalFoo);
    $insertStatement -> finish();
    $selectStatement -> finish();
  }
}

sub clean_up_dependent_tables {
  my ($handle, $id) = @_;
  &clean_up_table($dbh, $id_game, "dr_game_data");
  &clean_up_table($dbh, $id_game, "dr_map_author");
  &clean_up_table($dbh, $id_game, "dr_map_genre");
  &clean_up_table($dbh, $id_game, "dr_map_theme");
  &clean_up_table($dbh, $id_game, "dr_map_mechanic");
  &clean_up_table($dbh, $id_game, "dr_map_side");
  &clean_up_table($dbh, $id_game, "dr_map_leader");
  &clean_up_table($dbh, $id_game, "dr_map_special");
  &clean_up_table($dbh, $id_game, "dr_map_publisher");
  &clean_up_table($dbh, $id_game, "dr_map_engine");
  &clean_up_table($dbh, $id_game, "dr_map_series");
  &clean_up_table($dbh, $id_game, "dr_map_party");
}

sub clean_up_table {
  my ($handle, $id, $tablename) = @_;
  my $sth = $handle -> prepare("DELETE FROM " . $tablename . " WHERE id_game = ?;");
  $sth -> execute($id);
  $sth -> finish();
}

sub trim {
  my $text = shift();
  chomp $text;
  $text =~ s/(^[\s}"]+|[\s{"]+$)//g;
  return $text;
}

sub trimToBrace {
  my $text = shift();
  chomp $text;
  $text =~ s/(^\s*|{.*$)//g;
  return $text;
}
sub trimMultifields {
  my $text = shift();
  chomp $text;
  $text =~ s/(.*{"|"}.*)//g;
  return $text;
}

sub getYearFromDate {
  my ($k, $l) = @_;
  my $val = $game{$k}{$l};
  my $result = (split(/-/, $val))[0];
  if ($val =~ m/BC$/) {
    $result *= -1;
  }
  return $result;
}
