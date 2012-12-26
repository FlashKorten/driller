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
my $db = 'nn';
my $host = 'localhost';
my $port= '5432';
my $user = 'nemesis';
my $password = 'nemesis';
my $driver = "DBI:Pg:database=$db;host=$host;port=$port";
my $dbh = DBI->connect($driver,$user,$password) or die $DBI::errstr;
# --------------------------------------------------------------------------

die "USAGE: nemesimporter.pl IMPORTFILE\n" unless(@ARGV == 1);

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

foreach my $key (sort(keys %game)){
  # Insert the game:

  if ($game{$key}{'name'} =~ m/ - /){
    ($game{$key}{'name'}, $game{$key}{'subtitle'}) = split(/\s+-\s+/, $game{$key}{'name'});
  } else {
    $game{$key}{'subtitle'} = "";
  }
  $query = "SELECT id FROM dr_game WHERE lower(game) = lower(?);";
  $sth_select = $dbh -> prepare($query);
  $sth_select -> execute($game{$key}{'name'});
  $result = $sth_select->fetchrow_array();
  if (!defined $result) {
    $insert = "INSERT INTO dr_game (game, id_bgg, subtitle, description, gametime_start, year_from, gametime_end, year_upto, players_min, players_max, latitude, latitude_trunc, longitude, longitude_trunc, range, timescale) VALUES (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?) RETURNING id;";
    $sth_insert = $dbh -> prepare($insert);
    $sth_insert -> execute( $game{$key}{'name'}
                          , $game{$key}{'id_bgg'}
                          , $game{$key}{'subtitle'}
                          , $game{$key}{'description'}
                          , $game{$key}{'time_01'}
                          , &getYearFromDate($key, 'time_01')
                          , $game{$key}{'time_02'}
                          , &getYearFromDate($key, 'time_02')
                          , $game{$key}{'players_min'}
                          , $game{$key}{'players_max'}
                          , $game{$key}{'latitude'}
                          , (split(/\./, $game{$key}{'latitude'}))[0]
                          , $game{$key}{'longitude'}
                          , (split(/\./, $game{$key}{'longitude'}))[0]
                          , $game{$key}{'range'}
                          , $game{$key}{'timescale'});
    my $foo_rec = $sth_insert->fetchrow_hashref();
    $sth_insert -> finish();
    $id_game = $$foo_rec{"id"};
  } else {
          print "---------------------------------\n";
          print "Entry: $key exists already...\n";
          print "Please remove the entry manually before reimporting it.\n";
          print "---------------------------------\n";
          next;
  }
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
