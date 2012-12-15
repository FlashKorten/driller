#!/usr/bin/perl

#
# Importfile:
# "NAME" {
#   gametime_start / gametime_end
#   id_bgg
#   area{"a"[,"b"]*}
#   author{"a"[,"b"]*}
#   engine{"a"-"url"[,"b"-"url"]*}
#   genre{"a"[,"b"]*}
#   mechanic{"a"[,"b"]*}
#   side{"a"[,"b"]*}
#   party{"a"-"n"[,"b"-"m"]*}
#   theme{"a"[,"b"]*}
#   publisher{"a"-"url"[,"b"-"url"]*}
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
    ($game{$game}{'time_01'}, $game{$game}{'time_02'}) = split(/\s*\/\s*/, &trim($line));
    $line = <READ_FROM_FILE>;
    $game{$game}{'id_bgg'} = &trim($line);
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
  $query = "SELECT id FROM nn_game WHERE lower(game) = lower(?);";
  $debug && print "$query\n";
  $sth_select = $dbh -> prepare($query);
  $sth_select -> execute($game{$key}{'name'});
  $result = $sth_select->fetchrow_array();
  if (!defined $result) {
    $insert = "INSERT INTO nn_game (game, subtitle, gametime_start, gametime_end, id_bgg) VALUES (?,?,?,?,?) RETURNING id;";
    $sth_insert = $dbh -> prepare($insert);
    $sth_insert -> execute($game{$key}{'name'}, $game{$key}{'subtitle'}, $game{$key}{'time_01'}, $game{$key}{'time_02'}, $game{$key}{'id_bgg'});
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

  &insertSimpleField($key, "area");
  &insertSimpleField($key, "author");
  &insertSimpleField($key, "genre");
  &insertSimpleField($key, "theme");
  &insertSimpleField($key, "mechanic");
  &insertSimpleField($key, "side");
  &insertPublisher($key);
  &insertParty($key);
  &insertEngine($key);
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
    $selectQuery = "SELECT id FROM nn_$label WHERE lower($label) = lower(?);";
    $debug && print "$selectQuery\n";
    $selectStatement = $dbh -> prepare($selectQuery);
    $selectStatement -> execute($tmpFoo[$i]);
    $selectResult = $selectStatement->fetchrow_array();
    if (!defined $selectResult){
      $insertQuery = "INSERT INTO nn_$label ($label) VALUES (?) RETURNING id;";
      $debug && print "$insertQuery\n";
      $insertStatement = $dbh -> prepare($insertQuery);
      $insertStatement -> execute($tmpFoo[$i]);
      my $foo_rec = $insertStatement->fetchrow_hashref();
      $insertStatement -> finish();
      $tmpFoo[$i] = $$foo_rec{"id"};
    } else {
      $tmpFoo[$i] = $selectResult;
    }
    $insertQuery = "INSERT INTO nn_map_$label (id_game, id_$label) VALUES (?,?);";
    $insertStatement = $dbh -> prepare($insertQuery);
    $insertStatement -> execute($id_game, $tmpFoo[$i]);
    $insertStatement -> finish();
    $selectStatement -> finish();
  }
}

sub insertEngine {
  my $game_name = shift();
  unless ($game{$game_name}{"engine"}) {
    return;
  }
  my ($insertQuery, $selectResult, $selectQuery, $selectStatement, $insertStatement);
  my @tmpFoo = split(/","/, $game{$game_name}{"engine"});
  $debug && print $game{$game_name}{"engine"}."\nNo. of engines: ".@tmpFoo."\n";
  for (my $i = 0; $i < @tmpFoo; $i++){
    (my $engineFoo, my $engineUrlFoo) = split(/"-"/, $tmpFoo[$i]);
    $selectQuery = "SELECT id FROM nn_engine WHERE lower(engine) = lower(?);";
    $debug && print "Engine: $engineFoo - URL: $engineUrlFoo\n$selectQuery\n";
    $selectStatement = $dbh -> prepare($selectQuery);
    $selectStatement -> execute($engineFoo);
    $selectResult = $selectStatement->fetchrow_array();
    if (!defined $selectResult){
      $insertQuery = "INSERT INTO nn_engine (engine) VALUES (?) RETURNING id;";
      $debug && print "$insertQuery\n";
      $insertStatement = $dbh -> prepare($insertQuery);
      $insertStatement -> execute($engineFoo);
      my $foo_rec = $insertStatement->fetchrow_hashref();
      $insertStatement -> finish();
      $tmpFoo[$i] = $$foo_rec{"id"};
    } else {
      $tmpFoo[$i] = $selectResult;
    }
    $insertQuery = "INSERT INTO nn_map_engine (id_game, id_engine, url) VALUES (?,?,?);";
    $insertStatement = $dbh -> prepare($insertQuery);
    $insertStatement -> execute($id_game, $tmpFoo[$i], $engineUrlFoo);
    $insertStatement -> finish();
    $selectStatement -> finish();
  }
}

sub insertPublisher {
  my $game_name = shift();
  unless ($game{$game_name}{"publisher"}) {
    return;
  }
  my ($insertQuery, $selectResult, $selectQuery, $selectStatement, $insertStatement);
  my @tmpFoo = split(/","/, $game{$game_name}{"publisher"});
  $debug && print $game{$game_name}{"publisher"}."\nNo. of publishers: ".@tmpFoo."\n";
  for (my $i = 0; $i < @tmpFoo; $i++){
    (my $publisher, my $publisherUrl) = split(/"-"/, $tmpFoo[$i]);
    $selectQuery = "SELECT id FROM nn_publisher WHERE lower(publisher) = lower(?);";
    $debug && print "Publisher: $publisher - URL: $publisherUrl\n$selectQuery\n";
    $selectStatement = $dbh -> prepare($selectQuery);
    $selectStatement -> execute($publisher);
    $selectResult = $selectStatement->fetchrow_array();
    if (!defined $selectResult){
      $insertQuery = "INSERT INTO nn_publisher (publisher, url) VALUES (?,?) RETURNING id;";
      $debug && print "$insertQuery\n";
      $insertStatement = $dbh -> prepare($insertQuery);
      $insertStatement -> execute($publisher,$publisherUrl);
      my $foo_rec = $insertStatement->fetchrow_hashref();
      $insertStatement -> finish();
      $tmpFoo[$i] = $$foo_rec{"id"};
    } else {
      $tmpFoo[$i] = $selectResult;
    }
    $insertQuery = "INSERT INTO nn_map_publisher (id_game, id_publisher) VALUES (?,?);";
    $insertStatement = $dbh -> prepare($insertQuery);
    $insertStatement -> execute($id_game, $tmpFoo[$i]);
    $insertStatement -> finish();
    $selectStatement -> finish();
  }
}

sub insertParty {
  my $game_name = shift();
  unless ($game{$game_name}{"party"}) {
    return;
  }
  my ($insertQuery, $selectResult, $selectQuery, $selectStatement, $insertStatement);
  my @tmpFoo = split(/","/, $game{$game_name}{"party"});
  $debug && print $game{$game_name}{"party"}."\nNo. of parties: ".@tmpFoo."\n";
  for (my $i = 0; $i < @tmpFoo; $i++){
    (my $party, my $num_pl) = split(/"-"/, $tmpFoo[$i]);
    $selectQuery = "SELECT id FROM nn_party WHERE lower(party) = lower(?);";
    $debug && print "Party: $party - Players: $num_pl\n$selectQuery\n";
    $selectStatement = $dbh -> prepare($selectQuery);
    $selectStatement -> execute($party);
    $selectResult = $selectStatement->fetchrow_array();
    if (!defined $selectResult){
      $insertQuery = "INSERT INTO nn_party (party) VALUES (?) RETURNING id;";
      $debug && print "$insertQuery\n";
      $insertStatement = $dbh -> prepare($insertQuery);
      $insertStatement -> execute($party);
      my $foo_rec = $insertStatement->fetchrow_hashref();
      $insertStatement -> finish();
      $tmpFoo[$i] = $$foo_rec{"id"};
    } else {
      $tmpFoo[$i] = $selectResult;
    }
    $insertQuery = "INSERT INTO nn_map_party (id_game, num_players, id_party) VALUES (?,?,?);";
    $insertStatement = $dbh -> prepare($insertQuery);
    $insertStatement -> execute($id_game, $num_pl, $tmpFoo[$i]);
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
sub trimMultifields  {
  my $text = shift();
  chomp $text;
  $text =~ s/(.*{"|"}.*)//g;
  return $text;
}
