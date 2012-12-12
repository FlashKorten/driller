#!/usr/bin/perl

#
# Importfile:
# "NAME" {
#   gametime_start - gametime_end
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

#my $driver = "DBI:mysql:$db:$host","$user", "$password",{ PrintError => 0}) || die $DBI::errstr;

# my $dbh = DBI->connect("DBI:mysql:$db:$host", "$user", "$password", { PrintError => 0}) || die $DBI::errstr;


my $driver = "DBI:Pg:database=$db;host=$host;port=$port";
my $dbh = DBI->connect($driver,$user,$password) or die $DBI::errstr;

die "USAGE: nemesimporter.pl IMPORTFILE\n" unless(@ARGV == 1);

my $importfile = shift @ARGV;

open(IMPORT_FILE,"< $importfile") or die "Konnte Importdatei $importfile nicht oeffnen...\n";

my $debug = 0;

my %game;
my $game;
my $tmp;
my $time_01;
my $time_02;
my $num_pl_01;
my $num_pl_02;
my $id_bgg;

while(<IMPORT_FILE>){
	if (m/^\s*$/) {next;}
	if (m/^\s*".*"\s{\s*$/) {
		chomp;
		#s/[^"]*"\([^"]*\)".*$/$1/;
		s/^[^"]*"//;
		s/"[^"]*$//;
		$game = $_;
		$game{$game}{'name'} = $game;
		$time_01 = <IMPORT_FILE>;
		chomp($time_01);
		$time_02 = $time_01;
		$time_01 =~ s/^\s*//;
		$time_01 =~ s!\s*/.*!!;
		$time_02 =~ s!.*/\s*!!;
		$time_02 =~ s/\s*$//;
		$game{$game}{'time_01'} = $time_01;
		$game{$game}{'time_02'} = $time_02;
		$id_bgg = <IMPORT_FILE>;
		chomp($id_bgg);
		$id_bgg =~ s/^\s*//;
		$id_bgg =~ s/\s*$//;
		$game{$game}{'id_bgg'} = $id_bgg;
	} elsif (m/{.*}/) {
		chomp;
		$tmp = $_;
		$tmp =~ s/^\s*//;
		$tmp =~ s/{.*//;
		s/.*{"//;
		s/"}.*//;
		$game{$game}{$tmp} = $_;
	}
}

close(IMPORT_FILE);

my @tmp;
my $id_game;
my $ergebnis;
my $query;
my $sth_select;
my $insert;
my $sth_insert;

foreach my $key(sort(keys %game)){
				# Insert the game:

				if ($game{$key}{'name'} =~ m/ - /){
					$game{$key}{'subtitle'} = $game{$key}{'name'};
					$game{$key}{'name'} =~ s/ - .*$//;
					$game{$key}{'subtitle'} =~ s/^.* - //;
				} else {
					$game{$key}{'subtitle'} = "";
				}
				$query = "SELECT id FROM nn_game WHERE lower(game) = lower('".$game{$key}{'name'}."');";
				$debug && print "$query\n";
				$sth_select = $dbh -> prepare($query);
				$sth_select -> execute();
				$ergebnis = $sth_select->fetchrow_array();
				if (!defined $ergebnis) {
					$insert = "INSERT INTO nn_game (game, subtitle, gametime_start, gametime_end, id_bgg) VALUES ('"
					.$game{$key}{'name'}."','".$game{$key}{'subtitle'}."','"
					.$game{$key}{'time_01'}."','".$game{$key}{'time_02'}."','".$game{$key}{'id_bgg'}."');";

					$sth_insert = $dbh -> prepare($insert);
					$sth_insert -> execute();
					$sth_insert -> finish();
					$query = "SELECT id FROM nn_game WHERE game = '".$game{$key}{'name'}."';";
					$debug && print "$query\n";
					$sth_select = $dbh -> prepare($query);
					$sth_select -> execute();
					$id_game = $sth_select->fetchrow_array();
				} else {
								print "---------------------------------\n";
								print "Eintrag: $key schon vorhanden...\nBitte zuerst von Hand austragen\n";
								print "---------------------------------\n";
								next;
				}
				$debug && print "Game $key has ID #$id_game;\n";
				# Done inserting the game...

				# inserting the areas...
				@tmp = split(/","/, $game{$key}{"area"});
				$debug && print "No. of areas: ".@tmp."\n";
				for (my $i = 0; $i < @tmp; $i++){
					$query = "SELECT id FROM nn_area WHERE lower(area) = lower('".$tmp[$i]."');";
					$debug && print "$query\n";
					$sth_select = $dbh -> prepare($query);
					$sth_select -> execute();
					$ergebnis = $sth_select->fetchrow_array();
						if (!defined $ergebnis){
							$insert = "INSERT INTO nn_area (area) VALUES ('".$tmp[$i]."');";
							$debug && print "$insert\n";
							$sth_insert = $dbh -> prepare($insert);
							$sth_insert -> execute();
							$sth_insert -> finish();
							$query = "SELECT id FROM nn_area WHERE area = '".$tmp[$i]."';";
							$debug && print "$query\n";
							$sth_select = $dbh -> prepare($query);
							$sth_select -> execute();
							$tmp[$i] = $sth_select->fetchrow_array();
						} else {
										$tmp[$i] = $ergebnis;
						}
						$insert = "INSERT INTO nn_map_area (id_game, id_area) VALUES (".$id_game.",".$tmp[$i].");";
						$sth_insert = $dbh -> prepare($insert);
						$sth_insert -> execute();
						$sth_insert -> finish();
						$sth_select -> finish();
				}
				# done inserting the areas...

				# inserting the authors...
				@tmp = split(/","/, $game{$key}{"author"});
				$debug && print "No. of authors: ".@tmp."\n";
				for (my $i = 0; $i < @tmp; $i++){
					$query = "SELECT id FROM nn_author WHERE lower(author) = lower('".$tmp[$i]."');";
					$debug && print "$query\n";
					$sth_select = $dbh -> prepare($query);
					$sth_select -> execute();
					$ergebnis = $sth_select->fetchrow_array();
						if (!defined $ergebnis){
							$insert = "INSERT INTO nn_author (author) VALUES ('".$tmp[$i]."');";
							$debug && print "$insert\n";
							$sth_insert = $dbh -> prepare($insert);
							$sth_insert -> execute();
							$sth_insert -> finish();
							$query = "SELECT id FROM nn_author WHERE author = '".$tmp[$i]."';";
							$debug && print "$query\n";
							$sth_select = $dbh -> prepare($query);
							$sth_select -> execute();
							$tmp[$i] = $sth_select->fetchrow_array();
						} else {
										$tmp[$i] = $ergebnis;
						}
						$insert = "INSERT INTO nn_map_author (id_game, id_author) VALUES (".$id_game.",".$tmp[$i].");";
						$sth_insert = $dbh -> prepare($insert);
						$sth_insert -> execute();
						$sth_insert -> finish();
						$sth_select -> finish();
				}
				# done inserting the authors...

				# inserting the genres...
				@tmp = split(/","/, $game{$key}{"genre"});
				$debug && print "No. of genres: ".@tmp."\n";
				for (my $i = 0; $i < @tmp; $i++){
					$query = "SELECT id FROM nn_genre WHERE lower(genre) = lower('".$tmp[$i]."');";
					$debug && print "$query\n";
					$sth_select = $dbh -> prepare($query);
					$sth_select -> execute();
					$ergebnis = $sth_select->fetchrow_array();
						if (!defined $ergebnis){
							$insert = "INSERT INTO nn_genre (genre) VALUES ('".$tmp[$i]."');";
							$debug && print "$insert\n";
							$sth_insert = $dbh -> prepare($insert);
							$sth_insert -> execute();
							$sth_insert -> finish();
							$query = "SELECT id FROM nn_genre WHERE genre = '".$tmp[$i]."';";
							$debug && print "$query\n";
							$sth_select = $dbh -> prepare($query);
							$sth_select -> execute();
							$tmp[$i] = $sth_select->fetchrow_array();
						} else {
										$tmp[$i] = $ergebnis;
						}
						$insert = "INSERT INTO nn_map_genre (id_game, id_genre) VALUES (".$id_game.",".$tmp[$i].");";
						$sth_insert = $dbh -> prepare($insert);
						$sth_insert -> execute();
						$sth_insert -> finish();
						$sth_select -> finish();
				}
				# done inserting the genres...

				# inserting the themes...
				@tmp = split(/","/, $game{$key}{"theme"});
				$debug && print "No. of themes: ".@tmp."\n";
				for (my $i = 0; $i < @tmp; $i++){
					$query = "SELECT id FROM nn_theme WHERE lower(theme) = lower('".$tmp[$i]."');";
					$debug && print "$query\n";
					$sth_select = $dbh -> prepare($query);
					$sth_select -> execute();
					$ergebnis = $sth_select->fetchrow_array();
						if (!defined $ergebnis){
							$insert = "INSERT INTO nn_theme (theme) VALUES ('".$tmp[$i]."');";
							$debug && print "$insert\n";
							$sth_insert = $dbh -> prepare($insert);
							$sth_insert -> execute();
							$sth_insert -> finish();
							$query = "SELECT id FROM nn_theme WHERE theme = '".$tmp[$i]."';";
							$debug && print "$query\n";
							$sth_select = $dbh -> prepare($query);
							$sth_select -> execute();
							$tmp[$i] = $sth_select->fetchrow_array();
						} else {
										$tmp[$i] = $ergebnis;
						}
						$insert = "INSERT INTO nn_map_theme (id_game, id_theme) VALUES (".$id_game.",".$tmp[$i].");";
						$sth_insert = $dbh -> prepare($insert);
						$sth_insert -> execute();
						$sth_insert -> finish();
						$sth_select -> finish();
				}
				# done inserting the themes...

				# inserting the mechanics...
				@tmp = split(/","/, $game{$key}{"mechanic"});
				$debug && print "No. of mechanics: ".@tmp."\n";
				for (my $i = 0; $i < @tmp; $i++){
					$query = "SELECT id FROM nn_mechanic WHERE lower(mechanic) = lower('".$tmp[$i]."');";
					$debug && print "$query\n";
					$sth_select = $dbh -> prepare($query);
					$sth_select -> execute();
					$ergebnis = $sth_select->fetchrow_array();
						if (!defined $ergebnis){
							$insert = "INSERT INTO nn_mechanic (mechanic) VALUES ('".$tmp[$i]."');";
							$debug && print "$insert\n";
							$sth_insert = $dbh -> prepare($insert);
							$sth_insert -> execute();
							$sth_insert -> finish();
							$query = "SELECT id FROM nn_mechanic WHERE mechanic = '".$tmp[$i]."';";
							$debug && print "$query\n";
							$sth_select = $dbh -> prepare($query);
							$sth_select -> execute();
							$tmp[$i] = $sth_select->fetchrow_array();
						} else {
										$tmp[$i] = $ergebnis;
						}
						$insert = "INSERT INTO nn_map_mechanic (id_game, id_mechanic) VALUES (".$id_game.",".$tmp[$i].");";
						$sth_insert = $dbh -> prepare($insert);
						$sth_insert -> execute();
						$sth_insert -> finish();
						$sth_select -> finish();
				}
				# done inserting the mechanics...

				# inserting the sides...
				@tmp = split(/","/, $game{$key}{"side"});
				$debug && print "No. of sides: ".@tmp."\n";
				for (my $i = 0; $i < @tmp; $i++){
					$query = "SELECT id FROM nn_side WHERE lower(side) = lower('".$tmp[$i]."');";
					$debug && print "$query\n";
					$sth_select = $dbh -> prepare($query);
					$sth_select -> execute();
					$ergebnis = $sth_select->fetchrow_array();
						if (!defined $ergebnis){
							$insert = "INSERT INTO nn_side (side) VALUES ('".$tmp[$i]."');";
							$debug && print "$insert\n";
							$sth_insert = $dbh -> prepare($insert);
							$sth_insert -> execute();
							$sth_insert -> finish();
							$query = "SELECT id FROM nn_side WHERE side = '".$tmp[$i]."';";
							$debug && print "$query\n";
							$sth_select = $dbh -> prepare($query);
							$sth_select -> execute();
							$tmp[$i] = $sth_select->fetchrow_array();
						} else {
										$tmp[$i] = $ergebnis;
						}
						$insert = "INSERT INTO nn_map_side (id_game, id_side) VALUES (".$id_game.",".$tmp[$i].");";
						$sth_insert = $dbh -> prepare($insert);
						$sth_insert -> execute();
						$sth_insert -> finish();
						$sth_select -> finish();
				}
				# done inserting the sides...

				# inserting the engines...
				$debug && print $game{$key}{"engine"}."\n";
				@tmp = split(/","/, $game{$key}{"engine"});
				$debug && print "No. of engines: ".@tmp."\n";
				for (my $i = 0; $i < @tmp; $i++){
					(my $engine, my $engine_url) = split(/"-"/, $tmp[$i]);
					$debug && print "Engine: $engine - URL: $engine_url\n";
					$query = "SELECT id FROM nn_engine WHERE lower(engine) = lower('".$engine."');";
					$debug && print "$query\n";
					$sth_select = $dbh -> prepare($query);
					$sth_select -> execute();
					$ergebnis = $sth_select->fetchrow_array();
						if (!defined $ergebnis){
							$insert = "INSERT INTO nn_engine (engine) VALUES ('".$engine."');";
							$debug && print "$insert\n";
							$sth_insert = $dbh -> prepare($insert);
							$sth_insert -> execute();
							$sth_insert -> finish();
							$query = "SELECT id FROM nn_engine WHERE engine = '".$engine."';";
							$debug && print "$query\n";
							$sth_select = $dbh -> prepare($query);
							$sth_select -> execute();
							$tmp[$i] = $sth_select->fetchrow_array();
						} else {
										$tmp[$i] = $ergebnis;
						}
						$insert = "INSERT INTO nn_map_engine (id_game, id_engine, url) VALUES (".$id_game.",".$tmp[$i].",'".$engine_url."');";
						$sth_insert = $dbh -> prepare($insert);
						$sth_insert -> execute();
						$sth_insert -> finish();
						$sth_select -> finish();
				}
				# done inserting the engines...

				# inserting the publishers...
				$debug && print $game{$key}{"publisher"}."\n";
				@tmp = split(/","/, $game{$key}{"publisher"});
				$debug && print "No. of publishers: ".@tmp."\n";
				for (my $i = 0; $i < @tmp; $i++){
					(my $publisher, my $publisher_url) = split(/"-"/, $tmp[$i]);
					$debug && print "Publisher: $publisher - URL: $publisher_url\n";
					$query = "SELECT id FROM nn_publisher WHERE lower(publisher) = lower('".$publisher."');";
					$debug && print "$query\n";
					$sth_select = $dbh -> prepare($query);
					$sth_select -> execute();
					$ergebnis = $sth_select->fetchrow_array();
						if (!defined $ergebnis){
							$insert = "INSERT INTO nn_publisher (publisher, url) VALUES ('".$publisher."','".$publisher_url."');";
							$debug && print "$insert\n";
							$sth_insert = $dbh -> prepare($insert);
							$sth_insert -> execute();
							$sth_insert -> finish();
							$query = "SELECT id FROM nn_publisher WHERE publisher = '".$publisher."';";
							$debug && print "$query\n";
							$sth_select = $dbh -> prepare($query);
							$sth_select -> execute();
							$tmp[$i] = $sth_select->fetchrow_array();
						} else {
										$tmp[$i] = $ergebnis;
						}
						$insert = "INSERT INTO nn_map_publisher (id_game, id_publisher) VALUES (".$id_game.",".$tmp[$i].");";
						$sth_insert = $dbh -> prepare($insert);
						$sth_insert -> execute();
						$sth_insert -> finish();
						$sth_select -> finish();
				}
				# done inserting the publishers...

				# inserting the parties...
				$debug && print $game{$key}{"party"}."\n";
				@tmp = split(/","/, $game{$key}{"party"});
				$debug && print "No. of parties: ".@tmp."\n";
				for (my $i = 0; $i < @tmp; $i++){
					(my $party, my $num_pl) = split(/"-"/, $tmp[$i]);
					$debug && print "Party: $party - Players: $num_pl\n";
					$query = "SELECT id FROM nn_party WHERE lower(party) = lower('".$party."');";
					$debug && print "$query\n";
					$sth_select = $dbh -> prepare($query);
					$sth_select -> execute();
					$ergebnis = $sth_select->fetchrow_array();
						if (!defined $ergebnis){
							$insert = "INSERT INTO nn_party (party) VALUES ('".$party."');";
							$debug && print "$insert\n";
							$sth_insert = $dbh -> prepare($insert);
							$sth_insert -> execute();
							$sth_insert -> finish();
							$query = "SELECT id FROM nn_party WHERE party = '".$party."';";
							$debug && print "$query\n";
							$sth_select = $dbh -> prepare($query);
							$sth_select -> execute();
							$tmp[$i] = $sth_select->fetchrow_array();
						} else {
										$tmp[$i] = $ergebnis;
						}
						$insert = "INSERT INTO nn_map_party (id_game, num_players, id_party) VALUES (".$id_game.",".$num_pl.",".$tmp[$i].");";
						$sth_insert = $dbh -> prepare($insert);
						$sth_insert -> execute();
						$sth_insert -> finish();
						$sth_select -> finish();
				}
				# done inserting the parties...

}
