# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rails db:seed command (or created alongside the database with db:setup).
#
# Can't persuade this to run (yet) 23rd July 2025 - Executes IM1 and then stops
# Game.create! :name => 'Match Day', :filename => 'MATCHDAY.TAP',
#              picture_url: "https://worldofspectrum.org/pub/sinclair/screens/in-game/m/MatchDay.gif",
#              download_url: "https://www.worldofspectrum.org/pub/sinclair/games/m/MatchDay.tap.zip",
#              filetype: "tap"

# Runs but seems to always produce the same cards. Crashed on second run with unknown variable error
# Game.create! :name => '8 Cards Patience' , :filename => '8CARDPAT.TAP',
#              picture_url: "https://www.worldofspectrum.org/pub/sinclair/screens/in-game/e/EightCardsPatience.gif",
#              download_url: "https://www.worldofspectrum.org/pub/sinclair/games/e/EightCardsPatience.tap.zip",
#              filetype: "tap"

Game.create! :name => 'Football Manager', :filename => 'FOOTMANG.TAP',
             picture_url: "https://worldofspectrum.net/pub/sinclair/screens/in-game/f/FootballManager.gif",
             download_url: "https://worldofspectrum.net/pub/sinclair/games/f/FootballManager.tap.zip",
             filetype: "tap"

Game.create! :name => 'Cyrus IS Chess', :filename => 'CYRCHESS.TAP',
             picture_url: "https://worldofspectrum.org/pub/sinclair/screens/in-game/c/CyrusISChess.gif",
             download_url: "https://www.worldofspectrum.org/pub/sinclair/games/c/CyrusISChess.tap.zip",
             filetype: "tap"

Game.create! :name => 'Manic Miner', :filename => 'MANIC.TAP',
             picture_url: "https://www.worldofspectrum.org//pub/sinclair/screens/in-game/m/ManicMiner.gif",
             download_url: "https://www.worldofspectrum.org//pub/sinclair/games/m/ManicMiner.tap.zip",
             filetype: "tap"

{
  'z80docflags' => 'Documented Flags Test',
  'z80doc' => 'Documented Z80 Test',
  'z80flags' => 'Full Flags Test',
  'z80full' => 'Full Z80 Test',
}.each do |tap, name|
  Game.create! name: name, filename: "#{tap}.tap",
               download_url: "https://github.com/raxoft/z80test/releases/download/v1.2a/z80test-1.2a.zip",
               filetype: "tap"
end
