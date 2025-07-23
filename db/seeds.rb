# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rails db:seed command (or created alongside the database with db:setup).
#
# Examples:
#
#   movies = Movie.create([{ name: 'Star Wars' }, { name: 'Lord of the Rings' }])
#   Character.create(name: 'Luke', movie: movies.first)
# Game.create! :name => 'Match Day', :tapfile => 'MATCHDAY.tap'

"https://worldofspectrum.org//pub/sinclair/screens/in-game/m/MatchDay.gif"

Game.create! :name => 'Match Day', :filename => 'MATCHDAY.TAP',
             picture_url: "https://worldofspectrum.org/pub/sinclair/screens/in-game/m/MatchDay.gif",
             download_url: "https://www.worldofspectrum.org/pub/sinclair/games/m/MatchDay.tap.zip",
             filetype: "tap"

Game.create! :name => 'Football Manager', :filename => 'FOOTMANG.TAP',
             picture_url: "https://worldofspectrum.net/pub/sinclair/screens/in-game/f/FootballManager.gif",
             download_url: "https://worldofspectrum.net/pub/sinclair/games/f/FootballManager.tap.zip",
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

#  see https://github.com/raxoft/z80test
# Game.create! :name => 'Full Z80 Test', :tapfile => 'z80test/z80full.tap'
# Game.create! :name => 'Documented Z80 Test', :tapfile => 'z80test/z80doc.tap'
# Game.create! :name => 'Full Flags Test', :tapfile => 'z80test/z80flags.tap'

# t.string "name"
# t.string "tapfile"
# t.datetime "created_at", precision: nil, null: false
# t.datetime "updated_at", precision: nil, null: false
# t.string "picture_url"
# t.string "download_url"
# t.string "filetype"
