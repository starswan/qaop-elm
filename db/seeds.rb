# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rails db:seed command (or created alongside the database with db:setup).
#
# Examples:
#
#   movies = Movie.create([{ name: 'Star Wars' }, { name: 'Lord of the Rings' }])
#   Character.create(name: 'Luke', movie: movies.first)
Game.create! :name => 'Match Day', :tapfile => 'MATCHDAY.tap'
Game.create! :name => 'Football Manager', :tapfile => 'FOOTMANG.tap'
Game.create! :name => 'Full Z80 Test', :tapfile => 'z80test/z80full.tap'
