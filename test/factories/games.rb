FactoryBot.define do
  factory :game do
    filetype {  "tap" }

    trait :match_day do
      name { 'Match Day' }
      tapfile { 'MATCHDAY.tap' }
    end

    trait :z80_test_flags do
      name { 'Flags' }
      download_url { "https://github.com/raxoft/z80test/releases/download/v1.2a/z80test-1.2a.zip" }
      filetype {  "tap" }
      filename { 'z80docflags.tap' }
    end

    trait :z80_test_doc do
      name { 'Regs' }
      download_url { "https://github.com/raxoft/z80test/releases/download/v1.2a/z80test-1.2a.zip" }
      filename { 'z80doc.tap' }
    end

    trait :z80_full_flags do
      name { 'FullFlags' }
      download_url { "https://github.com/raxoft/z80test/releases/download/v1.2a/z80test-1.2a.zip" }
      filename { 'z80flags.tap' }
    end

    trait :z80_test_full do
      name { 'Full' }
      download_url { "https://github.com/raxoft/z80test/releases/download/v1.2a/z80test-1.2a.zip" }
      filename { 'z80full.tap' }
    end

    trait :football_manager do
      name {  'Football Manager' }
      filename { 'FOOTMANG.TAP' }
      picture_url { "https://worldofspectrum.net/pub/sinclair/screens/in-game/f/FootballManager.gif" }
      download_url { "https://worldofspectrum.net/pub/sinclair/games/f/FootballManager.tap.zip" }
    end
  end
end
