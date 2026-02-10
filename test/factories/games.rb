FactoryBot.define do
  factory :game do
    filetype {  "TAP" }

    trait :match_day do
      name { 'MatchDay' }
      download_url { "https://www.worldofspectrum.org/pub/sinclair/games/m/MatchDay.tap.zip" }
      filetype { "TAP" }
      filename { 'MATCHDAY.TAP' }
    end

    # Sadly this TAP file appears to be broken
    # trait :chuckie_egg do
    #   name { "ChuckieEgg" }
    #   download_url { "https://www.worldofspectrum.org/pub/sinclair/games/c/ChuckieEgg.tap.zip" }
    #   filetype { "tap" }
    #   filename { "Chuckie Egg.tap" }
    # end

    trait :colossus do
      name { "ColossusChess" }
      download_url { "https://www.worldofspectrum.org/pub/sinclair/games/c/ColossusChess4.tap.zip" }
      filetype { "TAP" }
      filename { "COLOSSUS.TAP" }
    end

    trait :checkers do
      name { "Checkers" }
      download_url { "https://www.worldofspectrum.org/pub/sinclair/games/c/Checkers.tap.zip" }
      filetype { "tap" }
      filename { "checkers.tap" }
    end

    trait :manic_miner do
      name { 'ManicMiner' }
      download_url { 'https://www.worldofspectrum.org/pub/sinclair/games/m/ManicMiner.tap.zip' }
      filetype { 'TAP' }
      filename { 'MANIC.TAP' }
    end

    trait :cyrus do
      name { 'CyrusChess' }
      download_url { "https://www.worldofspectrum.org/pub/sinclair/games/c/CyrusISChess.tap.zip" }
      # picture_url { "https://worldofspectrum.org/pub/sinclair/screens/in-game/c/CyrusISChess.gif" }
      filetype { "tap" }
      filename { 'CYRCHESS.TAP' }
    end

    trait :z80_test_flags do
      name { 'Flags' }
      download_url { "https://github.com/raxoft/z80test/releases/download/v1.2a/z80test-1.2a.zip" }
      filetype { "tap" }
      filename { 'z80docflags.tap' }
    end

    trait :z80_test_doc do
      name { 'Regs' }
      download_url { "https://github.com/raxoft/z80test/releases/download/v1.2a/z80test-1.2a.zip" }
      filename { 'z80doc.tap' }
      filetype { "tap" }
    end

    trait :z80_full_flags do
      name { 'FullFlags' }
      download_url { "https://github.com/raxoft/z80test/releases/download/v1.2a/z80test-1.2a.zip" }
      filename { 'z80flags.tap' }
      filetype { "tap" }
    end

    trait :z80_test_full do
      name { 'Full' }
      download_url { "https://github.com/raxoft/z80test/releases/download/v1.2a/z80test-1.2a.zip" }
      filename { 'z80full.tap' }
      filetype { "tap" }
    end

    trait :football_manager do
      filetype { "tap" }
      name { 'Football Manager' }
      filename { 'FOOTMANG.TAP' }
      picture_url { "https://worldofspectrum.net/pub/sinclair/screens/in-game/f/FootballManager.gif" }
      download_url { "https://worldofspectrum.net/pub/sinclair/games/f/FootballManager.tap.zip" }
    end
  end
end
