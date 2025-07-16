FactoryBot.define do
  factory :game do
    trait :match_day do
      name { 'Match Day' }
      tapfile { 'MATCHDAY.tap' }
    end

    trait :z80_test_flags do
      name { 'Flags' }
      directory { "z80test-1.2a" }
      download_url { "https://github.com/raxoft/z80test/releases/download/v1.2a/z80test-1.2a.zip" }
      filetype {  "tap" }
      filename { 'z80docflags.tap' }
    end

    trait :z80_test_doc do
      name { 'Regs' }
      directory { "z80test-1.2a" }
      download_url { "https://github.com/raxoft/z80test/releases/download/v1.2a/z80test-1.2a.zip" }
      filetype {  "tap" }
      filename { 'z80doc.tap' }
    end

    trait :z80_full_flags do
      name { 'FullFlags' }
      tapfile { 'z80test/z80flags.tap' }
    end

    trait :z80_test_full do
      name { 'Full' }
      tapfile { 'z80test/z80full.tap' }
    end

  end
end
