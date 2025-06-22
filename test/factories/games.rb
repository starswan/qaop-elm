FactoryBot.define do
  factory :game do
    trait :one do
        name { 'MyString' }
      tapfile { 'MyString' }
    end

    trait :two do
      name { 'MyString' }
      tapfile { 'MyString' }
    end

    trait :match_day do
      name { 'Match Day' }
      tapfile { 'MATCHDAY.tap' }
    end

    trait :z80_test do
      name { 'Z80 Test' }
      tapfile { 'z80test/z80doc.tap' }
    end

    trait :z80_test_full do
      name { 'Z80 Full Test' }
      tapfile { 'z80test/z80full.tap' }
    end

    trait :z80_test_flags do
      name { 'Z80 Full Test' }
      tapfile { 'z80test/z80docflags.tap' }
    end

    trait :z80_full_flags do
      name { 'Z80 Full Test' }
      tapfile { 'z80test/z80flags.tap' }
    end
  end
end
