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

    trait :z80_test_doc do
      name { 'Doc' }
      tapfile { 'z80test/z80doc.tap' }
    end

    trait :z80_test_full do
      name { 'Full' }
      tapfile { 'z80test/z80full.tap' }
    end

    trait :z80_test_flags do
      name { 'DocFlags' }
      tapfile { 'z80test/z80docflags.tap' }
    end

    trait :z80_full_flags do
      name { 'FullFlags' }
      tapfile { 'z80test/z80flags.tap' }
    end
  end
end
