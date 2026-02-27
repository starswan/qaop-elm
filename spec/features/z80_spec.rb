require "rails_helper"

# require "open-uri"
require "zip"

RSpec.describe "Game" do
  let(:expected_hz) { (ENV['HZ'] || "9.8").to_f }

  context "with documented Z80 test" do
    let(:version) { "1.2a" }
    let(:faraday) {
      Faraday.new do |f|
        f.response :raise_error
        f.response :follow_redirects
        f.adapter Faraday.default_adapter
      end
    }
    let(:cyrus) { build(:game, :cyrus) }
    let(:football_manager) { build(:game, :football_manager) }
    let(:flags) { build(:game, :z80_test_flags) }
    let(:regs) { build(:game, :z80_test_doc) }
    let(:full_flags) { build(:game, :z80_full_flags) }
    let(:full) { build(:game, :z80_test_full) }
    let(:matchday) { build(:game, :match_day) }
    let(:miner) { build(:game, :manic_miner) }
    # ideally speed test should be BASIC so we see if compiling helps
    # but matchday is interesting because it only manages 20Hz in optimised mode
    let(:z80_game) { ENV.fetch("Z80TEST", football_manager.name) }
    # let(:z80_game) { ENV.fetch("Z80TEST", matchday.name) }

    let(:programs_by_name) {
      [flags, regs, miner, matchday, full_flags, full, cyrus, football_manager].index_by(&:name)
    }
    let(:scripts) {
      {
        matchday.name => lambda { |spectrum|
          delay_and_send(spectrum, 200, "")
          delay_and_send(spectrum, 780, "")
          # Start 1 player match day (with kempston, so kicks all the time)
          delay_and_send(spectrum, 880, "")

          measure_speed_in_hz 180
        },
        cyrus.name => lambda { |spectrum|
          # square colours
          delay_and_send(spectrum, 300, "a0724")
          # Level 3 demo mode Cyrus vs Cyrus
          delay_and_send(spectrum, 450, "ld")

          measure_speed_in_hz
        },
        flags.name => lambda { |spectrum|
          [2350, 4710, 5900, 7300, 8200, 8780, 9100, 9550].each do |t|
            delay_and_send_just(spectrum, t, "y")
          end

          sleep 5

          measure_speed_in_hz
        },
        regs.name => lambda { |spectrum|
          # 19    42    63?
          [4390, 8950, 11350, 13980, 15950, 16850, 17500, 18250].each do |t|
            delay_and_send_just(spectrum, t, "y")
          end

          sleep 5

          measure_speed_in_hz
        },
        full_flags.name => lambda { |spectrum|
          [2550, 5190, 6750, 8300, 9400, 10000, 10340, 10600].each do |t|
            delay_and_send_just(spectrum, t, "y")
          end

          sleep 3

          measure_speed_in_hz
        },
        full.name => lambda { |spectrum|
          [4750, 8350, 12250, 13200, 14100, 14650, 16000, 16550].each do |t|
            delay_and_send_just(spectrum, t, "y")
          end

          sleep 3

          measure_speed_in_hz
        },
        football_manager.name => lambda { |spectrum|
          # Player name
          delay_and_send(spectrum, 470, FFaker::Name.first_name.first(5))
          # select Plymouth Argyle
          delay_and_send(spectrum, 930, "99")
          delay_and_send(spectrum, 1080, "99")
          delay_and_send(spectrum, 1225, "44")
          # select beginner
          delay_and_send(spectrum, 1330, "1")
          # select white team colours
          delay_and_send(spectrum, 1410, "7")
          # continue from main menu
          delay_and_send(spectrum, 1825, "99")
          #  Hit ENTER to start first match
          delay_and_send(spectrum, 1980, "")
          # continue into match
          delay_and_send(spectrum, 2280, "99")

          # wait for match to actually start properly
          sleep 3

          measure_speed_in_hz do
            spectrum.send_keys :enter
          end
          # spectrum.send_keys :enter
          # measure_speed_in_hz
        },
      }
    }

    before do
      programs_by_name.fetch(z80_game).save!
      visit '/'
    end

    # Flags: 013 of 160 tests failed.
    # 098 INI
    # 099 IND
    # 100 INIR
    # 101 INDR
    # 102 INIR NOP'
    # 103 INDR NOP'
    # 107 OUTI
    # 108 OUTD
    # 109 OTIR
    # 110 OTDR
    # 157 LD A,R
    #
    # Regs: 014 of 160 tests failed.
    # 98 INI             DB9A76D8E1E expected 07D1B0D1
    # 99 IND
    # 100 INIR
    # 101 INDR
    # 102 INIR NOP'
    # 103 INDR NOP'
    # 105 OUT (C), R
    # 107 OUTI           F6A55EE0 expected 58C80D63
    # 108 OUTD
    # 109 OTIR
    # 110 OTDR
    # 122 RETN
    # 123 RETI
    # 124 RETI/RETN
    # 157 LD A,R
    #
    # FullFlags - 017 of 160 tests failed
    # 1. 001 SCF 958E3E1E expected 3EC05634
    # 2. 002 CCF F06C5F84 expected 5B2237AE
    # 3. 005 SCF (ST) 958E3E1E expected C62AF5EE
    # 4. 006 CCF (ST) F06C5F84 expected A3C89474

    # Full - 033 of 160 tests failed
    # 1. 74 BIT N,(XY)- DD CB xx 40
    # 2. 79 SET N, (XY), R       DD CB xx C0
    # 3. 84 RES N, (XY), R       DD CB xx 80

    it "loads the emulator", :js do
      click_on z80_game
      # check that Elm is running
      expect(page).to have_content 'Refresh Interval'

      cpu_count = find("#cyclecount")
      # wait for Spectrum ROM to initialize so that
      # we can send LOAD "" to load the tape
      while cpu_count.text.to_i < 80
        sleep 0.5
      end
      spectrum = find("#spectrum")

      # This is very slow, but calling send_keys
      # with a string on an array is too quick
      # due to the 50Hz keyboard polling rate
      # Load tape
      data = 'j""'
      data.each_char do |k|
        spectrum.send_keys k
      end
      spectrum.send_keys [:enter]

      script = scripts.fetch(z80_game, lambda { |spectrum|
        measure_speed_in_hz do
          spectrum.send_keys 'y'
        end
      })

      speed = script.call(spectrum)

      expect(speed).to be > expected_hz
      puts "Speed #{speed} Hz"
    end
  end

  def delay_and_send_just(spectrum, time_until, keys)
    cpu_count = find("#cyclecount")

    while cpu_count.text.to_i < time_until
      sleep 0.5
    end
    keys.each_char do |k|
      spectrum.send_keys k
    end
  end

  def delay_and_send(spectrum, time_until, keys)
    delay_and_send_just(spectrum, time_until, keys)

    spectrum.send_keys [:enter]
  end

  def measure_speed_in_hz(max = 10_000_000)
    # Test emulation speed in Hz
    low = 0
    high = page.find("#hz").text.to_f
    # wait for speed to hit a steady state
    while high - low > 0.02 && page.find("#elapsed").text.to_i < max
      times = 0.upto(4).map do
        sleep 1
        page.find("#hz").text.to_f
      end
      low = times.min
      high = times.max
      yield if block_given?
    end
    high
  end
end
