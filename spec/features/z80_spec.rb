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
    let(:z80_game) { ENV.fetch("Z80_TEST", football_manager.name) }
    # let(:z80_game) { ENV.fetch("Z80_TEST", matchday.name) }

    let(:programs_by_name) {
      [flags, regs, miner, matchday, full_flags, full, cyrus, football_manager].index_by(&:name)
    }
    let(:scripts) {
      {
        matchday.name => ->(spectrum) {
          delay_and_send(spectrum, 200, "")
          delay_and_send(spectrum, 1050, "")
          # Start 1 player match day (with kempston, so kicks all the time)
          delay_and_send(spectrum, 1150, "")

          measure_speed_in_hz
        },
        cyrus.name => ->(spectrum) {
          # square colours
          delay_and_send(spectrum, 300, "a0724")
          # Level 3 demo mode Cyrus vs Cyrus
          delay_and_send(spectrum, 450, "ld")

          measure_speed_in_hz
        },
        football_manager.name => ->(spectrum) {
          # Player name
          delay_and_send(spectrum, 480, "robot")
          # select Norwich City
          delay_and_send(spectrum, 920, "11")
          # select beginner
          delay_and_send(spectrum, 1040, "1")
          # select white team colours
          delay_and_send(spectrum, 1200, "7")
          # continue from main menu
          delay_and_send(spectrum, 1700, "99")
          #  Hit ENTER to start first match
          delay_and_send(spectrum, 1900, "")
          # continue into match
          delay_and_send(spectrum, 2220, "99")

          measure_speed_in_hz do
            spectrum.send_keys :enter
          end
          spectrum.send_keys :enter
          measure_speed_in_hz
        }
      }
    }

    let(:times) {
      {
        flags.name => 7200,
        regs.name => 13000,
        full_flags.name => 7900,
        full.name => 14000,
      }
    }

    before do
      programs_by_name.fetch(z80_game).save!
      visit '/'
    end

    # Flags: 015 of 160 tests failed.
    # 052 SRO (XY), R (DD CB 00 00) 334E5D5A expected 0AF8B1A8
    # 074 BIT N,(XY)- DD CB xx 40-47 (undoc?) E3DC0E5A exp 6870B827
    # 089 LDIR-> NOP' (copying X -> X)
    # 090 LDDR ->NOP',
    # 098
    # 099
    # 100
    # 101
    # 102
    # 103
    # 107 OUTI
    # 108 OUTD
    # 109 OTIR
    # 110 OTDR
    # 157 LD A,R
    #
    # Regs: 024 of 160 tests failed.
    # 52 SRO (XY) ,R     F783EB33 expected 31DC0D48
    # 74 BIT N,(XY)-     BB3CB5FB expected 62003A45
    # 79 SET N,(XY),R  DD CB xx C0
    # 84 RES N,(XY),R  DD CB xx 80
    # 89 LDIR->NOP'      83DCFE53 expected EF3C3C61
    # 90 LDDR->NOP',     25AB70C9 expected
    # 96 IN R, (C)
    # 98 INI
    # 99 IND
    # 100 INIR
    # 101 INDR
    # 102 INIR NOP'
    # 103 INDR NOP'
    # 105 OUT (C), R
    # 107 OUTI
    # 108 OUTD
    # 109 OTIR
    # 110 OTDR
    # 122 RETN
    # 123 RETI
    # 124 RETI/RETN
    # 154 LD I,A
    # 156 LD A,I
    # 157 LD A,R
    #
    # FullFlags - 019 of 160 tests failed
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

      script = scripts.fetch(z80_game, -> (spectrum) {
         measure_speed_in_hz do
          spectrum.send_keys 'y'
        end.tap do
          if times.key? z80_game
            while cpu_count.text.to_i < times.fetch(z80_game)
              sleep 10
              # response to 'scroll?' question if required
              spectrum.send_keys 'y'
            end
          end
        end
      })

      speed = script.call(spectrum)

      expect(speed).to be > expected_hz
      puts "Speed #{speed} Hz"
    end
  end

  def delay_and_send(spectrum, time_until, keys)
    cpu_count = find("#cyclecount")

    while cpu_count.text.to_i < time_until
      sleep 0.5
    end
    keys.each_char do |k|
      spectrum.send_keys k
    end
    spectrum.send_keys [:enter]
  end

  def measure_speed_in_hz
    # Test emulation speed in Hz
    low = 0
    high = page.find("#hz").text.to_f
    # wait for speed to hit a steady state
    while high - low > 0.02
      times = 1.upto(8).map do
        sleep 0.4
        page.find("#hz").text.to_f
      end
      low = times.min
      high = times.max
      yield if block_given?
    end
    high
  end
end
