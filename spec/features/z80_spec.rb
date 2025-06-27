require "rails_helper"

# require "open-uri"
require "zip"

RSpec.describe "Spectrum Emulator" do
  before do
    z80_game.save!
  end

  let(:expected_hz) { (ENV['HZ'] || "11.21").to_f }

  # disable for now, as we don't want to run the test twice really
  # xcontext "with match day" do
  #   let(:z80_game) { build(:game, :match_day) }
  #
  #   it "loads the emulator", :js do
  #     visit '/'
  #     click_on z80_game.name
  #
  #     expect(page).to have_content 'Refresh Interval'
  #     speed = measure_speed_in_hz
  #     p "Speed #{speed} Hz"
  #     expect(speed).to be >= expected_hz
  #   end
  # end

  context "with documented Z80 test" do
    let(:z80_game) { build(:game, :z80_test) }
    let(:version) { "1.2a" }
    let(:z80_test_url) { "https://github.com/raxoft/z80test/releases/download/v#{version}/z80test-#{version}.zip" }
    let(:faraday) {
      Faraday.new do |f|
        f.response :raise_error
        f.response :follow_redirects
        f.adapter Faraday.default_adapter
      end
    }
    let(:z80full_directory) { Rails.root.join("public", "z80test") }

    before do
      unless File.exist? z80full_directory
        load_tapfile z80_test_url, z80full_directory
      end

      visit '/'
      click_on z80_game.name
    end

    # So far executed 158 of the tests, most of them fail.
    # Test 159 IM N seems to hang after setting IM back to 2
    # Disabled some of the IM routines, and now completes with
    # 142 of 160 tests failed and then crashes with
    # C Nonsense in BASIC 20:1
    # Test 0 checksum fails - prog loaded to 0x8000...
    # Including DAA, CPL, NEG,
    # ADD A,N, ADC A,N, SUB A,N, SBC A,N, AND N
    # Passes XOR N and OR N (18, 19)
    # Fails CP N (20), ALO A, A(21) fails
    # fails RLCA (29) RRCA (30) RLA (31) RRA (32)
    #
    # RLD RRD pass (33, 34) RLC A and RRC A (35, 36) pass
    # RL A and RR A pass (37, 38) SLA A and SRA A (39, 40) pass
    # SLIA SRL A (41, 42) pass
    #
    # RLC [R, (HL)] 43 fails
    #
    # SRO (XY) (51) passes
    # INC A (53) and DEC A (54) fail INC X DEC X (57, 58) fail
    # INC RR, DEC RR (63, 64) fail
    # ADC HL, RR (68) fail
    # SBC HL, RR (69) pass
    # BIT N,A (70) fails
    # RES N, A (00) up to RES N, (XY), R fail
    # LDI (85), LDD (86) LDIR (87), LDDR (88) pass
    # JR N (115) fails

    it "loads the emulator", :js do
      # check that Elm is running
      expect(page).to have_content 'Refresh Interval'

      sleep 6
      # This is very slow, but calling send_keys
      # with a string on an array
      # is too quick
      # data = "20eThis is a comment"
      # Load tape
      spectrum = find("#spectrum")
      data = 'j""'
      data.each_char do |k|
        spectrum.send_keys k
      end
      spectrum.send_keys [:enter]
      # # x.send_keys data
      # # x.send_keys data.split("")
      # x.send_keys [:enter]

      if ENV.key? "TEST_LOOP"
        1.upto(60).each do |i|
          sleep 30
          spectrum.send_keys 'y'
          puts "Loop #{i}"
        end
      end

      speed = measure_speed_in_hz spectrum
      expect(speed).to be > expected_hz
      puts "Speed #{speed} Hz"
    end
  end

  def measure_speed_in_hz spectrum
    # Test emulation speed in Hz
    low = 0
    high = page.find("#hz").text.to_f
    # wait for speed to hit a steady state
    while high - low > 0.02
      times = 1.upto(8).map do
        sleep 0.4
        page.find("#hz").text.to_f
      end
      # p "Speed Times #{times.sort}"
      low = times.min
      high = times.max
      # response to 'scroll?' question if required
      spectrum.send_keys 'y'
    end
    high
  end

  def load_tapfile input_url, output_directory
    zip_data = []
    faraday.get(input_url) do |req|
      req.options.on_data = Proc.new do |chunk, size|
        zip_data  << chunk
      end
    end
    zipdata = zip_data.join
    zipfile = StringIO.new zipdata
    Dir.mkdir output_directory
    Zip::InputStream.open(zipfile) do |zip_stream|
      while (entry = zip_stream.get_next_entry)
        entry_filename = entry.name.split("/").last
        if entry.name.ends_with?(".tap")
          data = entry.get_input_stream.read
          File.open("#{output_directory}/#{entry_filename}", "wb+") do |file|
            file.write(data)
          end
        end
      end
    end
  end
end