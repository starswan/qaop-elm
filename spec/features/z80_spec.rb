require "rails_helper"

# require "open-uri"
require "zip"

RSpec.describe "Spectrum Emulator" do
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
    let(:version) { "1.2a" }
    let(:z80_test_url) { "https://github.com/raxoft/z80test/releases/download/v#{version}/z80test-#{version}.zip" }
    let(:faraday) {
      Faraday.new do |f|
        f.response :raise_error
        f.response :follow_redirects
        f.adapter Faraday.default_adapter
      end
    }
    let!(:flags) { create(:game, :z80_test_flags) }
    let!(:regs) { create(:game, :z80_test_doc) }
    let!(:full_flags) { create(:game, :z80_full_flags) }
    let!(:full) { create(:game, :z80_test_full) }

    let(:z80base_directory) { Rails.root.join("public", "games") }
    let(:z80full_directory) { Rails.root.join("public", "games", "z80test") }
    let(:z80_game) { Game.find_by!(name: ENV.fetch("Z80_TEST", flags.name)) }

    let(:times) { {
      flags.name => 7800,
      regs.name => 12800,
      full_flags.name => 8300,
      full.name => 18000,
    }}

    before do
      FileUtils.mkdir_p(z80base_directory)
      unless File.exist? z80full_directory
        load_tapfile z80_test_url, z80full_directory
      end

      visit '/'
    end

    # Disabled some of the IM routines, and now completes.
    # Flags: 024 of 160 tests failed.
    # 51 SRO (XY) passes
    # 52 SRO (XY), R (DD CB 00 00)
    # 73 BIT N,(XY) passes
    # 74 BIT N,(XY)- DD CB xx 40-47 (undoc?) - same as DD CB 00 46
    # 89 LDIR-> NOP'. 90 LDDR ->NOP',
    # 93 CPIR, 94 CPDR
    # 96 -> 103 IN FE:FF -> BF
    # 107 OUTI, 108 OUTD, 109 OTIR, 110 OTDR
    # 156 LD A,I 157 LD A, R
    #
    # FullFlags - 028 tests failed
    #
    # Regs: 030 of 160 tests failed.
    # 52 SRO (XY) ,R (undocumented?) DD CB xx 00
    # 73 BIT N,(XY) passes
    # 74 BIT N,(XY)- DD CB xx 40
    # 78 SET N,(XY) passes
    # 79 SET N, (XY), R       DD CB xx C0
    # 83 RES N, (XY) passes
    # 84 RES N, (XY), R       DD CB xx 80
    # 89 LDIR->NOP'. 90 LDDR->NOP',
    # 93 CPIR, 94 CPDR
    # 96 -> 103 IN FE:FF -> BF
    # 105 OUT (C), R
    # 106 OUT (C), 0 passes
    # 107 OUTI, 108 OUTD, 109 OTIR, 110 OTDR
    # 122 RETN 123 RETI 124 RETI/RETN
    # 154 LD I,A 155 LD R, A
    # 156 LD A,I 157 LD A, R
    # 159 IM N

    it "loads the emulator", :js do
      click_on z80_game.name
      # check that Elm is running
      expect(page).to have_content 'Refresh Interval'

      cpu_count = find("#cyclecount")
      # wait for Spectrum ROM to initialize so that
      # we can send LOAD "" to load the tape
      while cpu_count.text.to_i < 80
        sleep 0.5
      end

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

      speed = measure_speed_in_hz spectrum
      if ENV.key? "Z80_TEST"
        while cpu_count.text.to_i < times.fetch(z80_game.name)
          sleep 5
          spectrum.send_keys 'y'
        end
      end

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