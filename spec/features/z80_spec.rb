require "rails_helper"

# require "open-uri"
require "zip"

RSpec.describe "Spectrum Emulator" do
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
    let!(:flags) { create(:game, :z80_test_flags) }
    let!(:regs) { create(:game, :z80_test_doc) }
    let!(:full_flags) { create(:game, :z80_full_flags) }
    let!(:full) { create(:game, :z80_test_full) }
    let!(:cyrus) { create(:game, :cyrus) }

    let(:z80_game) { Game.find_by!(name: ENV.fetch("Z80_TEST", cyrus.name)) }

    let(:times) { {
      flags.name => 7500,
      regs.name => 13000,
      full_flags.name => 7900,
      full.name => 14000,
    }}

    before do
      visit '/'
    end

    # Disabled some of the IM routines, and now completes.
    # Flags: 019 of 160 tests failed.
    # 052 SRO (XY), R (DD CB 00 00)
    # 074 BIT N,(XY)- DD CB xx 40-47 (undoc?) - same as DD CB 00 46
    # 089 LDIR-> NOP'
    # 090 LDDR ->NOP',
    # 095 IN A, (N)
    # 096 -> 103 IN FE:FF -> BF
    # 107 OUTI
    # 108 OUTD
    # 109 OTIR
    # 110 OTDR
    # 156 LD A,I
    # 157 LD A,R
    #
    # Regs: 026 of 160 tests failed.
    # 1. 52 SRO (XY) ,R (undocumented?) DD CB xx 00
    # 2. 74 BIT N,(XY)- DD CB xx 40
    # 3. 79 SET N, (XY), R       DD CB xx C0
    # 4. 84 RES N, (XY), R       DD CB xx 80
    # 5. 89 LDIR->NOP'
    # 6. 90 LDDR->NOP',
    # 7. 95 IN A, (N)
    # 8. 96 IN FE:FF -> BF
    # 9. 97 IN FE:FF -> BF
    # 10. 98 IN FE:FF -> BF
    # 11. 99 IN FE:FF -> BF
    # 12. 100 IN FE:FF -> BF
    # 13. 101 IN FE:FF -> BF
    # 14. 102 IN FE:FF -> BF
    # 15. 103 IN FE:FF -> BF
    # 16. 105 OUT (C), R
    # 17. 107 OUTI
    # 18. 108 OUTD
    # 19. 109 OTIR
    # 20. 110 OTDR
    # 21. 122 RETN
    # 22. 123 RETI
    # 23. 124 RETI/RETN
    # 24. 154 LD I,A
    # 25. 156 LD A,I
    # 26. 157 LD A,R
    #
    # FullFlags - 026 of 160 tests failed
    # 1. 001 SCF
    # 2. 002 CCF
    # 3. 005 SCF (ST)
    # 4. 006 CCF (ST)
    # 5. 071 BIT N,(HL)
    # 6. 072 BIT N,[ R, (HL)]
    # 7. 073 BIT N, (XY)

    # Full - 033 of 160 tests failed
    # 1. 74 BIT N,(XY)- DD CB xx 40
    # 2. 79 SET N, (XY), R       DD CB xx C0
    # 3. 84 RES N, (XY), R       DD CB xx 80

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

      if z80_game == cyrus
        # Wait for cyrus chess to initialise before
        # sending 'd' for demo mode
        while cpu_count.text.to_i < 300
          sleep 0.5
        end
        # square colours
        cycles = cpu_count.text.to_i
        "a0724".each_char do |k|
          spectrum.send_keys k
        end
        while cpu_count.text.to_i - cycles < 150
          sleep 0.5
        end
        # Level 3 demo mode Cyrus vs Cyrus
        "ld".each_char do |k|
          spectrum.send_keys k
        end
        speed = measure_speed_in_hz
      else
        speed = measure_speed_in_hz do
          spectrum.send_keys 'y'
        end
        while cpu_count.text.to_i < times.fetch(z80_game.name)
          sleep 5
          spectrum.send_keys 'y'
        end
      end

      expect(speed).to be > expected_hz
      puts "Speed #{speed} Hz"
    end
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
      # response to 'scroll?' question if required
      # spectrum.send_keys 'y'
      yield if block_given?
    end
    high
  end

  # def load_tapfile input_url, output_directory
  #   zip_data = []
  #   faraday.get(input_url) do |req|
  #     req.options.on_data = Proc.new do |chunk, size|
  #       zip_data  << chunk
  #     end
  #   end
  #   zipdata = zip_data.join
  #   zipfile = StringIO.new zipdata
  #   Dir.mkdir output_directory
  #   Zip::InputStream.open(zipfile) do |zip_stream|
  #     while (entry = zip_stream.get_next_entry)
  #       entry_filename = entry.name.split("/").last
  #       if entry.name.ends_with?(".tap")
  #         data = entry.get_input_stream.read
  #         File.open("#{output_directory}/#{entry_filename}", "wb+") do |file|
  #           file.write(data)
  #         end
  #       end
  #     end
  #   end
  # end
end