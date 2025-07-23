require "zip"

class GamesController < ApplicationController
  # GET /games
  # GET /games.xml
  def index
    @games = Game.all

    # respond_to do |format|
    #   format.html # index.html.erb
    #   format.xml  { render :xml => @games }
    # endfile
  end

  # GET /games/1
  # GET /games/1.xml
  def show
    @game = Game.find(params[:id])
  end

  def download
    game = Game.find(params[:id])
    input_url = game.download_url

    download_data = Rails.cache.fetch("games_#{game.id}_#{game.filename}") do
      logger.debug "Fetching #{input_url}"
      # zip_chunks = []
      # faraday.get(input_url) do |req|
      #   req.options.on_data = Proc.new do |chunk, size|
      #     zip_chunks << chunk
      #   end
      # end
      # logger.debug "Received #{zip_chunks.size} chunks"
      # zipdata = zip_chunks.join
      # This seems much easier, and seems to work more reliably.
      zipdata = faraday.get(input_url).body
      zipfile = StringIO.new zipdata
      logger.debug "Data size #{zipdata.size}"

      data = nil
      Zip::InputStream.open(zipfile) do |zip_stream|
        while (entry = zip_stream.get_next_entry)
          logger.debug "Entry #{entry.name}"
          entry_filename = entry.name.split("/").last
          if entry_filename == game.filename
            data = entry.get_input_stream.read
            break
          end
        end
      end
      data
    end

    send_data download_data
  end

  private

  def faraday
    Faraday.new do |f|
      f.response :raise_error
      f.response :follow_redirects
      f.adapter Faraday.default_adapter
    end
  end
end
