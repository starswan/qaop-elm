require "zip"

class GamesController < ApplicationController
  # GET /games
  # GET /games.xml
  def index
    @games = Game.all

    # respond_to do |format|
    #   format.html # index.html.erb
    #   format.xml  { render :xml => @games }
    # end
  end

  # GET /games/1
  # GET /games/1.xml
  def show
    @game = Game.find(params[:id])
  end

  def download
    game = Game.find(params[:id])
    # @file = Rails.cache.fetch("games_#{@game.id}") do

    input_url = game.download_url

    download_data = Rails.cache.fetch("games_#{game.id}") do
      zip_data = []
      logger.debug "Fetching #{input_url}"
      faraday.get(input_url) do |req|
        req.options.on_data = Proc.new do |chunk, size|
          zip_data  << chunk
        end
      end
      zipdata = zip_data.join
      zipfile = StringIO.new zipdata

      data = nil
      Zip::InputStream.open(zipfile) do |zip_stream|
        while (entry = zip_stream.get_next_entry)
          entry_filename = entry.name.split("/").last
          # if entry.name.ends_with?(".tap")
          if entry_filename == game.filename
            data = entry.get_input_stream.read
            break
            # File.open("#{output_directory}/#{entry_filename}", "wb+") do |file|
            #   file.write(data)
            # end
          end
        end
      end
      data
    end
    # download_data = data

    # zipfile = faraday.get @game.download_url
    # zip_stream = Zip::InputStream.new zipfile

    # file = nil
    # while entry = zip_stream.get_next_entry
    #   # All required operations on `entry` go here.
    #   if entry.filepath.split("/").last == @game.filename
    #     file = entry
    #     break
    #   end
    # end
    # send_data file.get_input_stream.read
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
