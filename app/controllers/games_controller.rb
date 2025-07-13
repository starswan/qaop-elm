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
    @game = Game.find(params[:id])
    # @file = Rails.cache.fetch("games_#{@game.id}") do
    zipfile = Faraday.get @game.download_url
    zip_stream = Zip::InputStream.new zipfile

    file = nil
    while entry = zip_stream.get_next_entry
      # All required operations on `entry` go here.
      if entry.filepath.split("/").last == @game.filename
        file = entry
        break
      end
    end
    send_data file.get_input_stream.read
  end
end
