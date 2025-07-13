class AddRemoteUrlsToGames < ActiveRecord::Migration[7.1]
  def change
    change_table :games do |t|
      t.string :picture_url
      t.string :download_url
      t.string :filetype
    end
    rename_column :games, :tapfile, :filename
  end
end
