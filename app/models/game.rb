class Game < ApplicationRecord
  class << self
    def ransackable_attributes(_auth_object = nil)
      [:name, :filename, :picture_url, :download_url, :filetype, :directory]
    end
  end
end
