ActiveAdmin.register Game do
  index do
    selectable_column
    column :name
    column :filename
    column :picture_url
    column :download_url
    column :filetype
    column :directory
    actions
  end

  # See permitted parameters documentation:
  # https://github.com/activeadmin/activeadmin/blob/master/docs/2-resource-customization.md#setting-up-strong-parameters
  #
  # Uncomment all parameters which should be permitted for assignment
  #
  permit_params :name, :filename, :picture_url, :download_url, :filetype, :directory
  #
  # or
  #
  # permit_params do
  #   permitted = [:name, :filename, :picture_url, :download_url, :filetype, :directory]
  #   permitted << :other if params[:action] == 'create' && current_user.admin?
  #   permitted
  # end
end
