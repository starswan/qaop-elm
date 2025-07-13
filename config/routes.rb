Rails.application.routes.draw do
  ActiveAdmin.routes(self)
  root 'games#index'

  resources :games, only: [:index, :show] do
    member do
      get :download
    end
  end
end
