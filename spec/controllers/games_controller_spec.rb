require 'rails_helper'

RSpec.describe GamesController, type: :controller do
  before do
    create :game, :z80_test_doc
    create :game, :z80_test_flags
  end
  let(:game_one) { Game.first }

  it "should get index" do
    get :index
    assert_response :success
    expect(assigns(:games)).not_to be_nil
  end

  it "should show game" do
    get :show, params: { :id => game_one.to_param }
    assert_response :success
  end
end
