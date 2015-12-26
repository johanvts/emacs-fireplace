Feature: Putting Emacs on Fire
  In order to do get warm in the winter
  As a cool emacs users
  I want to get some warmth from my favorite editor

  Scenario: Turn on the fire
    When I call "fireplace"
    Then there is a "*fireplace*" buffer


  Scenario: Turn on the fire and then off
    When I call "fireplace"
    And  I call "fireplace-off"
    Then I should be in buffer "*scratch*"

  Scenario: I can customize general setting
    When I try to configure "fireplace"
    Then I should be in buffer "*Customize Group: Fireplace*"
    And I should see "Fireplace Background Char"
    And I should see "Fireplace Buffer Name"
    And I should see "Fireplace Fury"
    And I should see "Fireplace Smoke Char"


  Scenario: I can customize faces
    When I try to configure "fireplace-faces"
    Then I should be in buffer "*Customize Group: Fireplace Faces*"
    And I should see "Fireplace Inner Flame Face"
    And I should see "Fireplace Outter Flame Face"
    And I should see "Fireplace Smoke Face"


  Scenario: Turn on the fire and then off
    When I call "fireplace"
    And  I switch to buffer "*fireplace*"
    And  I call "fireplace-toggle-smoke"
    And  I wait for 1 second
    # Then I should see "*" # see then
