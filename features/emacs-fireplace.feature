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
