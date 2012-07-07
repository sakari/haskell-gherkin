@tag1 @tag2
Feature: Feature name

  Description for the feature
  second line of description

  @tag @second
  Scenario: simple scenario
    Given foo is bar
    And all is <well>

  Scenario: scenario with block args
    Given bar
    | aa | bar |
    | value | ee |
    And foo also
    """
    laoeuaoeueua
    """

  @taggi
  Scenario-outline: outline
    Given foo <bar>
    | aa     | bee    |
    | value1 | value 2|
    Examples:
    | aaa | aouea |
    | aoe | aoe   |
