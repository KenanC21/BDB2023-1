# Introduction

This is a quick rundown of the classes, functions, and other things that will be necessary to create the blocking assignments code.

# Classes

The two main classes are the Player and Assignment classes.

## Player

The Player class represents a player (either offensive or defensive) on the field. It has an $x$ and $y$ coordinate as well as the player's ID and an additional identifier to determine whether or not they are on offense.

### Functions

- ```distance_from_player```: Takes a reference from another Player object and returns the Euclidean distance between the two players.

- ```potential_assignments```: Given a list of all defenders on a play, returns an ordered subset of which the player could potentially block in that frame by distance from the defender. A player can block any player within 5 yards of them. Calls the ```distance_from_player``` function.

- ```assign_block```: Passes a Player object and assigns it as this player's blocking assignment. Defenders will be passed a "blocking assignment" as their value for which lineman is blocking them.

### Variables

- ```x```: Player's x coordinate.

- ```y```: Player's y coordinate.

- ```player_id```: Player's unique NFL Player Identifier.

- ```on_offense```: Added identifier for whether or not the player is on offense.

- ```blocking_assignment```: A reference to the Player object to which this player is assigned. Set to ```None``` by default.

## Assignment

Class used to assign offensive lineman to a defender using a backtracking algorithm.

### Functions

- ```assign```: Takes an offensive and defensive player and refers them to one another as a blocking assignment.

- ```remove_assignment```: Called by the ```backtrack``` function. 

- ```backtrack```: Uses backtrack search to assign each pass blocker to a defender. More on backtracking search can be read here:https://github.com/aimacode/aima-java/blob/AIMA3e/notebooks/ConstraintSatisfactionProblems.ipynb

### Variables

- ```off_players```: List of pass blockers in the frame.

- ```def_players```: List of pass rushers in the frame.

- ```num_off_players```: Number of pass blockers in the frame.

- ```num_def_players```: Number of pass rushers in the frame.

- ```frame_id```: Unique frame identifier within a play.

- ```play_id```: Unique play identifier within a game.

- ```game_id```: Unique game identifier.

# Algorithm

From the blocking_assignment_algorithm_pseudocode.md:

    Backtrack(assignment, off_players, def_players):
      if all players in off_players have a blocking assignment, return assignment
      lineman = next offensive lineman, ordered by y value
      for each defender in potential_assignments(lineman) do
        if the defender is not already blocked then:
          assign(lineman, defender)
          result = Backtrack(assignment, off_players, def_players)
          if result is not failure:
            return result
      return failure
      
    potential_assignments(player):
      return an ordered list of potential defenders for a lineman to block by distance
      
    assign(offensive_player, defensive_player):
      assign the two players as blocking/being blocked by one another 
