# Blocking Assignment Algorithm Pseudocode

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
