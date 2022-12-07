# Blocking Assignment Algorithm Pseudocode

    BacktrackingSearch(csp):
      return Backtrack(csp, {})
      
    Backtrack(csp, assignment):
      if assignment is complete return assignment
      var = SelectUnassignedVariable(csp, assignment)
      for each value in OrderDomainValues(csp, var, assignment):
        if value is consistent with assignment:
          add {var = value} to assignment
      return failure
      
    SelectUnassignedVariable(csp, assignment):
      return offensive player with fewest number of nearby pass rushers that does NOT have a blocking assignment yet
      
      
    'var' in this function would be the offensive lineman
    OrderDomainValues(csp, var, assignment):
      return list of nearby defenders in order from nearest to farthest   
