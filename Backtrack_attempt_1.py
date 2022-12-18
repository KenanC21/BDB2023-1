def Backtrack(self, assignment, off_players, def_players):
    if off_players.blocking_assignment != None: 
        return assignment
    else:
        for def_players.player_id in off_players.potential_assignment:
            if off_players.assignment == None: 
                assign(off_players,def_players)
                result = Backtrack(assignment, off_players, def_players)
                if result is not False:
                    return result
    return False 
