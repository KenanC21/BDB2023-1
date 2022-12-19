def backtrack(self):
        # TODO
        unassigned_players = 0
        for ol in self.off_players: # first check if all players have an assignment
            if (ol.blocking_assignment is None):
                unassigned_players = unassigned_players + 1
        if unassigned_players == 0:
            return self
        for lineman in self.off_players:
            lineman_nflId = lineman.player_id
            if lineman.blocking_assignment is None:
                defender_nflId = None
                potential_assignments = lineman.potential_assignments(self.def_players)
                for defender in potential_assignments:
                    if defender.blocking_assignment is None:
                        self.assign(lineman, defender)
                        result = self.backtrack()
                        #self.remove_assignment(lineman)
        else:
                defender_nflId = lineman.blocking_assignment.player_id
                results = pd.concat([results, pd.DataFrame(data = [[game, play, frame, lineman_nflId, defender_nflId]], columns = ['gameId', 'playId', 'frameId', 'lineman_nflId', 'defender_nflId'])])

        return results
