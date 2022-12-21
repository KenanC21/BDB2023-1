import numpy as np

# How do we structure this problem? Make it more like a game theory problem
# Create classes for "states" (a single frame with blocking assignments) and "actions" (assigning an OL to a pass rusher)

class Player:
    """ create a player object, this is to speed up the 
    """
    def __init__(self, player_id, x, y, on_offense):
        # TODO
        self.player_id = player_id
        self.x = x
        self.y = y
        self.on_offense = on_offense
        self.blocking_assignment = None
        self.visited = False

    def distance_from_player(self, player):
        return np.sqrt((self.x - player.x) ** 2 + (self.y - player.y) ** 2)

    def potential_assignments(self, opponents):
        assignments = []
        dist = []
        for opp in opponents:
            if (self.distance_from_player(opp) <= 5):
                assignments.append(opp)
                dist.append(self.distance_from_player(opp))
        ordered_assignments = [x for _, x in sorted(zip(dist, assignments))] # orders the blocking assignments based on distance to this player
        return ordered_assignments

    def assign_block(self, player):
        self.blocking_assignment = player
    
    def visit(self):
        self.visited = True

    def copy(self):
        new_player = Player(self.player_id, self.x, self.y, self.on_offense)
        new_player.visited = self.visited
        return new_player



class Assignment:

    def __init__(self, off_players, def_players, frame_id, play_id, game_id):
        self.off_players = off_players
        self.def_players = def_players
        self.num_off_players = len(off_players)
        self.num_def_players = len(def_players)
        self.frame_id = frame_id
        self.play_id = play_id
        self.game_id = game_id
        self.backtracked = False

    def assign(self, off_player, def_player):
        off_player.assign_block(def_player)
        def_player.assign_block(off_player)

    def remove_assignment(self, off_player):
        off_player.blocking_assignment.blocking_assignment = None
        off_player.blocking_assignment = None

    # TODO - this is "good enough" to return a sufficient answer but it could be better, just need to eliminate the double counting. not sure where it's coming from
    def backtrack(self):
        # TODO
        this_assignment = self.copy()
        unvisited_players = 0
        all_possible_blocking_assignments = []
        for ol in this_assignment.off_players: # first check if all players have an assignment
            if (not ol.visited):
                unvisited_players = unvisited_players + 1
        #if unvisited_players == 0: # Not sure if this part is necessary, it could be causing the double counting
            # instead add the assignment to the list
        #    all_possible_blocking_assignments.append(this_assignment)
        for lineman in this_assignment.off_players:
            if not lineman.visited:
                lineman.visit()
                potential_assignments = lineman.potential_assignments(this_assignment.def_players)
                for defender in potential_assignments:
                    if defender.blocking_assignment is None:
                        this_assignment.assign(lineman, defender)
                        result = this_assignment.backtrack()
                        for result_assignment in result:
                            all_possible_blocking_assignments.append(result_assignment) 
                        this_assignment.remove_assignment(lineman)
        self.backtracked = True
        this_assignment.backtracked = True
        # add assignment to the list right here
        all_possible_blocking_assignments.append(this_assignment)
        return all_possible_blocking_assignments

    # https://github.com/aimacode/aima-java/blob/AIMA3e/notebooks/ConstraintSatisfactionProblems.ipynb
    # ^ helpful resource with CSP stuff


    def unblocked_defenders(self):
        if self.backtracked:
            num_unblocked = 0
            for defender in self.def_players:
                if defender.blocking_assignment is None:
                    num_unblocked += 1
            return num_unblocked
        else:
            return None

    def unassigned_linemen(self):
        if self.backtracked:
            # TODO
            num_unassigned = 0
            for lineman in self.off_players:
                if lineman.blocking_assignment is None:
                    num_unassigned += 1
            return num_unassigned
        else:
            return None

    def cumulative_blocking_distance(self):
        if self.backtracked:
            distance = 0
            for lineman in self.off_players:
                if lineman.blocking_assignment is not None:
                    distance += lineman.distance_from_player(lineman.blocking_assignment)
            return distance
        else:
            return None
        
    def copy(self):
        copy_off_players = []
        copy_def_players = []

        for o in self.off_players:
            copy = o.copy()
            if o.blocking_assignment is not None:
                defender_copy = o.blocking_assignment.copy()
                copy.assign_block(defender_copy)
                defender_copy.assign_block(copy)
            copy_off_players.append(copy)

        for d in self.def_players:
            copy = d.copy()
            if d.blocking_assignment is not None:
                blocker_copy = d.blocking_assignment.copy()
                copy.assign_block(blocker_copy)
                blocker_copy.assign_block(copy)
            copy_def_players.append(copy)
        return Assignment(copy_off_players, copy_def_players, self.frame_id, self.play_id, self.game_id)


# Just for testing purposes. Can be removed later if not needed
test = Player(12031, 35.12, 30.23, True)
print(test.player_id)
print(test.x)
print(test.y)
print(test.on_offense)
print(test.blocking_assignment)


def1 = Player(12503, 28.14, 28.45, False)
def2 = Player(12504, 30, 28.45, False)
def3 = Player(12505, 35.89, 28.45, False)
def4 = Player(12506, 40, 30.21, False)
def5 = Player(12507, 36.54, 29.78, False)


print(test.distance_from_player(def1))
print(test.distance_from_player(def2))
print(test.distance_from_player(def3))
print(test.distance_from_player(def4))
print(test.distance_from_player(def5))

assignments = test.potential_assignments([def1, def2, def3, def4, def5])

for a in assignments:
    print(a.player_id)