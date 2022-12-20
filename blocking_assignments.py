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


class Assignment:

    def __init__(self, off_players, def_players, frame_id, play_id, game_id):
        # TODO
        self.off_players = off_players
        self.def_players = def_players
        self.num_off_players = len(off_players)
        self.num_def_players = len(def_players)
        self.frame_id = frame_id
        self.play_id = play_id
        self.game_id = game_id
        self.all_possible_blocking_assignments = []
        self.backtracked = False

    def assign(self, off_player, def_player):
        # TODO
        off_player.assign_block(def_player)
        def_player.assign_block(off_player)

    def remove_assignment(self, off_player):
        # TODO - unsure if we'll need this or not? 
        off_player.blocking_assignment.blocking_assignment = None
        off_player.blocking_assignment = None


    def backtrack(self):
        # TODO
        unassigned_players = 0
        for ol in self.off_players: # first check if all players have an assignment
            if (ol.blocking_assignment is None):
                unassigned_players = unassigned_players + 1
        if unassigned_players == 0:
            # instead add the assignment to the list
            self.all_possible_blocking_assignments.append(self)
        for lineman in self.off_players:
            if lineman.blocking_assignment is None:
                potential_assignments = lineman.potential_assignments(self.def_players)
                for defender in potential_assignments:
                    if defender.blocking_assignment is None:
                        self.assign(lineman, defender)
                        result = self.backtrack()
                        for result_assignment in result:
                            self.all_possible_blocking_assignments.append(result_assignment) # this is causing an infinite loop for some reason, I have a feeling it's a deep vs shallow copy issue?
                        self.remove_assignment(lineman)
        self.backtracked = True
        # add assignment to the list right here
        self.all_possible_blocking_assignments.append(self)
        return self.all_possible_blocking_assignments

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
        return Assignment(self.off_players, self.def_players, self.frame_id, self.play_id, self.game_id)


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