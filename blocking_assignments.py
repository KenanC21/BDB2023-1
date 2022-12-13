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
        # TODO
        return np.sqrt((self.x - player.x) ** 2 + (self.y - player.y) ** 2)

    def potential_assignments(self, opponents):
        # TODO
        return None

    def assign_block(self, player):
        self.blocking_assignment = player

    


class Play:

    def __init__(self, off_players, def_players):
        # TODO
        self.off_players = off_players
        self.def_players = def_players

# testing
test = Player(12031, 35.12, 30.23, True)
print(test.player_id)
print(test.x)
print(test.y)
print(test.on_offense)
print(test.blocking_assignment)

test2 = Player(12503, 28.14, 28.45, False)
print(test.distance_from_player(test2))

test.assign_block(test2)
print(test.blocking_assignment.player_id)


