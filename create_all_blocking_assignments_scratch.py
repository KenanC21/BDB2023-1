import pandas as pd
import blocking_assignments as ba
import time

week1 = pd.read_csv('week1.csv')
pff = pd.read_csv('pffScoutingData.csv')
plays = pd.read_csv("plays.csv")

week1_pff_roles = pd.merge(week1, pff, how = "inner", on = ['gameId', 'playId', 'nflId'])
week1_pff_roles = pd.merge(week1_pff_roles, plays, how = "inner", on = ['gameId', 'playId'])
print(week1_pff_roles.head)

pass_block_rush = week1_pff_roles.loc[week1_pff_roles['pff_role'].isin(['Pass Block', 'Pass Rush'])]

print(pass_block_rush.head)


frames = pass_block_rush[['gameId', 'playId', 'frameId']].drop_duplicates()

frames = frames[0:1]
results = pd.DataFrame()
start_time = time.time()
for index, iter_frame in frames.iterrows():
    game = iter_frame.gameId
    play = iter_frame.playId
    frame = iter_frame.frameId

    temp = pass_block_rush.loc[(pass_block_rush['gameId'] == game) & ( pass_block_rush['playId'] == play) & (pass_block_rush['frameId'] == frame)]
    all_players = []
    for index, iter_player in temp.iterrows():
        print(game, " ", play, " ", frame, " ", iter_player.x, " ", iter_player.y, " ", iter_player.nflId, " ", iter_player.team == iter_player.possessionTeam)
        temp_player = ba.Player(iter_player.nflId, iter_player.x, iter_player.y, iter_player.team == iter_player.possessionTeam)
        all_players.append(temp_player)

    off_players = []
    def_players = []
    for p in all_players:
        if p.on_offense:
            off_players.append(p)
        else:
            def_players.append(p)
    
    assignment = ba.Assignment(off_players, def_players, frame, play, game)
    test_backtrack = assignment.backtrack()

    print(len(test_backtrack))

    #for lineman in test_backtrack.off_players:
    #    lineman_nflId = lineman.player_id
    #    if lineman.blocking_assignment is None:
    #        defender_nflId = None
    #    else:
    #        defender_nflId = lineman.blocking_assignment.player_id
    #    results = pd.concat([results, pd.DataFrame(data = [[game, play, frame, lineman_nflId, defender_nflId]], columns = ['gameId', 'playId', 'frameId', 'lineman_nflId', 'defender_nflId'])])


print(results)
end_time = time.time()

print(end_time - start_time)