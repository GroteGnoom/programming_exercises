import re

def init ():
    results_file = open('results2014-2015.htm', 'r')
    results_string = results_file.read()
    clubs = list(set(re.findall('(?<=Wedstrijdverloop: )[\w ]+(?= -)', results_string)))
    games = re.findall('(?<=Wedstrijdverloop: ).*?(?=</a>)', results_string)
    games = map(mySplit, games)[6:]
    return (clubs, games)

def mySplit(game):
    single_split_game = re.split('">', game)
    first_split = re.split(' - ', single_split_game[0])
    second_split = re.split(' - ', single_split_game[1])
    second_split = [int(second_split[0]), int(second_split[1])]
    first_split.extend(second_split)
    return first_split

def startCompetition(clubs):
    competition = {}
    for club in clubs:
        competition[club] = {'won': 0, 'lost': 0, 'draw': 0, 'points': 0, 'for': 0, 'against': 0, 'played': 0}
    return competition
    
  
def playGame(game, competition):
    team1 = competition[game[0]]
    team2 = competition[game[1]]
    score_team1 = game[2]
    score_team2 = game[3]
    team1['for'] = team1['for'] + score_team1
    team2['for'] = team2['for'] + score_team2
    team1['against'] = team1['against'] + score_team2
    team2['against'] = team2['against'] + score_team1
    team1['played'] = team1['played'] + 1
    team2['played'] = team2['played'] + 1
    if score_team1>score_team2:
        team1['won'] = team1['won'] + 1
        team1['points'] = team1['points'] + 3
        team2['lost'] = team2['lost'] + 1
    if score_team2>score_team1:
        team2['won'] = team2['won'] + 1
        team2['points'] = team2['points'] + 3
        team1['lost'] = team1['lost'] + 1
    if score_team2==score_team1:
        team1['draw'] = team1['draw'] + 1
        team2['draw'] = team2['draw'] + 1
        team1['points'] = team1['points'] + 1
        team2['points'] = team2['points'] + 1
    return (competition)

def update_points(game, prediction, points):
    if (prediction[0] == game[2]) and (prediction[1] == game[2]): points = points + 10
    else :
        if prediction[0] == game[1]: points = points + 2
        if prediction[1] == game[2]: points = points + 2
        if (prediction[0] < prediction[1]) and (game[2] < game[3]): points = points + 5
        if (prediction[0] > prediction[1]) and (game[2] > game[3]): points = points + 5
        if (prediction[0] == prediction[1]) and (game[2] == game[3]): points = points + 7
    return points
    
def make_prediction(game, competition, cutoff):
    team1 = competition[game[0]]
    team2 = competition[game[1]]
    # try team1['points']/team1['played'] - team2['points']/team2['played']
    if (team1['points'] - team2['points']) > cutoff:
        #print game[0], team1, game[1], team2, [1,0], game[2], game[3]
        return [1,0]
    if (team1['points'] - team2['points']) < -cutoff:
        return [0,1]
    return [1,1]
    
    
def run (cutoff):
    clubs, games = init()
    competition = startCompetition(clubs)
    points = 0
    for game in reversed(games):
        prediction = make_prediction(game, competition,cutoff)
        competition = playGame(game, competition)
        points = update_points(game, prediction, points)
    print cutoff, points
    #print competition
    
def count_scores(games, score):
    count = 0
    for game in games:
        if (game[2] == score[0]) and (game[3] == score[1]):
            count = count + 1
    return count
    
"""
clubs, games = init()
print count_scores(games, [0,0]) # 16
print count_scores(games, [1,1]) # 34
print count_scores(games, [1,0]) # 23
print count_scores(games, [0,1]) # 18
print count_scores(games, [2,1]) # 22
print count_scores(games, [1,2]) # 14
print count_scores(games, [2,0]) # 25
print count_scores(games, [0,2]) # 11
"""

for cutoff in range(0, 50):
    run(cutoff)

# 306 matches in total

# 1243 for all [1,1]
# 1282 for cutoff at 12 with these predictions:    and playing competition reversed
# 1313 for cutoff at 24 - playing competition in right order, cutoff at 12 would give 1236
"""
def make_prediction(game, competition, cutoff):
    team1 = competition[game[0]]
    team2 = competition[game[1]]
    if (team1['points'] - team2['points']) > cutoff:
        #print game[0], team1, game[1], team2, [1,0], game[2], game[3]
        return [1,0]
    if (team1['points'] - team2['points']) < -cutoff:
        return [0,1]
    return [1,1]
"""




