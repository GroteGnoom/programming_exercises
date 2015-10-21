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
    
def playGame(game, competition, prediction, points):
    competition[game[0]]['for'] = competition[game[0]]['for'] + game[2]
    competition[game[1]]['for'] = competition[game[1]]['for'] + game[3]
    competition[game[0]]['against'] = competition[game[0]]['against'] + game[3]
    competition[game[1]]['against'] = competition[game[1]]['against'] + game[2]
    competition[game[0]]['played'] = competition[game[0]]['played'] + 1
    competition[game[1]]['played'] = competition[game[1]]['played'] + 1

    if (prediction[0] == game[2]) and (prediction[1] == game[2]): points = points + 10
    else :
        if prediction[0] == game[1]: points = points + 2
        if prediction[1] == game[2]: points = points + 2
        if (prediction[0] < prediction[1]) and (game[2] < game[3]): points = points + 5
        if (prediction[0] > prediction[1]) and (game[2] > game[3]): points = points + 5
        if (prediction[0] == prediction[1]) and (game[2] == game[3]): points = points + 7
      
    if game[2]>game[3]:
        competition[game[0]]['won'] = competition[game[0]]['won'] + 1
        competition[game[0]]['points'] = competition[game[0]]['points'] + 3
        competition[game[1]]['lost'] = competition[game[1]]['lost'] + 1
    if game[3]>game[2]:
        competition[game[1]]['won'] = competition[game[1]]['won'] + 1
        competition[game[1]]['points'] = competition[game[1]]['points'] + 3
        competition[game[0]]['lost'] = competition[game[0]]['lost'] + 1
    if game[3]==game[2]:
        competition[game[0]]['draw'] = competition[game[0]]['draw'] + 1
        competition[game[1]]['draw'] = competition[game[1]]['draw'] + 1
        competition[game[0]]['points'] = competition[game[0]]['points'] + 1
        competition[game[1]]['points'] = competition[game[1]]['points'] + 1
    return (competition, points)
    
def run (prediction):
    clubs, games = init()
    competition = startCompetition(clubs)
    points = 0
    for game in games:
        competition, points = playGame(game, competition, prediction, points)
    print competition, points

run([0,0])
run([1,0])
run([0,1])
run([1,1])





