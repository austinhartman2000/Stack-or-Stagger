# -*- coding: utf-8 -*-
"""
Created on Thu Jun  2 13:07:17 2022

@author: Austin Hartman
"""

import pandas as pd
import re

def fix_team (team):
    if team == 'TOT' or len(team) > 3:
        return "-"
    else:
        return team

def fix_year (year):
    year = year[0:4]
    year  = int(year)+1
    return year

def fix_name (name):
    name = re.sub(r'[*.-]+', '', name)
    name = re.sub(r'[\']+', '', name)
    return name

data = pd.read_csv("lebron_data.csv")
data['TEAM'] = data['TEAM'].apply(fix_team)
data['SEASON'] = data['SEASON'].apply(fix_year)
data['PLAYER'] = data['PLAYER'].apply(fix_name)
data = data[data['TEAM'] != '-']
data = data.sort_values(by = ['WINS ADDED'], ascending = False)

print(data)
print(len(data))

bref_data = pd.read_csv("overall_data_dup.csv")
bref_data['Player_per_game'] = bref_data['Player_per_game'].apply(fix_name)

data = pd.merge(data, bref_data, how = "left", left_on = ["PLAYER", "SEASON", "TEAM"], right_on = ['Player_per_game', 'Year_per_game', 'Tm_per_game'])

print(len(data))

teams = data['TEAM'].unique()

new_subset = pd.DataFrame(columns = ['Team', 'Season', 'Player1', 'Player2', "Arch1", "Arch2", "Lebron1", "Lebron2", "WinsAdded1", "WinsAdded2", "MPG1", "MPG2", "PM0", "PM1", "PM2", "PM3"])

for year in range(2009,2023):
    for team in teams:
        year_str = str(year) + "-" + str(year+1-2000)
        subset = data[(data['SEASON'] == year) & (data['TEAM'] == team)].iloc[0:2]
        #print(subset)
        if len(subset) > 0:
            sub_dict = {
                'Team' : subset.iloc[0,2],
                'Season' : subset.iloc[0,1],
                'Player1' : subset.iloc[0,0],
                'Player2' : subset.iloc[1,0],
                'Arch1' : subset.iloc[0,3],
                'Arch2' : subset.iloc[1,3],
                'Lebron1' : subset.iloc[0,7],
                'Lebron2' : subset.iloc[1,7],
                'WinsAdded1' : subset.iloc[0,10],
                'WinsAdded2' : subset.iloc[1,10],
                'MPG1' : subset.iloc[0,21],
                'MPG2' : subset.iloc[1,21]                
            }
            new_subset = new_subset.append(sub_dict, ignore_index = True)

new_subset = new_subset.sort_values(by = ['Season', 'Team'])
new_subset.to_csv("StackOrStaggerData.csv", index = False)