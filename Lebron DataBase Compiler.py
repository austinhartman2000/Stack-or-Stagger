# -*- coding: utf-8 -*-
"""
Created on Thu Mar 17 00:23:36 2022

@author: Austin Hartman
"""

import pandas as pd
import numpy as np

stat_types = ['per_game', 'per_poss', 'advanced', 'play-by-play', 'shooting']

overall_data = pd.DataFrame()
rows = 0
columns = 0

for stat_type in stat_types:
    stat_type_data = pd.DataFrame()
    for season in range(2022, 1999,-1):
        season_stats = pd.read_csv("%s_data/%s_%d_dup.csv" % (stat_type, stat_type, season))

        if stat_type == 'per_game':
            #print(len(season_stats))
            rows += len(season_stats) 
        if season == 2022:
            columns += len(season_stats.loc[0])
        if len(stat_type_data) == 0:
            stat_type_data = season_stats.copy()
        else:
            stat_type_data = pd.concat([stat_type_data, season_stats.copy()], sort = False)
    
    
    if len(overall_data) == 0:
        overall_data = stat_type_data.copy()
        #print(overall_data)
        #print('got here')
    elif stat_type == 'per_poss':
        #print(stat_type)
        overall_data = pd.merge(overall_data, stat_type_data, on = "Player_id", suffixes = ["_per_game", "_" + stat_type])
    else:
        #print(stat_type)
        overall_data = pd.merge(overall_data, stat_type_data, on = "Player_id", suffixes = [None, "_" + stat_type])

print(overall_data)
print(rows, columns)

overall_data.to_csv("overall_data_dup.csv")