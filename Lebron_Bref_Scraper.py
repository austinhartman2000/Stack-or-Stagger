# -*- coding: utf-8 -*-
"""
Created on Wed Mar  9 23:22:57 2022

@author: Austin Hartman
"""

# needed libraries
from urllib.request import urlopen
from bs4 import BeautifulSoup
import pandas as pd
import unidecode
import re
import numpy as np

def no_accents (name):
    unaccented_string = unidecode.unidecode(name)
    return (unaccented_string)

def make_code (name, year, team):
    name = name.split()
    #name[0] = name[0][0]
    name = name[0:2]
    name = ''.join(name)
    res = re.sub(r'[\W]', '', name)
    res = res + str(year) + str(team)
    return res.lower()
    

stat_types = ['per_game', 'per_poss', 'advanced']

for stat_type in stat_types:
    for season in range(2022,1999,-1):
        # URL to scrape
        url = "https://www.basketball-reference.com/leagues/NBA_%d_%s.html" % (season, stat_type)
        
        # collect HTML data
        html = urlopen(url)
                
        # create beautiful soup object from HTML
        soup = BeautifulSoup(html, features="lxml")
        
        # use getText()to extract the headers into a list
        headers = [th.getText() for th in soup.findAll('tr', limit=2)[0].findAll('th')]
        
        headers.pop(0)
        
        # get rows from table
        rows = soup.findAll('tr')[1:]
        rows_data = [[td.getText() for td in rows[i].findAll('td')]
                            for i in range(len(rows))]
        # if you print row_data here you'll see an empty row
        # so, remove the empty row
        #print(rows_data)
        #rows_data.pop(20)
        
        print(str(season) + " " + stat_type)
            
        # create the dataframe
        single_year = pd.DataFrame(rows_data, columns = headers)
        
        single_year = single_year.dropna()
        #single_year = single_year.drop_duplicates(subset=['Player'], keep = "first")
        
        single_year = single_year.replace(r'^\s*$', np.nan, regex=True)
        single_year = single_year.dropna(axis = 1, how = 'all')
        single_year = single_year.fillna(0)
        
        single_year['Year'] = str(season)
        
        single_year['Player'] = single_year['Player'].apply(no_accents)
        single_year['Player_id'] = single_year.apply(lambda x: make_code(x['Player'], x['Year'], x['Tm']), axis = 1)
        
        #single_year = single_year.drop_duplicates(subset = ['Player_id', 'Tm'], keep = False)
        
        #drop_rows = range(20,len(nba_finals),20)
        #nba_finals.drop(drop_rows, 0, inplace=True)
        
        # export dataframe to a CSV 
        single_year.to_csv("%s_data/%s_%d_dup.csv" % (stat_type, stat_type, season), index=False)
        
stat_types = ['play-by-play', 'shooting']

for stat_type in stat_types:
    for season in range(2022,1999,-1):
        # URL to scrape
        url = "https://www.basketball-reference.com/leagues/NBA_%d_%s.html" % (season, stat_type)
        
        # collect HTML data
        html = urlopen(url)
                
        # create beautiful soup object from HTML
        soup = BeautifulSoup(html, features="lxml")
        
        # use getText()to extract the headers into a list
        headers = [th.getText() for th in soup.findAll('tr', limit=2)[1].findAll('th')]
        
        headers.pop(0)
        
        # get rows from table
        rows = soup.findAll('tr')[2:]
        rows_data = [[td.getText() for td in rows[i].findAll('td')]
                            for i in range(len(rows))]
        # if you print row_data here you'll see an empty row
        # so, remove the empty row
        #print(rows_data)
        #rows_data.pop(20)
        
        print(str(season) + " " + stat_type)
            
        # create the dataframe
        single_year = pd.DataFrame(rows_data, columns = headers)
        
        single_year = single_year.dropna()
        #single_year = single_year.drop_duplicates(subset=['Player'], keep = "first")
        
        single_year = single_year.replace(r'^\s*$', np.nan, regex=True)
        single_year = single_year.dropna(axis = 1, how = 'all')
        single_year = single_year.fillna(0)
        
        single_year['Year'] = str(season)
        
        single_year['Player'] = single_year['Player'].apply(no_accents)
        single_year['Player_id'] = single_year.apply(lambda x: make_code(x['Player'], x['Year'], x['Tm']), axis = 1)
        
        #drop_rows = range(20,len(nba_finals),20)
        #nba_finals.drop(drop_rows, 0, inplace=True)
        
        #single_year = single_year.drop_duplicates(subset = ['Player_id', 'Tm'], keep = False)
        
        
        # export dataframe to a CSV 
        single_year.to_csv("%s_data/%s_%d_dup.csv" % (stat_type, stat_type, season), index=False)