import pandas as pd

### Download the data.
d = pd.read_csv('~/Downloads/nhlplayers.csv')
### Get a list of team names.
names = d['Team'].unique().tolist()

results = []

### For each team, calculate the Gini coefficient.
for i in names:
    ### Subset on that team.
    d1 = d[d['Team'] == i]
    ### Take out goalies.
    d1 = d1[d1['Pos'] != 'G']
    ### Sort by TOI to find the top 21 skaters.
    d1 = d1.sort_values(by='TOI', ascending=False).reset_index(drop=True)[:21]
    ### Sort from least to most goals scored.
    d1 = d1.sort_values(by='Goals', ascending=True).reset_index(drop=True)
    ### Use the index (which goes from 0 to 20) to get the cumulative player percentile.
    d1['CumPlayer'] = (d1.index+1)/len(d1)
    ### Create a zero row so that the Lorenz curve starts at 0.
    d1 = d1.append(pd.DataFrame([[i,'NA',0,0,0,0,'NA',0,0]], columns=d1.columns))
    ### Re-sort the data to make the zero row first.
    d1 = d1.sort_values(by='CumPlayer', ascending=True).reset_index(drop=True)
    ### Calculate cumulative goals percentile.
    d1['CumGoals'] = (d1['Goals'].cumsum())/sum(d1['Goals'])
    ### Calculate area under the curve using some basic calculus.
    d1['Area'] = ((d1['CumGoals']+d1['CumGoals'].shift())/2)*d1.iloc[1]['CumPlayer']
    d1 = d1.fillna(value=0)
    ### Calculate the team's Gini coefficient.
    area = 1-2*sum(d1['Area'])
    ### Keep a running tally of your results.
    results.append(pd.Series(area))
### Create a dataframe with team names and corresponding coefficients.
results = pd.concat(results, axis=0).reset_index(drop=True)
results = pd.concat([pd.Series(names),results],axis=1,join_axes=[results.index])
### Make sure your columns are named and everything is clear!
d2 = pd.DataFrame({'Team': results[0], 'Gini': results[1]})
print(d2)
### Check your results and see if they match across all programs.
