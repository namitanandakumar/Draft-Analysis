from __future__ import division
import pandas as pd

d = pd.read_csv('~/Downloads/nhlplayers.csv')
names = d['Team'].unique().tolist()

results = []

for i in names:
    d1 = d[d['Team'] == i]
    d1 = d1[d1['Pos'] != 'G']
    d1 = d1.sort_values(by='TOI', ascending=False).reset_index(drop=True)[:21]
    d1 = d1.sort_values(by='Goals', ascending=True).reset_index(drop=True)
    d1['CumPlayer'] = (d1.index+1)/len(d1)
    d1 = d1.append(pd.DataFrame([[i,'NA',0,0,0,0,'NA',0,0]], columns=d1.columns))
    d1 = d1.sort_values(by='CumPlayer', ascending=True).reset_index(drop=True)
    d1['CumGoals'] = (d1['Goals'].cumsum())/sum(d1['Goals'])
    d1['Area'] = ((d1['CumGoals']+d1['CumGoals'].shift())/2)*d1.iloc[1]['CumPlayer']
    d1 = d1.fillna(value=0)
    area = 1-2*sum(d1['Area'])
    results.append(pd.Series(area))
results = pd.concat(results, axis=0).reset_index(drop=True)
results = pd.concat([pd.Series(names),results],axis=1,join_axes=[results.index])

d2 = pd.DataFrame({'Team': results[0], 'Gini': results[1]})
print d2
