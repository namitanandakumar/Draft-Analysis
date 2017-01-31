import pandas as pd

desired_width = 200
pd.set_option('display.width', desired_width)

stats = pd.read_csv('~/Downloads/00stats.csv')
stats = stats.sort_values(by='PS', ascending=False)
stats1 = stats[['Overall','Team']]
stats1 = stats1.sort_values(by='Team', ascending=True)

stats1.sort_values(by='Team', inplace=True)
stats1.set_index(keys=['Team'], drop=False, inplace=True)
names = stats1['Team'].unique().tolist()

results = []

for i in names:
    team = i
    df = stats1.loc[stats1.Team == i]
    df = df.sort_values(by='Overall', ascending=False)
    df1 = df['Overall']
    df1 = list(df1)
    stats2 = stats
    for i in df1:
        stats2 = stats2[['Overall', 'Team', 'Player', 'PS']]
        stats2.loc[stats2['Overall'] >= i, 'Rank'] = range(len(stats2) - i + 1)
        stats2['Rank'] = stats2['Rank'] + 1
        stats2.Rank = stats2.Rank.fillna(value=0)
        stats2 = stats2[stats2.Rank != 1]
    result = stats[~stats.isin(stats2)].dropna()
    result = result[['Team', 'Player', 'PS', 'Overall']]
    result = result.sort_values(by='Overall', ascending=True)
    result = result.reset_index()
    result = result.drop('index', 1)
    df = df.sort_values(by='Overall',ascending=True)
    df = df['Overall'].reset_index()
    result['Pick'] = df['Overall']
    result['Team'] = team
    result['PPS'] = result['PS'].sum()
    results.append(result)
results = pd.concat(results, axis=0)

actual = stats[['Overall','Player','PS']]
actual.columns = ['Pick', 'Actual','APS']
total = stats[['Team','PS']]
total.columns.values[1] = 'TPS'
total = total.groupby(['Team']).sum()
total['Team'] = total.index

results = results.merge(actual, on='Pick', how='left')
results = results.merge(total, on='Team', how='left')
results['PCT'] = (results['TPS']/results['PPS'])*100
results.columns.values[1] = 'Optimal'
results.columns.values[2] = 'OPS'
print results[results.Team == 'Boston Bruins']
print ""

counts = results.groupby(['Team']).count()
counts = counts['Optimal']
results2 = results.groupby(['Team']).mean()
results2 = results2[['Pick','TPS','PPS','PCT']]
results2 = pd.concat([counts,results2],axis=1,join_axes=[results2.index])
results2.columns = ['# of Picks','Average Pick', 'Total Value Drafted','Total Value Possible','% Value Extracted']
results2 = results2.round(2)
results2 = results2.sort_values(by='% Value Extracted',ascending=False)
print results2
