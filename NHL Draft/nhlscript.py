import pandas as pd

desired_width = 200
pd.set_option('display.width', desired_width)

# Calculate Draft Efficiency % past pick n for year x with expanded results for team y.
n = 0
x = 2007
y = 'Philadelphia Flyers'

# Read in .csv with Year, Overall Pick, Team, Player, Player Value.
# Here, Player Value is Career Point Shares (PS).
stats = pd.read_csv('~/Downloads/draftstats.csv')
stats = stats.sort_values(by='PS', ascending=False)
stats = stats[stats.Overall > n]
stats = stats[stats.Year == x]
stats['Overall'] = stats['Overall'] - n
# Some players have negative career Point Shares, which we zero out. Bad NHL player >= no NHL player.
stats.loc[stats['PS'] < 0, 'PS'] = 0
stats1 = stats[['Overall','Team']]
stats1 = stats1.sort_values(by='Team', ascending=True)

stats1.set_index(keys=['Team'], drop=False, inplace=True)
names = stats1['Team'].unique().tolist()

results = []

# Iteratively delete the best available player at each pick (working backwards) for each team.
# Then look at all the deletions.
for i in names:
    team = i
    df = stats1.loc[stats1.Team == i]
    df = df.sort_values(by='Overall', ascending=False)
    df1 = list(df['Overall'])
    stats2 = stats
    for i in df1:
        stats2 = stats2[['Overall','Team','Player','PS']]
        stats2.loc[stats2['Overall'] >= i, 'Rank'] = range(1, len(stats2) - i + 2)
        stats2.Rank = stats2.Rank.fillna(value=0)
        stats2 = stats2[stats2.Rank != 1]
    result = stats[~stats.isin(stats2)].dropna()
    result = result[['Team','Player','PS','Overall']]
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
actual.columns = ['Pick','Actual','APS']
total = stats[['Team','PS']]
total.columns.values[1] = 'TPS'
total = total.groupby(['Team']).sum()
total['Team'] = total.index

results = results.merge(actual, on='Pick', how='left')
results = results.merge(total, on='Team', how='left')
results['Pick'] = results['Pick'] + n
results['Overall'] = results['Overall'] + n
results['PCT'] = (results['TPS']/results['PPS'])*100
results.columns.values[1] = 'Optimal'
results.columns.values[2] = 'OPS'
results1 = results.copy()
results1.columns = ['Team','Optimal Player','Optimal Value','Actual Draft Position','Pick Used','Perfect Draft Value',
                    'Actual Player','Actual Value','Total Value Drafted','Draft Efficiency %']
print results1[results1.Team == y]
print ""

counts = results.groupby(['Team']).count()
counts = counts['Optimal']
results2 = results.groupby(['Team']).mean()
results2 = results2[['Pick','TPS','PPS','PCT']]
results2 = pd.concat([counts,results2],axis=1,join_axes=[results2.index])
results2 = results2.round({'Pick': 0, 'TPS': 0, 'PPS': 0, 'PCT': 2})
results2 = results2.sort_values(by='PCT',ascending=False)
results2['Rank'] = range(1,len(results2)+1)
results2 = results2[['Rank','Optimal','Pick','TPS','PPS','PCT']]
results2.columns = ['Rank','# of Picks','Average Pick','Total Value Drafted','Perfect Draft Value','Draft Efficiency %']
results2.columns.names = [str(x) + ' NHL Draft']
results2.index.names = ' '
print results2
