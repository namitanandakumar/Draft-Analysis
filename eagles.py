import pandas as pd

desired_width = 200
pd.set_option('display.width', desired_width)

picks = pd.read_csv('~/Downloads/98picks.csv')
picks = picks.sort_values(by='ARI', ascending=False)
# Each team is given the same amount of picks so that the loop runs correctly.
# Extra picks with NA values were added for teams that have fewer than the maximum number of picks.
# These are dropped in the results.
test = pd.read_csv('~/Downloads/98draft.csv')
test = test.sort_values(by='CarAV', ascending=False)

results = []

for i in range(len(picks.columns)):
    h = picks[picks.columns[i]]
    team = list(picks)[i]
    test1 = test
    for i in h:
        test1 = test1[['Overall', 'Team', 'Player', 'CarAV']]
        test1.loc[test1['Overall'] >= i, 'Rank'] = range(len(test1) - i + 1)
        test1['Rank'] = test1['Rank'] + 1
        test1.Rank = test1.Rank.fillna(value=0)
        test1 = test1[test1.Rank != 1]
    result = test[~test.isin(test1)].dropna()
    result = result[['Team', 'Player', 'CarAV', 'Overall']]
    result = result.sort_values(by='Overall', ascending=True)
    result = result.reset_index()
    result = result.drop('index', 1)
    result.columns.values[3] = 'Overall'
    result['Pick'] = h
    result['PCarAV'] = result['CarAV'].sum()
    result['Team'] = team
    results.append(result)
results = pd.concat(results, axis=0)

test2 = test[['Overall','Player','CarAV']]
test2.columns = ['Pick', 'Actual','ACarAV']
test3 = test[['Team','CarAV']]
test3.columns.values[1] = 'TCarAV'
test3 = test3.groupby(['Team']).sum()
test3['Team'] = test3.index

results = results.merge(test2, on='Pick', how='left')
results = results.merge(test3, on='Team', how='left')
results['PCT'] = (results['TCarAV']/results['PCarAV'])*100
counts = results.groupby(['Team']).count()
counts = counts['Player']
print results.head(n=100)

results2 = results.groupby(['Team']).mean()
results2 = results2[['Pick','TCarAV','PCarAV','PCT']]
results2 = pd.concat([counts,results2],axis=1,join_axes=[results2.index])
results2.columns = ['# of Picks','Average Pick', 'Total Value Drafted','Total Value Possible','% Value Extracted']
results2 = results2.round(2)
print results2.sort_values(by='% Value Extracted',ascending=False)
