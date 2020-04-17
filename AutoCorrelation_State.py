import os

import pandas as pd
from statsmodels.tsa.api import VAR

all_files = [i[0] + '/' + files for i in os.walk('data/csv', False) for files in i[2] if files[:2].isnumeric()]
d0 = pd.read_csv(all_files[0])
d0 = d0[d0['Country_Region'] == 'US']
States = list(pd.unique(d0['Province_State']))
States.remove('Recovered')
list_data = []
for state in States:
    data = pd.DataFrame(columns=['Confirmed', 'Deaths', 'Recovered', 'Active', 'Date'])
    for file in all_files:
        data_state = pd.read_csv(file)
        data_state = data_state[data_state['Province_State'] == state]
        data_state = data_state[['Confirmed', 'Deaths', 'Recovered', 'Active']]
        data_state = data_state.sum()
        date = file[9:-4]
        data_state['Date'] = date
        data = data.append(data_state, ignore_index=True)
    data.set_index('Date', inplace=True)
    print(state + '\n\n')
    list_data.append(data)
    data.index = pd.to_datetime(data.index, format="%m-%d-%Y")
    data = data.asfreq('d')
    data = data.fillna(method='ffill')
    try:
        model = VAR((data[['Confirmed', 'Deaths']]).astype('float64'), dates=data.index)
        results = model.fit(3)
        fig = results.plot_forecast(7)
        fig.savefig('Predictions/' + state + '_pred.png')
        fig.show()
    except ValueError:
        print('Impossible to estimate')

##
