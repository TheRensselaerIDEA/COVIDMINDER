import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import warnings
import matplotlib.dates as mdates
from statsmodels.tsa.statespace.sarimax import SARIMAX

path = '~/Documents/covid/'
pathdata = path + 'data/csv/time_series/covid_NY_TS_plot.cases.csv'
pathDiabetes = path + 'data/csv/time_series/NY_counties_diabetes.csv'
Regions = pd.read_csv(path + 'data/csv/time_series/NY_counties_regions.csv', index_col='County')


def Build_Data(pathdata, Regions):
    DataPlot = pd.read_csv(pathdata, parse_dates=True)
    DataPlot.set_index(['date', 'County'], inplace=True)
    List_Regions = pd.unique(Regions['Region'])
    Regions_Cases = pd.DataFrame()
    for region in List_Regions:
        DataRegion = DataPlot[DataPlot['Region'] == region]
        DataRegion = DataRegion.sum(level=0)
        DataRegion['p_cases'] = DataRegion['cases'] / DataRegion['Population'] * 100000
        Regions_Cases[region] = DataRegion['p_cases']
    Regions_Cases.index = pd.to_datetime(Regions_Cases.index)
    Regions_Daily_Cases = Regions_Cases.diff()
    Regions_Daily_Cases = Regions_Daily_Cases.fillna(0)
    Regions_Daily_Cases = Regions_Daily_Cases.rolling(7, min_periods=1).sum()
    return (DataPlot, Regions_Daily_Cases)


def Build_Training_Data(DataPlot, Regions_Daily_Cases, pathDiabetes, Regions, exog=True, v=7):
    '''
    Builds lists to input in the ARIMA model
    :param DataPlot: Dataframe of the data over days
    :param pathDiabetes: path to the csv Diabetes data
    :param Regions: Regions Dataframe
    :param exog: Boolean to return or not the exogenous data
    :param v: Number of days to exclude from training
    '''
    List_Regions = pd.unique(Regions['Region'])
    Diabetes = pd.read_csv(pathDiabetes, parse_dates=True)
    Diabetes.set_index(['County'], inplace=True)
    Values = []
    Pop = []
    Food_Insecure = []
    Dict_Pop = {}
    Dict_Food_Insec = {}
    for region in List_Regions:
        DataRegion = DataPlot[DataPlot['Region'] == region]
        DataRegion = DataRegion.sum(level=0)
        counties = pd.unique(DataPlot[DataPlot['Region'] == region].index.get_level_values('County'))
        Food_InsecureReg = Diabetes.loc[counties]['no_Food_Insecure'].sum() / DataRegion['Population'].iloc[0]
        Values = Values + list(Regions_Daily_Cases[region].iloc[:-v]) + 5 * [np.nan]
        Dict_Food_Insec[region] = Food_InsecureReg
        Dict_Pop[region] = DataRegion['Population'].iloc[0]
        if exog:
            Pop = Pop + [DataRegion['Population'].iloc[0]] * len(Regions_Daily_Cases[region].iloc[:-v]) + 5 * [0]
            Food_Insecure = Food_Insecure + [Food_InsecureReg] * len(Regions_Daily_Cases[region].iloc[:-v]) + 5 * [0]
    if exog:
        return (Values, Pop, Food_Insecure, Dict_Pop, Dict_Food_Insec)
    else:
        return (Values)


def GridSearch(Regions, Regions_Daily_Cases, Values, Food_Insecure=None, Pop=None, Dict_Pop=None, Dict_Food_Insec=None,
               exog=True, plot=False, v=7):
    Palette = dict(Regions[['Region', 'Color']].to_dict('split')['data'])
    warnings.filterwarnings("ignore")
    formatter = mdates.DateFormatter('%a %d/%m')
    params = []
    scoresExog = []
    plt.style.use('ggplot')
    List_Regions = pd.unique(Regions['Region'])
    for p in range(1, 5):
        for q in range(1, 5):
            for d in range(3):
                try:
                    if exog:
                        model = SARIMAX(Values, exog=np.array([Pop, Food_Insecure]).transpose(), order=(p, d, q),
                                        missing='drop', enforce_invertibility=False)
                    else:
                        model = SARIMAX(Values, order=(p, d, q),
                                        missing='drop', enforce_invertibility=False)
                    results = model.fit(disp=0)
                    scores_counties = []
                    plt.figure()
                    ax = plt.gca()
                    plt.xticks(rotation=20)
                    ax.xaxis.set_major_locator(mdates.DayLocator(interval=7))
                    ax.xaxis.set_major_formatter(formatter)
                    for region in List_Regions:
                        DataCounty = Regions_Daily_Cases[region].dropna()
                        if exog:
                            ModelCounty = SARIMAX(DataCounty[:-v],
                                                  exog=np.array([[Dict_Pop[region]] * len(DataCounty[:-v]),
                                                                 [Dict_Food_Insec[region]] * len(
                                                                     DataCounty[:-v])]).transpose(),
                                                  order=(p, d, q), missing='drop', enforce_invertibility=False)
                        else:
                            ModelCounty = SARIMAX(DataCounty[:-v], order=(p, d, q), missing='drop',
                                                  enforce_invertibility=False)
                        res = ModelCounty.smooth(results.params)
                        fc = res.get_prediction(len(DataCounty) - v, len(DataCounty), exog=np.array(
                            [[Dict_Pop[region]] * (v + 1), [Dict_Food_Insec[region]] * (v + 1)]).transpose())
                        frame = fc.summary_frame(alpha=0.05)
                        fc = frame['mean']
                        Y = DataCounty.iloc[-v:].values
                        Yhat = fc[-v:].values
                        # Ybar = np.mean(Y)
                        MAE = (sum(abs(Y - Yhat)) / v)
                        scores_counties.append(MAE)
                        confInf = frame['mean_ci_lower']
                        confSup = frame['mean_ci_upper']
                        if plot:
                            pl = plt.plot(DataCounty, label=region, color=Palette[region])
                            plt.fill_between(confInf.index, confSup, confInf, alpha=0.3, color=pl[0].get_color())
                            plt.title("Daily Cases Predicted with a single ARIMA({},{},{}) model".format(p, d, q))
                            plt.plot(fc, '--', color=pl[0].get_color())
                    if plot:
                        plt.text(1, 0.9, 'Mean Absolute Error : {:.0f}'.format(np.nanmean(scores_counties)),
                                 transform=ax.transAxes, horizontalalignment='left')
                        # plt.xlim([DataCounty.iloc[-v-7:].index[0], DataCounty.iloc[-v-7:].index[-1]])
                        plt.yscale('log')
                        plt.legend(bbox_to_anchor=(1, 0.5), loc='center left', fontsize=6)
                        plt.savefig('PredictionCountiesDailyExog/ARIMA{}{}{}_Pred.png'.format(p, d, q))
                        plt.show()
                    scoresExog.append(np.nanmean(scores_counties))
                    params.append((p, d, q))
                except:
                    print('Training Failed for parameters :')
                    print(p, d, q)

    argbest = np.argmin(scoresExog)
    print('Best distance : ', scoresExog[argbest])
    print('Best params : ', params[argbest])
    BestParams = params[argbest]
    return BestParams, scoresExog[argbest]


def Prediction(Regions, Regions_Daily_Cases, Values, BestParams, Food_Insecure=None, Pop=None, exog=True, plot=False,
               v=7):
    BestMod = SARIMAX(Values, exog=np.array([Pop, Food_Insecure]).transpose(), order=BestParams, missing='drop',
                      enforce_invertibility=False)
    BestRes = BestMod.fit()
    List_Regions = pd.unique(Regions['Region'])
    BestRes.summary()
    Predictions = pd.DataFrame(columns=['region', 'mean', 'mean_ci_upper', 'mean_ci_lower'])
    for region in List_Regions:
        DataCounty = Regions_Daily_Cases[region].dropna()
        if exog:
            ModelCounty = SARIMAX(DataCounty, exog=np.array([[Dict_Pop[region]] * len(DataCounty),
                                                             [Dict_Food_Insec[region]] * len(
                                                                 DataCounty)]).transpose(), order=BestParams,
                                  missing='drop', enforce_invertibility=False)
        else:
            ModelCounty = SARIMAX(DataCounty, order=BestParams, missing='drop', enforce_invertibility=False)
        res = ModelCounty.smooth(BestRes.params)
        if exog:
            fc = res.get_prediction(0, len(DataCounty) + v, exog=np.array(
                [[Dict_Pop[region]] * (v + 1), [Dict_Food_Insec[region]] * (v + 1)]).transpose())
        else:
            fc = res.get_prediction(0, len(DataCounty) + v)
        frame = fc.summary_frame(alpha=0.05)
        fc = frame['mean']
        confInf = frame['mean_ci_lower']
        confSup = frame['mean_ci_upper']
        frame['region'] = [region] * len(frame)
        Predictions = Predictions.append(frame[['region', 'mean', 'mean_ci_upper', 'mean_ci_lower']])
        if plot:
            pl = plt.plot(DataCounty, label=region, color=Palette[region])
            plt.fill_between(confInf.index, confSup, confInf, alpha=0.3, color=pl[0].get_color())
            plt.plot(fc, '--', color=pl[0].get_color())
            plt.title('Best ARIMA Predictions Cases per 100k for ' + region)
            # plt.legend(bbox_to_anchor=(1,0.5),loc='center left',fontsize=6)
            plt.yscale('log')
            plt.savefig('PredictionsARIMABestExog/' + region)
            plt.show()
    Predictions.index.name = 'date'
    return (Predictions)


DataPlot, Regions_Daily_Cases = Build_Data(pathdata, Regions)
Values, Pop, Food_Insecure, Dict_Pop, Dict_Food_Insec = Build_Training_Data(DataPlot, Regions_Daily_Cases, pathDiabetes,
                                                                            Regions, exog=True, v=7)
Params, results = GridSearch(Regions, Regions_Daily_Cases, Values, Food_Insecure, Pop, Dict_Pop, Dict_Food_Insec, True,
                             False, 7)
Pred = Prediction(Regions, Regions_Daily_Cases, Values, Params, Food_Insecure, Pop, exog=True, plot=False)
Pred.to_csv(path+'data/csv/time_series/covid_NY_Prediction.csv')
