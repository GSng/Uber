"""
Created on Sat Oct 20 16:11:18 2013

@author: g_singhal
"""

import os
from datetime import datetime
from json import loads
import dateutil
import numpy as np

#R used to build/forecast models
import pandas as pd
from rpy2.robjects import r
import pandas.rpy.common as com
r('library(forecast)')

hourly_volume = None

print 'Done!'

#Model is seasonal->Weekly Season Start=1st Fri of dataset
def get_friday_index(hourly_volume):
    c = 0
    for i in hourly_volume.index:
        c+=1
        if i.dayofweek == 4:
            return c
#Monthly Season Start=1st Fri of 1st Month of dataset
def get_first_of_month(hourly_volume):
    d = hourly_volume.index[:1][0]
    sd = dateutil.parser.parse('%d/%d/%d' %(d.year, d.month, 1))
    rng = pd.Series(index=pd.date_range(sd, periods=31, freq='D'))
    for i in rng.index:
        #print i
        if i.dayofweek==5:
            fri = i.day
            return d.day-fri+2

#Takes JSON file, builds model
def create_model(input_json):
    global hourly_volume
    
    #Loads JSON file
    print 'Loading Data...'
    json = loads(input_json)
    
    #Converts to Pandas Time Series Dataframe which can be converted to be used by R
    df = pd.DataFrame(json)
    df.columns = ['time']
    df['time'] = df['time'].apply(dateutil.parser.parse)
    df.set_index('time', inplace=True)
    df['t'] = 1
    
    #Resamples Dataframe into hourly (for weekly model) and daily (for monthly model) buckets
    hourly_volume = df.resample('1H', how=np.count_nonzero)
    daily_volume =  df.resample('1D', how=np.count_nonzero)
    
    print 'Creating Model...'
	
    #Converts Pandas Dataframe to R Dataframe
    demand_data_daily = com.convert_to_r_dataframe(daily_volume)
    demand_data_hourly = com.convert_to_r_dataframe(hourly_volume)
    
    #Brings Dataframes into R workspace
    r.assign('train_data_hourly',demand_data_hourly)
    r.assign('train_data_daily',demand_data_daily)
	#Assigns values to required input variables in R
    r('start_index = ' +str(get_friday_index(hourly_volume)))
    r('month_index = ' +str(get_first_of_month(hourly_volume)))
    
    #Reorganizes hourly dataframe to seasonal time series w/ 168 hr weekly intervals starting at the 1st Fri    
    r('train_data_ts <- ts(train_data_hourly[,1],start=c(1,(168-start_index+2)),frequency=168)')
    #Adds 0.01 as model input data must be non-zero
    r('train_data_ts = train_data_ts+ 0.01')
	#R creates hourly model we set beta=0 as we assume no global trend (HOLTZ-WINTERS MODEL)
    r('hr_model <- HoltWinters(train_data_ts,beta=0,seasonal="m",start.periods=(168+start_index-1))')
    
    #R creates a monthly model IFF there is enough data (min 8 weeks)
    r('dy_model = NULL')
    #1st Fri of hourly dataset translated for daily dataset
    r('start_index = (start_index-1)/24+1')
    if (r('length(train_data_daily[,1])>(28*2+start_index-1)')[0]):
        #if the first fri of the month of the dataset proceeds start date of dataset, sets to prior month's first fri
        r('if(month_index<1){month_index = 28-month_index }')
        #Reorganizes daily dataframe to seasonal time series
        r('train_data_ts <- ts(train_data_daily[,1],start=c(1,month_index),frequency=28)')
		#R creates monthly model, again we assume no global trend
        r('dy_model <- HoltWinters(train_data_ts,seasonal="m",start.periods=(28+start_index-1))')
    
	print 'Model Created!'

#Specify End Date -> Produces forecast
def get_prediction_for_daterange(ed='2012-05-20 23:00:00+00:00', asutc=False):
    #start date for forecast is necessarily the period proceeding the last period of the dataset
    sd = hourly_volume.index[-1:][0].isoformat()
    print 'Forecasting...'
    
    #Time Zone info is annoying
    sd = dateutil.parser.parse(sd).replace(tzinfo=None)
    ed = dateutil.parser.parse(ed).replace(tzinfo=None)
    
    #Compute # of days to forecast ahead
    td = ed-sd
    td = td.days+1
    #Brings days to forecast into R
    r('d_forecast = '+str(td)+' # of days forecast')
    
    #R creates hourly forecasts for the specified number of days to forecast ahead
    r('HRforecast <- forecast.HoltWinters(hr_model,h=(d_forecast*24))')
    r('forecast_result <- HRforecast$mean')
    
    #If monthly model was built (min 8 weeks of data), reconciles hourly forecast with daily forecast over month
    if (r('!is.null(dy_model)')[0]):
        
        #R creates daily forecast for the number of days to forecast ahead
        r('dy_forecast <- forecast.HoltWinters(dy_model,h=d_forecast)')
        #Extract daily forecast vector from time-series forecast object
        r('dy_fcst_result <- dy_forecast$mean')
        r('dy_fcst_result  <- unclass(dy_fcst_result)')
        r('dy_fcst_result <- dy_fcst_result[]')
        
        #Re-bucket hourly forecast into daily forecast
        r('hr_fcst_by_day <- unname(tapply(forecast_result, (seq_along(forecast_result)-1) %/% 24, sum))')
        #scale hourly model by daily model
        r('scale_coefs <- dy_fcst_result/hr_fcst_by_day')
        #Scale scaling-coefficient vector to length of hourly forecast vector
        r('scale_coefs <- as.vector(matrix(rep(scale_coefs,each=24),nrow=24))')
        #Extract hourly forecast vector from time-series forecast object
        r('hr_fcst_result <- unclass(forecast_result)')
        r('hr_fcst_result <- hr_fcst_result[]')
        #multiply hourly forecast vector by scaling ceofficients to get final result
        r('forecast_result <- hr_fcst_result*scale_coefs')
    
    #Load prediction from R into Python workspace	    
    pred = com.load_data('forecast_result')
    #Convert prediction to Pandas time series
    pred = pd.Series(pred)
    rng = pd.date_range(sd, periods=td*24, freq='H')
    pred.index = pd.Series(rng, name='time')
    
    ret = pred.reset_index() 
    if asutc:
        ret.index =  ret.time.apply(lambda x: str(x))
    else:
        ret.index =  ret.time.apply(lambda x: int(x.strftime('%s')))
    ret = ret[0]
    return ret#.to_dict()

#Returns time series for actual (non-forecast) data
def get_actual():
    actual = hourly_volume.reset_index()
    actual = actual.reset_index() 
    actual.index =  actual.time.apply(lambda x: int(x.strftime('%s')))
    actual = actual['t']
    return actual

def get_prediction_ftime():
    ret = pred.reset_index() 
    ret.index =  ret.time.apply(lambda x: int(x.strftime('%s')))
    ret = ret[0]
    return ret