# -*- coding: utf-8 -*-
"""
Created on Sun Oct 20 16:22:23 2013

@author: g_singhal
"""

from flask import Flask,jsonify, Response
from flask import url_for, redirect
from flask.globals import request
from datetime import datetime 
from json import dumps, loads
from flask.templating import render_template
from flask.helpers import send_from_directory
from flask import url_for, redirect 
from functools import wraps
import urllib2
import pandas as pd
import dateutil
import timeser

app = Flask(__name__)

s = None

#loads assigned data by default
if __name__ == "__main__":
    f = open('../uber_demand_prediction_challenge.json')
    s = f.read()
    timeser.create_model(s)
    app.run(host='0.0.0.0', port=5002)

#Can load or append any JSON file
@app.route('/', methods=['GET', 'POST'])
def upload_file():
    global s
    if request.method == 'POST':
        #open filename        
        f = request.files['file']
        #read file             
        json = f.read()
        #create new or append depending on input type
        inputType = request.form.get('inputType') 
        if inputType=='Create_New':
            #build model from JSON file  
            timeser.create_model(json)
        else:
            #extend(json)
            old_json = loads(s)
            new_json = loads(json)
            old_json.extend(new_json)
            s = dumps(old_json)
            timeser.create_model(s)
               
        #redirects to forecast visualization
        return redirect("/vis")
    return '''
    <!doctype html>
    <title>Upload new File</title>
    <h1>Upload new File</h1>
    <form action="" method=post enctype=multipart/form-data>
      <p><input type=file name=file>
         <input type="radio" name="inputType" value="Create_New">CreateNew<br>
         <input type="radio" name="inputType" value="Append_Existing">Append<br>
         <input type=submit value=Upload>
         </form>
    '''
#Visualizes Forecast
@app.route('/vis')
def home():
    return render_template('ajax.html')

@app.route('/predict', methods=['GET','POST'])
# @support_jsonp
def predict():
    #Redundant: start_data=first proceeding period of end of dataset    
    #start_date = request.args.get('start_date', '')
    end_date = request.args.get('end_date', '2012-05-05 23:00:00+00:00')  
    if end_date=='null':
        end_date = '2012-05-05 23:00:00+00:00'
    
    #Depending on output type perform the appropriate prediction action
    #timeser.get_prediction_for_daterange builds forecast    
    output_type = request.args.get('type', 'json')
    if output_type=='coords':
        #For visualization below methods are called        
        return dumps(get_coords(end_date))    
    elif output_type=='json':
        predicted_demand = timeser.get_prediction_for_daterange(end_date, asutc=True)
        return jsonify(predicted_demand.to_dict())
    else:
        predicted_demand = timeser.get_prediction_for_daterange(end_date, asutc=True)
        odir = './generated_csvs/'
        tfile =  'temp'+str(datetime.now())+'.csv'
        predicted_demand.to_csv(odir  + tfile)
        return send_from_directory(odir, tfile)

def get_coords(end_date):
    #get_prediction_for_daterange in timeser.py -> creates foreasts
    data = get_incoords(timeser.get_prediction_for_daterange(end_date))
    #ret is the dataframe for the visualization graph
    ret = [{'data':data, 'name':'forecast', 'color':'red'}]
    #retrieves the input data, transforms to ret dataframe
    actual = get_incoords(timeser.get_actual())
    #appends forecat data to input data
    ret.append({'data':actual,'name':'actual','color':'blue'})
    return ret

#transforms data so it can be inthe ret framework
def get_incoords(df):
    data = []   
    def a(x):
        time = int(x['time'])
        data.append({'x':time,'y':int(x[1])})   
    tdf = df.reset_index()
    tdf.apply(a,axis=1)
    return data

app.debug = True
