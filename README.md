# CRM-ARIMA-GARCH
The aim of this report is to predict the daily stock price of CRM in the last 20 days of 2019 base on the CRM daily stock price from January 2016 to December 2019 by choosing an adequate ARIMA model and GARCH model.

## Table of contents
* [General info](#general-info)
* [Results](#results)

## General info
Salesforce.com, Inc. is a cloud-based software company headquartered in San Francisco, California. Salesforce provides customer relationship management (CRM) service and technology for managing the company’s relationships and interactions with customers. More than 150,000 companies are using Salesforce software. For 2018, Salesforce reported earnings of $127 million. 
The data used in this report is the CRM stock daily closing prices which are collected every evening except weekends and holidays. The data is obtained from Yahoo through the package quantmod in R.
Training Sample: It consist of 985 times observations corresponding to 985 daily data points from January 2016 to December 2019.
Testing Sample: It consists of 20 times observations corresponding to 20 daily data points represent the last 20 days of December 2019.

## Results
Forecasts from ARIMA(2,0,4) with non-zero mean (blue line), the 80% forecast limits (blue), and the 95% forecast limits (gray).
![arima-forecast](https://user-images.githubusercontent.com/56982400/95800900-96eb6800-0cc6-11eb-81d5-6e5ae39b2c8a.jpeg)

Forecasts from GARCH(2,1) and the mean model ARIMA(2,0,4) with the 95% forecast limits (yellow).
![garch-forcast](https://user-images.githubusercontent.com/56982400/95800929-aec2ec00-0cc6-11eb-9c9d-89318934c27c.jpeg)

Summary results of forecasts from ARIMA(2,0,4) and GARCH(2,1) with the mean model ARIMA(2,0,4).
![result-table](https://user-images.githubusercontent.com/56982400/95801106-4de7e380-0cc7-11eb-86e1-c7bf4d360840.png)
