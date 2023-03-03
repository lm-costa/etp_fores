
# **Forecasting potential evapotranspiration through machine learning algorithms to optimize the hydrological services assessment**

This repository contains the data and codes used

## **DATA RAW**

In the `data-raw` folder is the database.csv file, which consists of the
junction of all the stations analyzed from **CFSR**. To create this
single file we used the data available in `data-raw/cfsr_new`, which is
the data of each station with corrected date (`data-raw/cfsr`). In this
folder you will also find the nasa power files (`data-raw/power`).

## **DATA**

In this folder is the reference ETp data calculated from CFSR and NASA
power data. You will also find the python code for modeling, and the
files with the interactions of each model, and the file that will be
used for the water balance with the estimated and observed ETp for nasa
power (wd.csv). Finally you will find the climatology csv file.

## **R**

In this folder you will find all the R language code used.

*1- corr_fun.R: this is the function created to correct the date format*

*2- database_creation.R: code used to create the database.csv file*

*3- climatology.R: Code used to calculate climatology data for each
station and for the whole* basin

*4 - etp_baseline.R: Code used to calculate monthly ETp for both
databases, which were later used for modeling.*

*5 - water_balance.R: code used to calculate observed and estimated
water balance (from random forest) for the validation data (NASA-POWER)*
