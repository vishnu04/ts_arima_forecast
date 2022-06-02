# TimeSeries Forecasting using ARIMA models
- Forecasting Profits on superstores dataset containing 9993 sales records using ARIMA models
- Dataset : superstores.csv
- After data preprocessing and EDA, (p,d,q) values of ARIMA models are found for both seasonal and without seasonal adjustments.
- Fit models with different p,d and q values and tried to find best forecasting model based on AIC, RMSE and Ljung-Box-p values

## Code and Resources Used
R Version: 4.1.2\
Packages: lubridate, xts, tseries, ggplot2, forecast

## Data Pre-Processing
	- Removing unwanted columns (Row.ID)
	- Formatting date and other columns
	- Checking for any NA values 
	- Selecting subset of data (Columns : Order.Date, Sales, Quantitiy, Discount, Profit) 
	- Aggregating the data for each day based on Order.Date
	- Converting the dataframe to timeseries
	
## Exploratory Data Analysis
	- Creating Daily, Weekly and Monthly time series objects 
	- Plotting Daily, Weekly and Monthly time series objects with and without difference (diff = 1)
	- Based on the variance and not considering too much smooth in variance,
	  decided to perform further analysis on Weekly time series object.
	- Plotting Moving averages of Weekly Sales with/without difference (diff = 1)
	- Testing for Stationarity using Augmented Dickey-Fuller Test
	- Checking for any seasonality by decomposition of the Weekly sales data 
	
## Model Building
	- Finding p, d and q values for Weekly sales time series ( with / without seasonal adjustments) 
	- Building forecast models based on the p,d and q values
	- Finding the best model based on AIC, RMSE and Ljung-Box-p values.
		
### Notes:
	- /TimeSeries_Forecasting_ARIMA_files/: Contains all the output images of the markdown file
	- TimeSeries_Forecasting_ARIMA.md: Contains R code used for analysis and implementing forecasting models.
	- superstores.csv: Original Dataset 

## Conclusion:
	- Based on AIC, RMSE, p, d and q values, Auto Arima Model (p=1, d=1 and q = 2) with seasonal adjustment gives
	  better forecast values when compared to other models.
