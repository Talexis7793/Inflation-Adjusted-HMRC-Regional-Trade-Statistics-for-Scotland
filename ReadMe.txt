### Background and purpose ###

This project represents the first stage of processing in the production of https://www.gov.scot/publications/inflation-adjusted-hmrc-regional-trade-statistics-for-scotland-q4-2024/. The final output is seasonally and inflation adjusted data in csv format, which can be used as input into later reporting projects. The inflation adjustment is performed using calculations known as Chain Volume Measures (CVM).

This code produces csv outputs and visualisations of HMRC's Regional Trade Statistics. The data are read in either using HMRC's API or from a csv file which can be manually uploaded. The code then seasonally adjusts the data and tests it for volatility; this is important because if a time series shows a lot of volatility the calculations that come later will likely be inaccurate. There is some automated QA and data validation built into the process which is explained in comments throughout, for example if a particular series (e.g. exports of fish & shellfish to either EU or non-EU) are found to be volatile then this commodity code is added to the list of erratics and excluded from inflation adjustment calculations. Though this is automated, some manual checking may be required, and this is explained in notes and messages output to the console when running the code. The final step is to adjust the data for inflation by calculating CVMs. 

The final output is a set of csv files which can be fed into a visualisation program within this project, or copied over for use within a much larger reporting program, which also produces a markdown report for the above publication. 

#### Using the code ####

When updating the data to a new release of Regional Trade Statistics:

Open the control script and the 'create cvm files' script. The control script details several options for configuring the analysis and output from the project, there are comments explaining what each of them do. There are also some recommendations in the 'create cvm files' script for setting these options. Then run each of the source statements in the 'create cvm files' script in order, first for the EU/non-EU breakdown. The same scripts are called multiple times because we have to run the code through at different levels of granularity (e.g. EU/Non-EU breakdown vs. trade with each individual partner country). Between running the process for EU/non-EU and partner country level breakdown you will have to reconfigure the options in the control script, but this is explained in comments. 

The 'create cvm files' script should contain all the scripts you need to run to create the outputs necessary to update the publication. You can ignore the 'API download' and 'API Data - processing' scripts if you save the current price data as a CSV into the '1) data/raw' folder (naming convention: HMRC RTS 2024 Q2 - UK data SITC2 level - imports and exports.csv). Only use the API if this is for some reason unavailable. The scripts are set up to rely on the csv file rather than the API primarily because we have to run this script on pre-release data when the data on the API will not yet be updated. 

The output from most scripts is a csv file which is read in by the next script in the pipeline. You should not need to worry about this, as long as you leave the control script configured consistently then each script should know which file to read in.

This is all you need to do if you just want to update the data using the standard process. If you want to run the process more manually keep reading:

1. As above configure the control script. 

2. Read in the data. Either use the '2) Alternative method to read in data - csv' script or if you want to use the API:

	First, go to 2 ) data - API download
	-Update data_date variable to latest quarter and run. 
	-This will download and save the raw RTS data (this might take some time, maybe around an hour).

	Next, go to 2) API Data - processing.R
	-There are some options here that you may want to change. 
	-I include all unknown regions (see line 75-82) in the UK data. You may wish to exclude this data.
	-Second, lines 10 to 17 impute a value for an erroneous customs declaration of prefabricated buildings. 
	-Have a look to see if this data (in netmass) still looks wrong. If it has been revised then remove lines 10-18. 
		You might want to plot netmass in excel to see if there are any spikes or drops in the latest period to investigate.
	-This program will save data from the API into a csv file, so you don't need to run it every time you re-run the analysis (just use the csv unless you want to refresh the data).


3. Run '2a) determine volatility'. This program tests each series for volality. What I mean by series depends on how you have configured the control script. If this is a breakdown for EU/Non-EU then it will be flows to each of these for each commodity category for example. Volatile series are added to the vector 'erratics'. The products (or countries if you are running this at a more granular level) in this vector will later be excluded from CVM calculations, as they could bias them. You might want to manually check and adjust this after running the script.

4. Run '2b) seasonal adjustment.R'. This may take considerable time (especially if you are looking at the breakdown for trade between individual countries rather than EU/Non-EU). The seasonal adjustment is run in a loop on each series in the input. The program is configured to handle errors gracefully, so if the seasonal adjustment fails for a particular series an error is output to the console, the series is marked as excluded from seasonal adjustment and the program keeps going. This is useful because there can be hundreds of series we want to adjust.

5. Run '2c) seasonal adjustment QA.R'. This QAs the results from the seasonal adjustment, and automatically reverts some series to using the unadjusted values if the process may have caused an error. Some data may not be appropriate for seasonal adjustment and may be overcorrected by it, so this program looks for large differences caused by the seasonal adjustment and other markers of problematic series (see the script for details). The series which have been reverted to unadjusted values are marked as excluded from seasonal adjustment, you might want to manually check them.

6. Run '3) 'CVM Calculations.R'. This should give you cvm_series dataframe with all of your outputs (and saved to csv in the outputs folder if you selected that option)

7. Run '3b) CVM calculations.R' at SITC2 level. This should write the SITC 2 level analysis to CSV.

- At this point I usually compare this with the most recent version of the previous quarter's outputs. The CVM series should look very similar until the last few quarters. 
- If there are significant changes to previous quarters then you want to investigate whether there have been revisions or you have a data/processing issue.
- Then (perhaps using visualisation) you might want to investigate whether any SITC1 level results have seen large swings in CVM or IDEF. 
	This could indicate an erronous data entry that you have to remove/impute, or (hopefully not!) a problem with the code/data download.
	It is good to compare this entry to the raw data to see where the issue is occuring.
	As an example, in the 2023Q2 data I realised that A) there was the aforementioned prefab buildings data issue, and 
		B) the SITC 0 products were written as 1, 2, 3, etc, instead of 01, 02, 03, which caused issues during the aggregation. I only saw this when checking the SITC0 results compared to older data.

8. Run '5) Visualisation.R'. This outputs charts in the outputs folder:
- You can play around with the charts to see if you can find any interesting insights, eg. changing which variables are displayed.
- reindex_year input changes the index year for the base
- legend_var will change which variable is used as the legend (and therefore which variable there will be a seperate lines for)
- A graph with only one line will add an extra line that shows 2019 (pre-pandemic) levels)

Other charts produced in this script:

	UK-Scotland SITC1 Charts:
	- This is a facet plot. This is where you have a few different panels for different variables. I use it for the powerpoint for Implied Price Deflators by SITC1 product charts.  
	- ref_year chooses the reference year in the plot, which year to set = 100.
	- sitc_chart_df chooses the variables that will be in the plot. 
	- But you can also play around with this, but you also need to change the following two variables
	- wrap_var - chooses the variable that will be different in each of the plots on the graph. By default this is the SITC1 products excluding 9 
	- legend - chooses the lines that will be compared on each plot. By dafault this is "country" which is UK and Scotland. 
 
	If you want to change what is plotted, you can. For instance, to plot, UK vs Scotland on each of the different variables (CVM, Value and IDEF):
	1) change sitc_chart_df:
 	- comment out variable filter so all variables are in the dataframe
 	- add agg == "Total excl.sitc3 & erratic" filter
	2) change wrap_var to variable
	3) change legend to country

Partner-Flowtype CVM vs CP Chart:
- this is the exact same format as the previous chart, but it is set up to produce the CP vs CVM measures chart in the powerpoint
- You can play around with this in the same way as above. 
- If you have time try to make these two charts into a function!

This process flow will produce e.g. breakdowns for product flows to EU/non-EU, but you can run the whole process again with the control script configured differently to get e.g. individual country level outputs.


