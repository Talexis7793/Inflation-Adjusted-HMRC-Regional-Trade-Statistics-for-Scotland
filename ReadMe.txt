#### Reference guides ####

Methodology note:
https://erdm.scotland.gov.uk:8443/documents/A45314010/details

Visio visualisation of methodology and code:
https://scotsconnect.sharepoint.com/:u:/r/sites/EUEA-SG/_layouts/15/Doc.aspx?sourcedoc=%7BD6710EFF-8975-475F-904C-554BA16966DB%7D&file=CVM%20code.vsdx&action=default&mobileredirect=true


#### Using the code ####

When updating the data to a new release of Regional Trade Statistics, follow these steps.

#Note - You can ignore the 'API download' and 'API Data - processing' scripts if you save the current price data as a CSV into the '1) data/raw' folder (naming convention: HMRC RTS 2024 Q2 - UK data SITC2 level - imports and exports.csv). Then run the '2) Alternative method to read in data - csv' script. Only use the API if this is for some reason unavailable.

# You can circumvent the next 2 steps by running script "2) Alternative method to read in data". This requires that a csv file containing the raw RTS data is already saved in the folder: "1) data/raw/HMRC RTS ",year_want," ", quarter_want," - UK data SITC2 level - imports and exports.csv". This can be created on pre-release day using SAS code in "GCS\HMRC RTS\Quarterly Publications\Automated Reporting\RTS report automation - R code\Read in data". Only do the next 2 steps to read in data using the API if you don't already have the CSV (ie not on pre-release day as the most recent data won't be available on the API anyway): 

	# First, go to 2 ) data - API download
	-Update data_date variable to latest quarter and run. 
	-This will download and save the raw RTS data (this might take some time, maybe around an hour).

	# Next, go to 2) API Data - processing.R
	-There are some options here that you may want to change. 
	-I include all unknown regions (see line 75-82) in the UK data. You may wish to exclude this data.
	-Second, lines 10 to 17 impute a value for an erroneous customs declaration of prefabricated buildings. 
	-Have a look to see if this data (in netmass) still looks wrong. If it has been revised then remove lines 10-18. 
		You might want to plot netmass in excel to see if there are any spikes or drops in the latest period to investigate.
	-This program will save data from the API into a csv file, so you don't need to run it every time you re-run the analysis (just use the csv unless 	you want to refresh the data).

# Then go to 2b) seasonal adjustment.R (in the seasonal adjustment subfolder).
-Change any values you need to at the start of the script, what they do should be explained in comments. all_uk and country_breakdown are important as they determine which flows are included (just UK & Scotland or every UK nation, just EU/non-EU or each partner country). 
-This also saves outputs as csvs and names them according to which breakdown you have chosen.

# Then go to 3) CVM Calculations.R (you shouldn't need the manual script)
- Change data_date variable to latest quarter
- Change any of the options at the start of the script, comments in the code tell you what they do. 
- Using write_to_clip and copy_to_clipboard options, choose whether you want script to be written to the outputs folder or copied to your clipboard (quite useful option)
- Run whole script.
- This should give you cvm_series dataframe with all of your outputs (and saved to csv in the outputs folder if you selected that option)

# Then go to 3b) CVM calculations at SITC2 level.
 - Change data_date and run the whole script. 
 - This should write the SITC 2 level analysis to CSV.

- At this point I usually compare this with the most recent version of the previous quarter's outputs. The CVM series should look very similar until the last few quarters. 
- If there are significant changes to previous quarters then you want to investigate whether there have been revisions or you have a data/processing issue.
- Then (perhaps using visualisation) you might want to investigate whether any SITC1 level results have seen large swings in CVM or IDEF. 
	This could indicate an erronous data entry that you have to remove/impute, or (hopefully not!) a problem with the code/data download.
	It is good to compare this entry to the raw data to see where the issue is occuring.
	As an example, in the 2023Q2 data I realised that A) there was the aforementioned prefab buildings data issue, and 
		B) the SITC 0 products were written as 1, 2, 3, etc, instead of 01, 02, 03, which caused issues during the aggregation. I only saw this when checking the SITC0 results compared to older data.
	


# Then go to 5) Visualisation.R
ggplot_line_chart() charts:
- The first few graphs using ggplot_line_chart in this file are the ones used in the powerpoint. 
- Turn save_switch = TRUE (saves the outputs) and run the ggplot_line_chart code.
- You can use this to update the powerpoint
- You can also play around with the charts to see if you can find any interesting insights, eg. changing which variables are displayed.
- index_year input changes the index year for the base
- legend_var will change which variable is used as the legend (and therefore which variable there will be a seperate lines for)
- A graph with only one line will add an extra line that shows 2019 (pre-pandemic) levels)


- The next few charts are a bit more fiddly, since I never put them into function format. Please feel free to do this! The UK-Scotland SITC1 Charts and Partner-Flowtype CVM vs CP charts are both the same code, and could be functionalised.

UK-Scotland SITC1 Charts:
- This is a facet plot. This is where you have a few different panels for different variables. I use it for the powerpoint for Implied Price Deflators by SITC1 product charts.  
- ref_year chooses the reference year in the plot, which year to set = 100.
- sitc_chart_df chooses the variables that will be in the plot. 
- For powerpoint, you should only have to change between flowtype = "Imports" and flowtype = "Exports". 
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


Scotland - UK share Chart:
- I dont use this very much, you can just choose a baseline and run this. 
- It will produce a chart showing the share of Scotland as a proportion of the UK in Value and Volume measures, as well as the difference.


# Finally! Go to 6) Tables.R
- This code is a bit work in progress.
- I would only use the code from line starting "#### Year ending statistics #### "
- Choose the most recent quarter number, and what year you want to compare the current year-ending to (probably 2019). 
- Run the code
- I would just copy the results into a spreadsheet and format as in the powerpoints. You may wish to output in a more simple way. See powerpoint for examples.
- The two main outputs used in the powerpoint are: 
	Trade by UK, Scotland &  Exports, Imports by SITC1 level vs pre-pandemic levels
		(you can also do this by EU or Non EU by changing the table_sitc1_YE filter partner to "EU" or "Non EU" from "World"
	Key trade statistics vs pre-pandemic levels.

