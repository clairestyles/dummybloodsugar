# Dummy blood sugar monitoring
Example project to record and visualise twice-daily blood glucose monitoring results 


## Motivation
To enabling graphing of blood sugar readings, both raw and summarised.  
To demonstrate ability to summarise and visualise data using R, and basic familiarity with GitHub.


## Completed functionality

Raw data converted to datasets:

* All measurements with daily average ("daily" data)
* Rolling average of the last 7 days, last 10 measurements or cumulative total ("rolling" data)

Graph function allows user to specify graph type (daily or rolling) and time period (all available data, or most recent X days).


## In the pipeline

* Summarise data at level of week and month
* Shiny app


## How to use

### Data
Use the example data provided, OR

1. Create a CSV file of blood sugar readings entitled "sugar.csv" with the setup below, in the same folder as the script. 

date | sug.am | sug.pm
--- | --- | ---
format dd/mm/yyyy (no leading zeros) | morning (or fasting) readings | evening (or non-fasting) readings

2. Populate the sugar columns with measurements in mmol/L (standard unit for blood sugar readings in Australia). Where no data exists, leave cell blank. [Note: Graph axes will accommodate measurements in mg/dL, but will be labelled as mmol/L.] 

### Plot
Execute the script unchanged to produce daily and rolling graphs (see above for definitions) of all available data, and the most recent 30 days of data (provided at least 30 days of data are available).  

To produce graphs of data for other time periods, run the final section with the desired number of days in the print(plotter()) commands.


