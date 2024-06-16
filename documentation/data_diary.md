2024-06-13
* Created a tibble of each MSA as a row and each analysis dimension as a column and filled them with a category if they were "Much worse/better", "Slightly worse/better", or "Similar" to the U.S. overall situation for each measure (i.e. employment, wage, establishments, inflation, spending, GDP)
* Used this CSV for a table made in Flourish that is color coded.

2024-06-12
* Downloaded BEA GDP chained 2017 dollar 72 NAICS supersector data for each MSA and nationally from 2017 to 2022.
* Downloaded CAGR GDP chained 2017 dollar 72 NAICS supersector data for each MSA for period of 2017 to 2019, 2019 to 2020, and 2020 to 2022.
* Created a CSV for a Datawrapper line chart of percent difference from '17 to '19 GDP trend for each MSA and nationally from 2020 to 2022.
* Created a CSV for a Datawrapper grouped bar chart for the CAGR from 2017-2019, 2019-2020, and 2020-2022 for each MSA

2024-06-11
* Downloaded Affinity consumer spending data from [Opportunity Insights Economic Tracker](https://github.com/OpportunityInsights/EconomicTracker) for cities and nationally and saved to `raw_data` folder.
* Selected only weekly data from before 2022-06-05 when OI Affinity data went from daily (7 day average) to weekly. Aligned daily to weekly by only selecting daily data for Sundays. 
* Calculated a 12 week rolling average in consumer spending compared to pre-pandemic index period for all 10 MSAs and natinoally
* Created a CSV to put into Datawrapper of percentage change of consumer spending in accommodation and food services  from pre-pandemic index period from April 2020 to Juny 2024. One column for each of the 10 MSAs and for the national level.

2024-06-10
* Calculated percentage change in restaurant industry average weekly wages from 2019 to each quarter from Q1 2020 to Q4 2023 for all 10 MSAs and the U.S. in much the same way that was done monthly for employment data
* Created a CSV to put into Datawrapper of change from 2020 to 2023 of quarterly wage i levels compared to the same quarter in 2019. One column for each of the 10 MSAs and for the national level. 
* Calculated the percent change of the yearly _average_ of restaurant establishments in 2019 and 2023 from the quarterly QCEW data for 10 MSAs and the U.S.
* Created a CSV to put into Datawrapper of a bar chart of the percentage change in restaurant establishments from 2019 to 2023 for 10 MSAs and U.S.

2024-06-05
* Downloaded the quarterly QCEW zip file data on the restaurant industry
* Extracted data from zip files in `raw_data` folder into `working_data`
* Combined CSVs into one data frame
* Calculated percentage change in restaurant industry employment for all 10 MSAs and the U.S. 
* Created a CSV to put into Datawrapper of change from 2020 to 2023 of monthly restaurant industry employment levels compared to the same month in 2019. One column for each of the 10 MSAs and for the national level.

2024-06-04
* Created data frame of combined CBSA and national food away from home CPI inflation data
* Filtered data frame down to only MSAs in my analysis set and only for months after March 2019
* Calculated the average annual growth rate of food away from home inflation for 10 MSAs in analysis set and nationally.
* Created a CSV to put into Datawrapper of average annual growth rate of food away from home inflation for 10 MSAs in analysis set as well as an extra column for the national change.

2024-06-03
* Decided on 10 CBSAs that are in CPI and OI data as well as have ~1 or >1 average location quotient for restaurant industry employment, establishments, and wages in 2019.
* Created join file with 10 CBSAs and their fips codes that are present in QCEW, CPI, and OI data.
* Calculated the average location quotient for employment, establishments, and wages and and ranked them. They range from a high of 1.343 in Honolulu to 0.963 in Los Angeles.
* Created dot plot of 2019 location quotients for employment, establishments, and wage for 10 CBSAs.