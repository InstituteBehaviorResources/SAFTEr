# SAFTEr
R package for researchers to apply the SAFTE model to sleep data for comparison to other collected observations.

## Background and Disclaimer
SAFTEr is a package written for R that applies the Sleep, Activity, Fatigue, and Task Effectiveness (SAFTE) model to previously collected sleep data. The SAFTE model is a biomathematical framework that predicts human cognitive performance based on previous sleep patterns. <br><br>
SAFTEr is based on the original patented model under expired US Patent No. 6,579,233 Entitled: System and Method for Evaluating Task Effectiveness Based on Sleep Pattern. SAFTEr was constructed using the original functions and algorithms provided by and under the supervision of SAFTE model inventor Dr. Steven Hursh. <br><br>
This R package should not be confused with similarly trademarked product names, such as SAFTE (Fatigue Science) or SAFTE-FAST (Institutes for Behavior Resources, Inc.). SAFTEr is limited to the use of the original patented SAFTE model equations and does not include features specific to the commercial SAFTE-FAST software: <br>
* scheduling insights
* Auto-Sleep or automated sleep estimation
* graphical user interface to interact with the data
* data importing from scheduling systems
* batch processing of large data sets
* a mechanism to phase-shift the model resulting from shift work or transmeridian travel
* does not consider geographic location or light exposure based on location or time of year
* does not include derived performance metrics or subjective alertness scores
* does not include any improvements to the original model made since the time of the patent. <br><br> 

**The SAFTEr package is intended for use by students, scholars, and scientific researchers as a freely available research tool and is not intended to be used, marketed, or sold as a product or as a service for profit.**

## Installation
You can install the released version of SAFTEr from GitHub with: <br><br>
install.packages("devtools") <br>
devtools::install_github("InstituteBehaviorResources/SAFTEr")

## Functionality
Datasets need to be formatted correctly before using the SAFTEr functions (see below). <br>
Functions need to be applied in the following order: <br>
1. SAFTE_epoch_tbl
1. SAFTE_model
1.	SAFTE_plot / SAFTE_plot_compare_dt / SAFTE_plot_compare_ln

## Formatting Data
The sleep data must be loaded into R and formatted correctly before the dataset can run through the SAFTEr functions. The number of columns in the dataset do not matter; however, there are a few key columns that need to have the following headers so that SAFTEr can identify them:<br>
* ID – Subject/Participant ID
* Event – Event type such as Sleep, Work, Crewing, Test, etc. Event identification can be defined in function
* Start – Start date and time of event as a POSIXct as `YYYY-mm-dd HH:MM’ \* 
* End – End date and time of event as a POSIXct as `YYYY-mm-dd HH:MM’ \* 

\*The easiest way to make sure your datetimes are of the correct data class is to check the class using the base R class() function. You can also make sure to import your data using tidyverse’s readr package and import interface to define the column as ‘datetime’

## SAFTE_epoch_tbl
Once your data table has the proper headers and is imported into your environment, you will need to run it through the SAFTE_Epoch_tbl() function. This will return a tibble of 1-minute Epoch tables for Sleep, Work, Crewing, and/or Testing data. <br> <br>
You will need to identify the imported dataset, the subjectID of the participant, the data label for each Event, and an observation start and end time. <br> <br>
**SAFTEr is currently set to run data within in a 3 week period. <br><br>
SAFTEr is also programmed to start after the first instance of sleep and will cut off all events that happen prior to first sleep.**

## SAFTE_model
This function will apply the classic SAFTE model to the dataset. <br><br>
You will need to pass in the dataset tibble returned from SAFTE_epoch_tbl as well as a bedtime in a 24hr format (ex: 11:00 PM should be entered as “23:00).<br><br>
This function will take some time to process.
## SAFTE_plot
This function will plot the return from the SAFTE_model function with overlapping time series of events so you can visualize when events (sleep, work, etc.) occur with the SAFTE model output.<br><br>
SAFTE_plot_compare… will allow you to compare other datasets that match up with the SAFTE output epoch times. For example, this gives the user the ability to show test scores and the SAFTE model output within the same plot area.

## Contact for Help or Contributions
Please contact Jake Choynowski at jchoynowski@ibrinc.org for any questions on using, citing, or contributing to  SAFTEr. 




