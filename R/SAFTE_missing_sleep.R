##########################  SAFTE Missing Sleep #################################
####                                                                         ####
###   This takes the raw, properly formatted data uploaded by the user and    ###
####     searches for any dates that do not include Sleep. This could be     ####
###      helpful in identifying missing nights of sleep data which can        ###
####     dramatically affect the calculations in the SAFTE model.            ####
###                                                                           ###
####     This will output a table listing all 24 hour periods that do not    ####
###      have an instance of sleep.                                           ###
###                                                                          ####
#################################################################################

#### REQUIRED LIBRARIES:
# library(tidyverse)
# library(lubridate)


SAFTE_missing_sleep<-
  function(dataset,     #correctly formatted dataset with start and end periods
           subject_id,  #subject_id of the specific subject to model
           split_time = 12,  # the time at which the user wants to split the day
           sleep_id,   #marker id for a sleep period (usually noted as "Sleep")
           obs_start, #observation start time (beginning of study)
           obs_end){ # obsevation end time (end of study)

  #define start and end period as dates
  obs_start<- as.Date(obs_start)
  obs_end <- as.Date(obs_end)


 #create a tibble of distinct days within observation period
  observation_period <- tibble(ID = subject_id , Event_Date = map2(obs_start,
                                                                   obs_end,
                                                                   seq,
                                                                   by = "1 days")) %>%
    distinct() %>%
    unnest(cols = Event_Date)


  #create a tibble of filtered sleep events and assign to days based on cutoff time
  dataset<-dataset %>%
    filter(Event == sleep_id & ID %in% subject_id) %>%
    mutate(Event_Date = if_else(End < as_date(End) + hours(split_time),
                                as_date(End),
                                as_date(End + days(1))))

  #anti-join the tables to show missing sleep dates
  missing_sleep<-anti_join(observation_period, dataset, by = c("ID", "Event_Date" ))

  return(missing_sleep)



  }
