library(tidyverse)
library(lubridate)


SAFTE_missing_sleep<-function(dataset, subject_id, split_time, sleep_event, obs_start, obs_end){

  #define start and end period as dates
  obs_start<- as.Date(obs_start)
  obs_end <- as.Date(obs_end)


 #create a tibble of days within observation period
  observation_period <- tibble(ID = subject_id , Event_Date = map2(obs_start,
                                                                   obs_end,
                                                                   seq,
                                                                   by = "1 days")) %>%
    distinct %>%
    unnest(cols = Event_Date)


  #create a tibble of filtered sleep events and assign to days based on cutoff time
  dataset<-dataset %>%
    filter(Event == sleep_event & ID %in% subject_id) %>%
    mutate(Event_Date = if_else(End < as_date(End) + hours(split_time),
                                as_date(End),
                                as_date(End + days(1))))

  #anti-join the tables to show missing sleep dates
  return(anti_join(observation_period, dataset, by = c("ID", "Event_Date" )))
  #return(dataset)



}
