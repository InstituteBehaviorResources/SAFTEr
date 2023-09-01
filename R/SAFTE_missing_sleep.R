#' SAFTE Missing Sleep
#'
#' This function takes the raw, properly formatted data uploaded by the user and
#' searches for any dates that do not include "Sleep". This could be helpful in
#' identifying missing night of sleep data which can dramatically affect the
#' calculations in the SAFTE model.
#'
#' Properly formatted data at a minimum has a single column for
#' sleep identification, a single column for start datetimes for bedtime, and a
#' single column for end dateimes for waketimes. It can also include work, test,
#' and crewing identified periods within the same columns.
#'
#' This is not for EPOCH by EPOCH data or data already divided into 1 minute
#' epochs.
#'
#' @param dataset Name of the properly formatted dataset uploaded into the R environment by the user (Required)
#' @param subject_id Identifier of the subject/participant/patient. (Required)
#' @param split_time #the hour (0-23) at which the user wants to split the day. Deafults to noon ("12"). (Required)
#' @param sleep_id Marker ID for a sleep period (usually "Sleep" or "sleep"). Defaults to "Sleep"#'
#' @param obs_start Oberservation start time (i.e. beginning of the study). Formatted as 'YYYY-mm-dd HH:MM' (Required)
#' @param obs_end Observation end time (i.e. end of the study). Cannot be more than 21 days from the obs_start. Formatted as 'YYYY-mm-dd HH:MM' (Required)
#'
#' @returns This will output a table listing all 24 hour periods that do not have an instance of sleep.
#'
#' @import tidyverse
#' @import lubridate
#'
#' @export


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
