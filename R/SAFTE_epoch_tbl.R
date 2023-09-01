#' SAFTE Epoch Table
#'
#' This function takes the raw, properly formatted data uploaded by the user and
#' creates tables of the 1 minute Epoch data. This is the first essential step
#' for using the SAFTE model.
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
#' @param obs_start Oberservation start time (i.e. beginning of the study). Formatted as 'YYYY-mm-dd HH:MM' (Required)
#' @param obs_end Observation end time (i.e. end of the study). Cannot be more than 21 days from the obs_start. Formatted as 'YYYY-mm-dd HH:MM' (Required)
#' @param sleep_id Marker ID for a sleep period (usually "Sleep" or "sleep"). Defaults to "Sleep"
#' @param work_id Marker ID for a work period. Defaults to "Work"
#' @param crewing_id Marker ID for a crewing period within a work period. Defaults to "Crewing"
#' @param test_id Marker ID for a test period (i.e. PVT taken or medication given). Defaults to "Test"
#'
#' @returns A list 4 tables. One for identified periods of sleep, work, testing, and/or crewing if applicable. Period types not identified will return a table as Null.
#'
#' @import tidyverse
#' @import lubridate
#'
#' @export


SAFTE_epoch_tbl<-
  function(dataset,
           subject_id,
           obs_start,
           obs_end,
           sleep_id = "Sleep",
           work_id = "Work",
           crewing_id = "Crewing",
           test_id = "Test") {

  #Turn subject ID into a string--this is so a mix of numerical and alpha-char can be used together
  subject_id<-as.character(subject_id)

  #Check date formats--this will be phased out in the future
  if(is.na(ymd_hm(obs_start))) stop("Date Format Incorrect. Dates needs to be formated 'YYYY-mm-dd HH:MM'")
  if(is.na(ymd_hm(obs_end))) stop("Date Format Incorrect. Dates need to be formated 'YYYY-mm-dd HH:MM'")


  #change observation start and end from character to datetimes types
  obs_start<-ymd_hm(obs_start)
  obs_end<-ymd_hm(obs_end)

  #Check observation length to be less than 21 days
  if(difftime(obs_end,obs_start, units = "days")>21) stop("Observation period is too long, must be less than 21 days")

  #create a table of 1 minute epochs from observation start to observation end of table
  smap_obs<-tibble(ID = as.character(subject_id),
                   Obs_DateTime = map2(obs_start,
                                       obs_end,
                                       seq,
                                       by = "1 mins")) %>%
    unnest(cols = Obs_DateTime) %>%
    distinct

  #create a table of 1 minute epochs of sleep events from dataset
  smap_sleep<- dataset %>%
    mutate(ID = as.character(ID)) %>%
    filter(ID == subject_id, Event==sleep_id) %>%
    transmute(ID, Obs_DateTime = map2(Start,
                                      End,
                                      seq,
                                      by = "1 mins")) %>%
    unnest(cols = Obs_DateTime) %>%
    distinct %>%
    mutate(Sleep = 1)

  #create a table of 1 minute epochs of work events from dataset
  smap_work<- dataset %>%
    mutate(ID = as.character(ID)) %>%
    filter(ID == subject_id, Event==work_id) %>%
    transmute(ID, Obs_DateTime = map2(Start,
                                      End,
                                      seq,
                                      by = "1 mins")) %>%
    unnest(cols = Obs_DateTime) %>%
    distinct %>%
    mutate(Work = 1)

  #create a table of 1 minute epochs of test events from dataset
  smap_test<- dataset %>%
    mutate(ID = as.character(ID)) %>%
    filter(ID == subject_id, Event==test_id) %>%
    transmute(ID, Obs_DateTime = map2(Start,
                                      End,
                                      seq,
                                      by = "1 mins")) %>%
    unnest(cols = Obs_DateTime) %>%
    distinct %>%
    mutate(Test = 1)

  #create a table of 1 minute epochs of crewing events from dataset
  smap_crewing<- dataset %>%
    mutate(ID = as.character(ID)) %>%
    filter(ID == subject_id, Event==crewing_id) %>%
    transmute(ID, Obs_DateTime = map2(Start,
                                      End,
                                      seq,
                                      by = "1 mins")) %>%
    unnest(cols = Obs_DateTime) %>%
    distinct %>%
    mutate(Crewing = 1)


  #Trim the data so it begins with the first epoch of sleep. The SAFTE model can
  ###  only accurately calculate efficiency after accounting for sleep


  first_sleep <- smap_sleep$Obs_DateTime[1]

  smap_obs<-smap_obs %>%
    filter(Obs_DateTime>=first_sleep)

  #join the tables together and replace NA w/ 0 for sleep, add Epochs #,
  Sleep_EBE<-left_join(smap_obs, smap_sleep, by = c("ID", "Obs_DateTime")) %>%
    mutate(Sleep = replace_na(Sleep, 0),
           Epochs = row_number()-1,
           Time = hour(Obs_DateTime) + (minute(Obs_DateTime)/60),
           Days = as.numeric(abs(as.Date(obs_start)-as.Date(Obs_DateTime)) + 1),
           Fraction_Days = (Days + Time/24) - 1
    )

  #join the tables together and replace NA w/ 0 for wake, add Epochs #, if no work
  ###  markers are observed than table is NUll

  if(nrow(smap_work)>1){
    Work_EBE<-left_join(smap_obs, smap_work, by = c("ID", "Obs_DateTime")) %>%
      mutate(Work = replace_na(Work, 0),
             Epochs = row_number()-1,
             Time = hour(Obs_DateTime) + (minute(Obs_DateTime)/60),
             Days = as.numeric(abs(as.Date(obs_start)-as.Date(Obs_DateTime)) + 1),
             Fraction_Days = (Days + Time/24) - 1
      )
  }else{
    Work_EBE<-NULL
  }

  #join the tables together and replace NA w/ 0 for wake, add Epochs #, if no test
  ###  markers are observed than table is NUll

  if(nrow(smap_test)>1){
    Test_EBE<-left_join(smap_obs, smap_test, by = c("ID", "Obs_DateTime")) %>%
      mutate(Test = replace_na(Test, 0),
             Epochs = row_number()-1,
             Time = hour(Obs_DateTime) + (minute(Obs_DateTime)/60),
             Days = as.numeric(abs(as.Date(obs_start)-as.Date(Obs_DateTime)) + 1),
             Fraction_Days = (Days + Time/24) - 1
      )
  }else{
    Test_EBE<-NULL
  }

  #join the tables together and replace NA w/ 0 for crewing, add Epochs #, if no
  ###  crewing makers are observed than table is NUll

  if(nrow(smap_crewing)>1){
    Crewing_EBE<-left_join(smap_obs, smap_crewing, by = c("ID", "Obs_DateTime")) %>%
      mutate(Test = replace_na(Crewing, 0),
             Epochs = row_number()-1,
             Time = hour(Obs_DateTime) + (minute(Obs_DateTime)/60),
             Days = as.numeric(abs(as.Date(obs_start)-as.Date(Obs_DateTime)) + 1),
             Fraction_Days = (Days + Time/24) - 1
      )
  }else{
    Crewing_EBE<-NULL
  }

  ### Create output in a list to be referenced for future analysis

  EBE_Data<-list(Sleep_EBE = Sleep_EBE, Work_EBE = Work_EBE, Test_EBE = Test_EBE,
                 Crewing_EBE = Crewing_EBE)

  return(EBE_Data)

}

