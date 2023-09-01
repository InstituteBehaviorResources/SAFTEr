##########################  SAFTE EPOCH TABLE ########################################
####                                                                             ####
###   This takes the raw, properly formatted data uploaded by the user and        ###
####    creates tables of 1 minute Epoch data. This is the first essential step  ####
###     for using the SAFTE model. This is for raw data that has stated start     ###
####    (bedtime) and end (waketime) data and is not for EPOCH by EPOCH sleep    ####
###     data. It will return a list of 4 tables, one for identified periods of    ###
####    sleep, work, testing, and/or crewing if applicable. Only sleep is needed.####
###                                                                               ###
#####################################################################################

#### REQUIRED LIBRARIES:
# library(tidyverse)
# library(lubridate)




SAFTE_epoch_tbl<-
  function(dataset, #correctly formatted dataset with start and end periods
           subject_id, #subject_id of the specific subject to model
           obs_start,  #observation start time (beginning of study)
           obs_end,   # obsevation end time (end of study) CANNOT be longer than 21 days
           sleep_id = "Sleep", #marker id for a sleep period (usually noted as "Sleep")
           work_id = "Work",   #marker id for a work period
           crewing_id = "Crewing", #marker id for a crewing period within work period
           test_id = "Test") { #maker for a test period (i.e. PVT taken or medication given)

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

