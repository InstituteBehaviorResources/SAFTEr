#' SAFTE Model
#'
#' This function takes the SAFTE_epoch_tbl output and runs it through the SAFTE model
#'
#' The current algorithm is still being refactored for quicker calculations. The current
#' function can run 1+ minutes for final output. A progress bar should display (progress package)
#'
#'
#' @param dataset Dataset output from SAFTE_epoch_tbl
#' @param bedtime Goal or average bedtime of the subject. Should be formatted in 24hr time as "HH:MM". Default is set to"23:00" or 11PM
#'
#'
#' @returns This will output a table listing all 24 hour periods that do not have an instance of sleep.
#'
#' @import tidyverse
#' @import lubridate
#' @import progress
#'
#' @export





SAFTE_model<-
  function(dataset, bedtime = "23:00"){

    #Check bedtime format before conversion
    if(is.na(hm(bedtime))) stop("Bedtime Format Incorrect. Bedtime needs to be formated 'HH:MM'")


  # Constants
  Epoch_Length<- as.double(dataset$Sleep_EBE$Obs_DateTime[2] - dataset$Sleep_EBE$Obs_DateTime[1])
  Reservoir_Capacity <- 2880
  Normalization_Constant <- 96.7
  alpha_limit <- 3.4
  relative_amp_swc <- 0.00312
  mesor_sleep_propensity_rhythm <- 0
  amplitude_sleep_propensity_rhythm <- 0.55
  amplitude_sleep_inertia <-0.08
  amp_12hr_cycle <- 0.5
  relative_phase_12hr_cycle <- 3
  kappa_var<- 0.5
  Reservoir_Parameter1 <- .22
  Reservoir_Parameter2 <- .5
  Reservoir_Parameter3 <- .0015
  amp1<-7.8
  amp2<-5
  max_sleep_inertia_perct<-5

  # Calculate Acrophase

  bedtime<-hm(bedtime)
  start_hour<-hour(bedtime) + (minute(bedtime)/60)

  acrophase<- as.numeric(if_else(start_hour - 5 <0, start_hour + 19, start_hour - 5))

  # Create Min/ HR Asleep/Awake Values
  st_table<-dataset$Sleep_EBE %>%
    group_by(grp = with(rle(Sleep), rep(seq_along(lengths), lengths))) %>%
    mutate(ct = as.numeric(1:n()),
           Min_Asleep = if_else(Sleep > 0, ct, 0),
           Hr_Asleep = Min_Asleep/60,
           Min_Awake = if_else(Sleep == 0, ct, 0),
           Hr_Awake = Min_Awake/60
    )%>%
    ungroup() %>%
    select(-grp, -ct)




  st_table<-st_table %>%
    mutate(circadian_24plus12 = (cos(6.28319 * (st_table$Time - acrophase)/24) +
                                   amp_12hr_cycle *
                                   cos(12.56637 * (st_table$Time - acrophase - relative_phase_12hr_cycle )/24)
    ),
    sleep_propensity = mesor_sleep_propensity_rhythm - amplitude_sleep_propensity_rhythm * circadian_24plus12,
    func_p = if_else(Sleep == 0, kappa_var * Epoch_Length, 0),
    sleep_debt = 0,
    sleep_intensity =  0,
    alpha_var =  0,
    func_s =  0,
    rc_adjust =  0,
    reservoir_balance =  0)

  pb <- progress_bar$new(
    format = "  Analyzing [:bar] :percent eta: :eta time elapsed :elapsed",
    total = nrow(st_table), complete = 'z', current = 'Z', clear = FALSE, width= 60)

  for(i in 1:nrow(st_table)){
    pb$tick()


    if(i == 1){

      st_table$sleep_debt[i] = relative_amp_swc * (Reservoir_Capacity - 2400)

      st_table$sleep_intensity[i] = st_table$sleep_debt[i] + st_table$sleep_propensity[i]

      st_table$alpha_var[i] = if_else(st_table$Sleep[i] == 1, st_table$sleep_intensity[i], 0)

      st_table$func_s[i] = if_else(st_table$alpha_var[i] > alpha_limit, alpha_limit * Epoch_Length, st_table$alpha_var[i] * Epoch_Length)

      st_table$rc_adjust[i] = if_else(st_table$Sleep[i]==1,Reservoir_Capacity + Epoch_Length * Reservoir_Parameter1, Reservoir_Capacity)

      st_table$reservoir_balance[i] = 2400 - st_table$func_p[i]
    }else{
      st_table$sleep_debt[i] = relative_amp_swc * (st_table$rc_adjust[i-1] - st_table$reservoir_balance[i-1])

      st_table$sleep_intensity[i] = st_table$sleep_debt[i] + st_table$sleep_propensity[i]

      st_table$alpha_var[i] = if_else(st_table$Sleep[i] == 1, st_table$sleep_intensity[i], 0)

      st_table$func_s[i] = if_else(st_table$alpha_var[i] > alpha_limit, alpha_limit * Epoch_Length, st_table$alpha_var[i] * Epoch_Length)

      st_table$rc_adjust[i] = if_else(st_table$Sleep[i]==1,
                                      if_else(st_table$rc_adjust[i-1] > Reservoir_Capacity, Reservoir_Capacity,
                                              st_table$rc_adjust[i-1] + Epoch_Length * (Reservoir_Parameter1 * (1 - (st_table$sleep_debt[i-1]/Reservoir_Parameter2)) +
                                                                                          Reservoir_Parameter3 * (Reservoir_Capacity - st_table$rc_adjust[i-1]))), st_table$rc_adjust[i-1])

      st_table$reservoir_balance[i]<-if_else(st_table$Sleep[i] == 1,
                                             if_else((st_table$reservoir_balance[i-1] + st_table$func_s[i] - st_table$func_p[i]) <0,0,
                                                     if_else(st_table$reservoir_balance[i-1]  > 2880,
                                                             st_table$reservoir_balance[i-1]  - st_table$func_p[i],
                                                             st_table$reservoir_balance[i-1]  + st_table$func_s[i] - st_table$func_p[i])),
                                             st_table$reservoir_balance[i-1]  - st_table$func_p[i])



    }

  }

  st_table<- st_table %>%
    mutate(
      Variable_C_Amp = if_else(Epochs == 0, amp2 * (Reservoir_Capacity - 2400)/Reservoir_Capacity + amp1,
                               amp2 * (Reservoir_Capacity - lag(reservoir_balance, n =1))/Reservoir_Capacity + amp1),

      func_c = if_else(Epochs==0, if_else((2400 + func_s-func_p) < 0, 0,
                                          Variable_C_Amp * circadian_24plus12),

                       if_else((lag(reservoir_balance, n =1) + func_s-func_p) < 0, 0,
                               Variable_C_Amp * circadian_24plus12)),




      sleep_inertia = if_else(Epochs==0,0,
                              if_else(Min_Awake <= 0,0,
                                      if_else(sleep_intensity < 0.01,
                                              if_else(lag(Min_Awake, n = 1) > 240,0,
                                                      (-1) * (max_sleep_inertia_perct ^(-lag(Min_Awake, n = 1)*(1/0.01)*amplitude_sleep_inertia))),
                                              if_else(lag(Min_Awake, n=1) > 240, 0,
                                                      (-1)*(max_sleep_inertia_perct ^(-lag(Min_Awake, n=1)*(1/sleep_intensity)*amplitude_sleep_inertia)))))),

      Effectiveness = 100 * (100*(reservoir_balance/Reservoir_Capacity)+func_c+sleep_inertia)/Normalization_Constant,

      Reservoir_Percent = reservoir_balance/2800
    )

    if(!is.null(dataset$Work_EBE)){

    st_table<-left_join(st_table, dataset$Work_EBE %>%
                          select(Obs_DateTime,Work),
                        by = "Obs_DateTime")
  }
  if(!is.null(dataset$Test_EBE)){

    st_table<-left_join(st_table, dataset$Test_EBE %>%
                          select(Obs_DateTime,Test),
                        by = "Obs_DateTime")
  }
  if(!is.null(dataset$Crewing_EBE)){

    st_table<-left_join(st_table, dataset$Crewing_EBE %>%
                          select(Obs_DateTime,Crewing),
                        by = "Obs_DateTime")
  }

  SAFTE_Table<-list(Epoch_Table = st_table,
                    Settings = list(SubjectIDs = unique(st_table$ID),
                                    Dataset = deparse(substitute(dataset))))


  return(SAFTE_Table)
}
