SAFTE_model<-function(sleep_data){


  #Constants
  Epoch_Length<- as.double(sleep_data$Sleep_EBE$Obs_DateTime[2] - sleep_data$Sleep_EBE$Obs_DateTime[1])
  Reservoir_Capacity <- 2880
  Normalization_Constant <- 96.7
  alpha_limit <- 3.4
  relative_amp_swc <- 0.00312
  mesor_sleep_propensity_rhythm <- 0
  amplitude_sleep_propensity_rhythm <- 0.6
  amplitude_sleep_inertia <-0.08
  amp_12hr_cycle <- 0.3
  relative_phase_12hr_cycle <- 3.2
  start_hour<-0
  acrophase_initial <- 20 ###currently set to 20, need to discuss with Steve if this is adjustable
  kappa_var<- 0.5
  Reservoir_Parameter1 <- .22
  Reservoir_Parameter2 <- .5
  Reservoir_Parameter3 <- .0015
  amp1<-7.8
  amp2<-5
  max_sleep_inertia_perct<-5


  #possible constants?
  acrophase<- 20
  pre_average_awake_hr <- 16
  run_average_3_awake_hr <- 16
  goal_phase<- 20
  proper_acrophase<-20
  average_awake_hr <- 16



  ############### Create Min/ HR Asleep/Awake Values
  st_table<-sleep_data$Sleep_EBE %>%
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

      st_table$reservoir_balance[i]<-if_else(st_table$Min_Asleep[i] > Epoch_Length,
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

  #st_table<-left_join(st_table, sleep_data$Sleep_EBE %>%
  # select(Obs_DateTime,Sleep),
  #   by = "Obs_DateTime")
  if(!is.null(sleep_data$Work_EBE)){

    st_table<-left_join(st_table, sleep_data$Work_EBE %>%
                          select(Obs_DateTime,Work),
                        by = "Obs_DateTime")
  }
  if(!is.null(sleep_data$Test_EBE)){

    st_table<-left_join(st_table, sleep_data$Test_EBE %>%
                          select(Obs_DateTime,Test),
                        by = "Obs_DateTime")
  }
  if(!is.null(sleep_data$Crewing_EBE)){

    st_table<-left_join(st_table, sleep_data$Crewing_EBE %>%
                          select(Obs_DateTime,Crewing),
                        by = "Obs_DateTime")
  }

  SAFTE_Table<-list(Epoch_Table = st_table,
                    Settings = list(SubjectIDs = unique(st_table$ID),
                                    Dataset = deparse(substitute(sleep_data))))


  return(SAFTE_Table)
}