select_attributes_ODiN <- function (df) {
  df <- df %>% 
    sjlabelled::remove_all_labels() %>% 
    tibble()    # without this, results will be cast as a list

  df <- df %>%
    select(OPID,
           HHPers,
           HHPlOP,
           WoPC,
           Geslacht,
           Leeftijd,
           Herkomst,
           HHGestInkG,
           OPRijbewijsAu,
           FqNEFiets,
           FqEFiets,
           FqBTM,
           FqTrein,
           FqAutoB,
           FqAutoP,
           FqBrSnor,
           Jaar,
           Maand,
           Dag,
           Weekdag,
           Feestdag,
           AantVpl,
           AantOVVpl,
           
           VerplID,
           Doel,
           VertLoc,
           VertGeb,
           VertPC,
           AankGeb,
           AankPC,
           AfstV,
           Hvm,
           HvmRol,
           VertUur,
           VertMin,
           AankUur,
           AankMin,
           Reisduur,
           
           RitID,
           AfstR,
           Rvm,
           RvmRol,
           RVertUur,
           RVertMin,
           RAankUur,
           RAankMin,
           RVertStat,
           RAankStat,
           
           FactorP
    ) %>%
    rename(agent_ID = OPID,
           hh_size = HHPers,
           hh_position = HHPlOP,
           hh_PC4 = WoPC,
           gender = Geslacht,
           age = Leeftijd,
           migration_background = Herkomst,
           hh_std_income = HHGestInkG,
           has_driving_license = OPRijbewijsAu,
           freq_ebike = FqNEFiets,
           freq_bike = FqEFiets,
           freq_bus_tram_metro = FqBTM,
           treq_train = FqTrein,
           freq_car_driver = FqAutoB,
           freq_car_passenger = FqAutoP,
           freq_moped = FqBrSnor,
           year = Jaar,
           month = Maand,
           day = Dag,
           weekday = Weekdag,
           is_festivity = Feestdag,
           freq_trips = AantVpl,
           freq_trips_public_transp = AantOVVpl,
           
           disp_id = VerplID,
           disp_reason = Doel,
           disp_start_home = VertLoc,
           disp_start_NL = VertGeb,
           disp_start_PC4 = VertPC,
           disp_arrival_NL = AankGeb,
           disp_arrival_PC4 = AankPC,
           disp_distance = AfstV,
           disp_modal_choice = Hvm,
           disp_role = HvmRol,
           disp_start_hour = VertUur,
           disp_start_min = VertMin,
           disp_arrival_hour = AankUur,
           disp_arrival_min = AankMin,
           disp_duration = Reisduur,

           
           ride_id = RitID,
           ride_distance = AfstR,
           ride_modal_choice = Rvm,
           ride_role = RvmRol,
           ride_start_hour = RVertUur,
           ride_start_min = RVertMin,
           ride_arrival_hour = RAankUur,
           ride_arrival_min = RAankMin,
           ride_start_train_station = RVertStat,
           ride_arrival_train_station = RAankStat,
           
           p_value = FactorP
    )
  return(df)
}

select_attributes_OViN <- function (df) {
  df <- df %>% 
    sjlabelled::remove_all_labels() %>% 
    tibble()    # without this, results will be cast as a list
  
  df <- df %>%
    select(OPID,
           HHPers,
           HHPlOP,
           Wogem,
           Geslacht,
           Leeftijd,
           Herkomst,
           HHGestInkG,
           Rijbewijs,
           Jaar,
           Maand,
           Dag,
           Weekdag,
           AantVpl,
           AantOVVpl,
           
           VerplID,
           VerplNr,
           Doel,
           Vertrekp,
           VertPCBL,
           VertPC,
           AankPCBL,
           AankPC,
           AfstV,
           Hvm,
           RolOPAuto,
           VertUur,
           VertMin,
           AankUur,
           AankMin,
           Reisduur,
           
           RitID,
           AfstR,
           Rvm,
           KRvm,
           RVertUur,
           RVertMin,
           RAankUur,
           RAankMin,
           RVertStat,
           RAankStat,
           
           FactorP
    ) %>%
    rename(agent_ID = OPID,
           hh_size = HHPers,
           hh_position = HHPlOP,
           hh_municipality = Wogem,
           gender = Geslacht,
           age = Leeftijd,
           migration_background = Herkomst,
           hh_std_income = HHGestInkG,
           has_driving_license = Rijbewijs,
           year = Jaar,
           month = Maand,
           day = Dag,
           weekday = Weekdag,
           freq_trips = AantVpl,
           freq_trips_public_transp = AantOVVpl,
           
           disp_id = VerplID,
           disp_counter = VerplNr,
           disp_reason = Doel,
           disp_start_home = Vertrekp,
           disp_start_PC4 = VertPC,
           disp_arrival_PC4 = AankPC,
           disp_distance = AfstV,
           disp_modal_choice = Hvm,
           disp_role = RolOPAuto,
           disp_start_hour = VertUur,
           disp_start_min = VertMin,
           disp_arrival_hour = AankUur,
           disp_arrival_min = AankMin,
           disp_duration = Reisduur,
           
           ride_id = RitID,
           ride_distance = AfstR,
           ride_modal_choice = Rvm,
           ride_role = KRvm,
           ride_start_hour = RVertUur,
           ride_start_min = RVertMin,
           ride_arrival_hour = RAankUur,
           ride_arrival_min = RAankMin,
           ride_start_train_station = RVertStat,
           ride_arrival_train_station = RAankStat,
           
           p_value = FactorP
    )
  return(df)
}

select_attributes_OViN_2014 <- function (df) {
  df <- df %>% 
    sjlabelled::remove_all_labels() %>% 
    tibble()    # without this, results will be cast as a list
  
  df <- df %>%
    select(OPID,
           HHPers,
           HHPlOP,
           Wogem,
           Geslacht,
           Leeftijd,
           Herkomst,
           HHGestInkG,
           Rijbewijs,
           Jaar,
           Maand,
           Dag,
           Weekdag,
           AantVpl,
           AantOVVpl,
           
           VerplID,
           VerplNr,
           Doel,
           Vertrekp,
           VertPCBL,
           VertPC,
           AankPCBL,
           AankPC,
           AfstV,
           Hvm,
           RolOPAuto,
           VertUur,
           VertMin,
           AankUur,
           AankMin,
           Reisduur,
           
           RitID,
           AfstR,
           Rvm,
           KRVm,
           RVertUur,
           RVertMin,
           RAankUur,
           RAankMin,
           
           FactorP
    ) %>%
    rename(agent_ID = OPID,
           hh_size = HHPers,
           hh_position = HHPlOP,
           hh_municipality = Wogem,
           gender = Geslacht,
           age = Leeftijd,
           migration_background = Herkomst,
           hh_std_income = HHGestInkG,
           has_driving_license = Rijbewijs,
           year = Jaar,
           month = Maand,
           day = Dag,
           weekday = Weekdag,
           freq_trips = AantVpl,
           freq_trips_public_transp = AantOVVpl,
           
           disp_id = VerplID,
           disp_counter = VerplNr,
           disp_reason = Doel,
           disp_start_home = Vertrekp,
           disp_start_PC4 = VertPC,
           disp_arrival_PC4 = AankPC,
           disp_distance = AfstV,
           disp_modal_choice = Hvm,
           disp_role = RolOPAuto,
           disp_start_hour = VertUur,
           disp_start_min = VertMin,
           disp_arrival_hour = AankUur,
           disp_arrival_min = AankMin,
           disp_duration = Reisduur,
           
           ride_id = RitID,
           ride_distance = AfstR,
           ride_modal_choice = Rvm,
           ride_role = KRVm,
           ride_start_hour = RVertUur,
           ride_start_min = RVertMin,
           ride_arrival_hour = RAankUur,
           ride_arrival_min = RAankMin,
           
           p_value = FactorP
    )
  return(df)
}

filter_hh_PC4 <- function(df, df_PC4) {
  df <- df[df$hh_PC4 %in% df_PC4,]
  return(df)
}

filter_hh_municipality <- function(df, municipality_code) {
  df <- df[df$hh_municipality == municipality_code,]
  return(df)
}

home_municipality_to_PC4 <- function(df, df_PC4) {
  # take only the rows/agents that start the day from home
  df <- df[df$disp_start_home==1,]
  
  # find the starting point of the first move, and set it as home
  df$hh_PC4=NA
  df[df$disp_counter==1,]$hh_PC4 = df[df$disp_counter==1,]$disp_start_PC4
  
  # apply attribute to all the rows of the agent
  df = df %>%
    group_by(agent_ID) %>% 
    mutate(hh_PC4 = zoo::na.locf(hh_PC4, na.rm=FALSE))
  
  return(df)
}

filter_out_NA <- function(df){
  return (df)
}