# Select attributes and translate them into English. For ODiN only.
select_attributes_ODiN <- function (df) {
  df <- df %>% 
    sjlabelled::remove_all_labels() %>% 
    tibble()    # without this, results will be cast as a list

  df <- df %>%
    select(OP,
           OPID,
           HHPers,
           HHPlOP,
           WoPC,
           Geslacht,
           Leeftijd,
           Herkomst,
           HHGestInkG,
           OPRijbewijsAu,
           #FqNEFiets,
           #FqEFiets,
           #FqBTM,
           #FqTrein,
           #FqAutoB,
           #FqAutoP,
           #FqBrSnor,
           Jaar,
           Maand,
           Dag,
           Weekdag,
           #Feestdag,
           AantVpl,
           AantOVVpl,
           
           VerplID,
           Doel,
           VertLoc,
           VertPC,
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
           #RVertStat,
           #RAankStat,
           
           FactorP
    ) %>%
    rename(agent_new = OP,
           agent_ID = OPID,
           hh_size = HHPers,
           hh_position = HHPlOP,
           hh_PC4 = WoPC,
           gender = Geslacht,
           age = Leeftijd,
           migration_background = Herkomst,
           hh_std_income = HHGestInkG,
           has_driving_license = OPRijbewijsAu,
           #freq_ebike = FqNEFiets,
           #freq_bike = FqEFiets,
           #freq_bus_tram_metro = FqBTM,
           #treq_train = FqTrein,
           #freq_car_driver = FqAutoB,
           #freq_car_passenger = FqAutoP,
           #freq_moped = FqBrSnor,
           year = Jaar,
           month = Maand,
           day = Dag,
           weekday = Weekdag,
           #is_festivity = Feestdag,
           freq_trips = AantVpl,
           freq_trips_public_transp = AantOVVpl,
           
           disp_id = VerplID,
           disp_reason = Doel,
           disp_start_home = VertLoc,
           disp_start_PC4 = VertPC,
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
           #ride_start_train_station = RVertStat,
           #ride_arrival_train_station = RAankStat,
           
           p_value = FactorP
    )
  df$disp_start_PC4 <- as.character(df$disp_start_PC4)
  return(df)
}

# Select attributes and translate them into English. For OViN only (>2014).
select_attributes_OViN <- function (df) {
  df <- df %>% 
    sjlabelled::remove_all_labels() %>% 
    tibble()    # without this, results will be cast as a list
  
  df <- df %>%
    select(OP,
           OPID,
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
           VertPC,
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
           #RVertStat,
           #RAankStat,
           
           FactorP
    ) %>%
    rename(agent_new = OP,
           agent_ID = OPID,
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
           #ride_start_train_station = RVertStat,
           #ride_arrival_train_station = RAankStat,
           
           p_value = FactorP
    )
  return(df)
}

# Select attributes and translate them into English. For OViN only (<2015).
select_attributes_OViN_2014 <- function (df) {
  df <- df %>% 
    sjlabelled::remove_all_labels() %>% 
    tibble()    # without this, results will be cast as a list
  
  df <- df %>%
    select(OP,
           OPID,
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
           VertPC,
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
    rename(agent_new = OP,
           agent_ID = OPID,
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

# Filter agents that live in a specific area, PC4 level
filter_hh_PC4 <- function(df, df_PC4) {
  df <- df[df$hh_PC4 %in% df_PC4,]
  return(df)
}

# Filter agents that live in a specific area, municipality level
filter_hh_municipality <- function(df, municipality_code) {
  # Filter municipality home
  df <- df[df$hh_municipality == municipality_code,]
  
  # Remove useless column
  df<- subset(df, select=-c(hh_municipality))
  return(df)
}

# Use the PC4 of the first day displacement (from home) as home address
home_municipality_to_PC4 <- function(df, df_PC4) {
  # filter agents in OViN that only start the day from home
  df <- filter_start_day_from_home(df)
  
  # find the starting point of the first move, and set it as home
  df$hh_PC4=NA
  df[df$disp_counter==1,]$hh_PC4 = as.character(df[df$disp_counter==1,]$disp_start_PC4)
  
  # apply attribute to all the rows of the agent
  df <- df %>%
    group_by(agent_ID) %>% 
    mutate(hh_PC4 = zoo::na.locf(hh_PC4, na.rm=FALSE))
  
  # Remove useless column
  df <- subset(df, select=-c(disp_counter))
  return(df)
}

# Filter agents that only start the day from home
filter_start_day_from_home <- function (df) {
  # Filter agents starting the day from home
  df <- df[df$disp_start_home==1,]
  
  # Remove useless column
  df <- subset(df, select=-c(disp_start_home))
  return(df)
}

# Format modal choices in English labels and simplify into groups. For ODiN only.
format_modal_choice_ODiN <- function (df){
  df <- df %>%
    mutate(disp_modal_choice = recode(disp_modal_choice,
                                      '1' = 'private motorised vehicle', # car
                                      '2' = 'train',
                                      '3' = 'bus',
                                      '4' = 'subway',
                                      '5' = 'tram',
                                      '6' = 'bike', # e-bike
                                      '7' = 'bike',
                                      '8' = 'bike',
                                      '9' = 'foot',
                                      '10' = 'bus',
                                      '11' = 'private motorised vehicle', # delivery van
                                      '12' = 'private motorised vehicle', # truck
                                      '13' = 'private motorised vehicle', # motorhome
                                      '14' = 'private motorised vehicle', # taxi
                                      '15' = 'private motorised vehicle', # agricultural vehicle
                                      '16' = 'private motorised vehicle', # motorbike
                                      '17' = 'private motorised vehicle', # moped
                                      '18' = 'private motorised vehicle', # moped
                                      '19' = 'private motorised vehicle', # disabled vehicle with motor
                                      '20' = 'bike', # disabled vehicle without motor
                                      '21' = 'foot', # skates
                                      '22' = 'boat',
                                      '23' = 'other')) %>%
    mutate(ride_modal_choice = recode(ride_modal_choice,
                                      '1' = 'private motorised vehicle', # car
                                      '2' = 'train',
                                      '3' = 'bus',
                                      '4' = 'subway',
                                      '5' = 'tram',
                                      '6' = 'bike', # e-bike
                                      '7' = 'bike',
                                      '8' = 'bike',
                                      '9' = 'foot',
                                      '10' = 'bus',
                                      '11' = 'private motorised vehicle', # delivery van
                                      '12' = 'private motorised vehicle', # truck
                                      '13' = 'private motorised vehicle', # motorhome
                                      '14' = 'private motorised vehicle', # taxi
                                      '15' = 'private motorised vehicle', # agricultural vehicle
                                      '16' = 'private motorised vehicle', # motorbike
                                      '17' = 'private motorised vehicle', # moped
                                      '18' = 'private motorised vehicle', # moped
                                      '19' = 'private motorised vehicle', # disabled vehicle with motor
                                      '20' = 'bike', # disabled vehicle without motor
                                      '21' = 'foot', # skates
                                      '22' = 'boat',
                                      '23' = 'other'))
  df[is.na(df$disp_modal_choice) & is.na(df$disp_id),]$disp_modal_choice <- 'No move'
  df[is.na(df$ride_modal_choice) & is.na(df$ride_id),]$ride_modal_choice <- 'No move'
  
  return(df)
}

# Format modal choices in English labels and simplify into groups. For OViN only.
format_modal_choice_OViN <- function (df){
  df <- df %>%
    mutate(disp_modal_choice = recode(disp_modal_choice,
                                      '1' = 'train',
                                      '2' = 'private motorised vehicle', # bus (private)
                                      '3' = 'subway',
                                      '4' = 'tram',
                                      '5' = 'bus',
                                      '6' = 'private motorised vehicle', # car
                                      '7' = 'private motorised vehicle', # delivery van
                                      '8' = 'private motorised vehicle', # truck
                                      '9' = 'private motorised vehicle', # motorhome
                                      '10' = 'private motorised vehicle', # car
                                      '11' = 'private motorised vehicle', # taxi
                                      '12' = 'private motorised vehicle', # motorbike
                                      '13' = 'private motorised vehicle', # moped
                                      '14' = 'private motorised vehicle', # moped
                                      '15' = 'bike',
                                      '16' = 'bike',
                                      '17' = 'private motorised vehicle', # agricultural vehicle
                                      '18' = 'boat',
                                      '19' = 'plane',
                                      '20' = 'foot', # skates
                                      '21' = 'private motorised vehicle', # disabled vehicle
                                      '22' = 'foot',
                                      '23' = 'foot', # pram
                                      '24' = 'other')) %>%
    mutate(ride_modal_choice = recode(ride_modal_choice,
                                      '1' = 'train',
                                      '2' = 'private motorised vehicle', # bus (private)
                                      '3' = 'subway',
                                      '4' = 'tram',
                                      '5' = 'bus',
                                      '6' = 'private motorised vehicle', # car
                                      '7' = 'private motorised vehicle', # delivery van
                                      '8' = 'private motorised vehicle', # truck
                                      '9' = 'private motorised vehicle', # motorhome
                                      '10' = 'private motorised vehicle', # car
                                      '11' = 'private motorised vehicle', # taxi
                                      '12' = 'private motorised vehicle', # motorbike
                                      '13' = 'private motorised vehicle', # moped
                                      '14' = 'private motorised vehicle', # moped
                                      '15' = 'bike',
                                      '16' = 'bike',
                                      '17' = 'private motorised vehicle', # agricultural vehicle
                                      '18' = 'boat',
                                      '19' = 'plane',
                                      '20' = 'foot', # skates
                                      '21' = 'private motorised vehicle', # disabled vehicle
                                      '22' = 'foot',
                                      '23' = 'foot', # pram
                                      '24' = 'other'))
  
  df[is.na(df$disp_modal_choice) & is.na(df$disp_id),]$disp_modal_choice <- 'No move'
  df[is.na(df$ride_modal_choice) & is.na(df$ride_id),]$ride_modal_choice <- 'No move'
  
  return(df)
}

# Format roles in rides to English labels. For ODiN only.
format_role_ODiN <- function (df){
  df$ride_role <- recode(df$ride_role,
                         '1' = 'driver',
                         '2' = 'passenger',
                         '3' = 'unknown',
                         '4' = 'no role')
  df[is.na(df$ride_role) & is.na(df$ride_id),]$ride_role <- 'No move'
  
  df$disp_role <- recode(df$disp_role,
                         '1' = 'driver',
                         '2' = 'passenger',
                         '3' = 'unknown',
                         '4' = 'no role')
  df[is.na(df$disp_role) & is.na(df$disp_id),]$disp_role <- 'No move'
  
  return(df)
}

# Format roles in rides to English labels. For OViN only.
format_role_OViN <- function (df){
  df$ride_role <- recode(df$ride_role,
                         '1' = 'driver',
                         '2' = 'passenger',
                         '3' = 'train',
                         '4' = 'bus/tram/metro',
                         '5' = 'moped',
                         '6' = 'bike',
                         '7' = 'foot',
                         '8' = 'other')
  df[is.na(df$ride_role) & is.na(df$ride_id),]$ride_role <- 'No move'
  
  df$disp_role <- recode(df$disp_role,
                         '0' = 'no role',
                         '1' = 'driver',
                         '2' = 'passenger',
                         '3' = 'both driver and passenger')
  df[is.na(df$disp_role) & is.na(df$disp_id),]$disp_role <- 'No move'
  
  return(df)
}

# Format the values of all the attributes in common between ODiN and OViN
format_values <- function(df) {
  df$agent_new <- recode(df$agent_new,
                      '0' =	'no',
                      '1' =	'yes')
  
  df$hh_position <- recode(df$hh_position,
                           '1' = 'single household',
                           '2' = 'couple',
                           '3' = 'couple + child(ren)',
                           '4' = 'couple + child(ren) + other(s)',
                           '5' = 'couple + other(s)',
                           '6' = 'single-parent household + child(ren)',
                           '7' = 'single-parent household + child(ren) + other(s)',
                           '8' = 'other household',
                           '9' = 'unknown')
  
  df$gender <- recode(df$gender,
                      '1' =	'male',
                      '2' =	'female')
  
  df$migration_background <- recode(df$migration_background,
                                    '1' = 'Dutch',
                                    '2' = 'Western',
                                    '3' = 'Non_Western',
                                    '4' = 'unknown')
  
  df$has_driving_license <- recode(df$has_driving_license,
                                   '0' = 'no',
                                   '1' =	'yes',
                                   '3' =	'unknown')
  
  df$disp_reason <- recode(df$disp_reason,
                           '1' = 'to home',
                           '2' = 'to work',
                           '3' = 'business visit',
                           '4' = 'transport is the job',
                           '5' = 'pick up / bring people',
                           '6' = 'collection/delivery of goods',
                           '7' = 'follow education',
                           '8' = 'shopping',
                           '9' = 'visit/stay',
                           '10' =	'hiking',
                           '11' =	'sports/hobby',
                           '12' =	'other leisure activities',
                           '13' =	'services/personal care',
                           '14' =	'other')
  df[is.na(df$disp_reason) & is.na(df$disp_id),]$disp_reason <- 'No move'
  
  return(df)
}

get_n_agents <- function(df) {
  return(length(unique(df$agent_ID)))
}

get_n_displacements <- function(df) {
  return(length(unique(df$disp_id)))
}