library(dplyr)

folder_path <- ('../../merged_data2')
files <- list.files(folder_path)

vars <- list()

for (file in files) {
  file_path <- paste(folder_path, file, sep = "/")
  
  file_name <- tools::file_path_sans_ext(basename(file))

  file_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  file_data <- file_data %>% 
    mutate(id = file_name)
  
  assign(file_name, file_data)
  
}

# Funkcja do obliczania odległości między dwoma punktami na sferze (haversine formula)
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  # Przeliczanie na radiany
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  
  # Różnica między szerokościami i długościami geograficznymi
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  
  # Obliczanie odległości za pomocą formuły haversine
  a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6371 # Średnica Ziemi w kilometrach
  distance <- R * c
  
  return(distance) # Odległość w kilometrach
}

summarise_activity <- function(df) {
  df <- df %>% 
    mutate(Gyroscope = sqrt(Gyroscope_x^2 + Gyroscope_y^2 + Gyroscope_z^2)) %>%
    mutate(Magnetometer = sqrt(Magnetometer_x^2 + Magnetometer_y^2 + Magnetometer_z^2)) %>%
    mutate(TotalAcceleration = sqrt(TotalAcceleration_x^2 + TotalAcceleration_y^2 + TotalAcceleration_z^2)) %>% 
    select(id, time, Gyroscope, Location_speed, Orientation_roll, Orientation_pitch, Orientation_yaw, Pedometer_steps, Magnetometer, TotalAcceleration, Location_longitude, Location_latitude, Location_altitude, Location_bearing)
  
  df_tmp <- data.frame(
    id = df %>% select(id) %>% unique(),
    total_time = (max(df$time) - min(df$time)) / 10 ^ 6,

    mean_speed = (mean(df$Location_speed) * 3.6),
    max_speed = max(df$Location_speed) * 3.6,
    min_speed = min(df$Location_speed) * 3.6,

    total_distance = 0,

    mean_acceleration = mean(df$TotalAcceleration),
    max_acceleration = max(df$TotalAcceleration),
    min_acceleration = min(df$TotalAcceleration),
    sd_acceleration = sd(df$TotalAcceleration),

    mean_gyroscope = mean(df$Gyroscope),

    mean_magnetometer = mean(df$Magnetometer),

    steps_per_minute = max(df$Pedometer_steps) / (total_time / 60),
    total_steps = max(df$Pedometer_steps),

    average_roll = mean(df$Orientation_roll),
    median_roll = median(df$Orientation_roll),
    min_roll = min(df$Orientation_roll),
    max_roll = max(df$Orientation_roll),
    sd_roll = sd(df$Orientation_roll),

    average_pitch = mean(df$Orientation_pitch, na.rm = TRUE),
    median_pitch = median(df$Orientation_pitch, na.rm = TRUE),
    min_pitch = min(df$Orientation_pitch, na.rm = TRUE),
    max_pitch = max(df$Orientation_pitch, na.rm = TRUE),
    sd_pitch = sd(df$Orientation_pitch, na.rm = TRUE),
    
    average_yaw = mean(df$Orientation_yaw),
    median_yaw = median(df$Orientation_yaw),
    min_yaw = min(df$Orientation_yaw),
    max_yaw = max(df$Orientation_yaw),
    sd_yaw = sd(df$Orientation_yaw)
    )
  
  total_distance1 <- 0
  
  for (i in 1:(nrow(df) - 1)) {
    distance <- haversine_distance(
      df$Location_latitude[i], df$Location_longitude[i],
      df$Location_latitude[i + 1], df$Location_longitude[i + 1]
    )
    total_distance1 <- total_distance1 + distance
  }
  
  df_tmp$total_distance <- total_distance1
  df_tmp
}

result <- summarise_activity(Cycling_1) %>% 
  rbind(summarise_activity(Cycling_2)) %>%
  rbind(summarise_activity(Cycling_3)) %>%
  rbind(summarise_activity(Cycling_4)) %>%
  rbind(summarise_activity(Cycling_5)) %>%
  rbind(summarise_activity(Cycling_6)) %>%
  rbind(summarise_activity(Cycling_7)) %>%
  rbind(summarise_activity(Sitting_8)) %>%
  rbind(summarise_activity(Sitting_9)) %>%
  rbind(summarise_activity(Sitting_10)) %>%
  rbind(summarise_activity(Walking_11)) %>%
  rbind(summarise_activity(Walking_12))


df <- df %>% 
  mutate(Gyroscope = sqrt(Gyroscope_x^2 + Gyroscope_y^2 + Gyroscope_z^2)) %>%
  mutate(Magnetometer = sqrt(Magnetometer_x^2 + Magnetometer_y^2 + Magnetometer_z^2)) %>%
  mutate(TotalAcceleration = sqrt(TotalAcceleration_x^2 + TotalAcceleration_y^2 + TotalAcceleration_z^2)) %>% 
  select(id, time, Gyroscope, Location_speed, Orientation_roll, Orientation_pitch, Orientation_yaw, Pedometer_steps, Magnetometer, TotalAcceleration, id, Location_longitude, Location_latitude, Location_altitude, Location_bearing)






