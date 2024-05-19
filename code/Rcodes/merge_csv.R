library(dplyr)

folder_path <- ('../../cleared_data')
folders <- list.files(folder_path)

for (folder in folders){
  one_activity_path <- paste(folder_path, folder, sep = "/")
  print(paste0("Files in ", folder, ":"))
  print(list.files(one_activity_path))
  cat("\n")
}


i <- 1
for (dir in folders) {
  activity <- strsplit(dir, "-")[[1]][1]
  
  folder <- paste(folder_path, dir, sep = "/")
  
  file_list <- list.files(path = folder, pattern = "\\.csv$", full.names = TRUE)
  
  result <- data.frame(time = numeric())
  
  for (file_path in file_list) {
    file <- tools::file_path_sans_ext(basename(file_path))
    if (file == "Annotation" | file == "Metadata") {
      next
    }
    file_name <- paste0(file, "_", i)
    
    file_data <- read.csv(file_path, stringsAsFactors = FALSE)
    
    file_data <- file_data %>% 
      rename_with(~ ifelse(. != "time", paste0(file, "_", .), .), 
                  .cols = -matches("^time$"))
    
    result <- result %>% 
      full_join(file_data, by = "time")
    
    # assign(file_name, file_data)
  }
  
  write.csv(result, file = paste0('../../merged_data/', activity, '_',i, '.csv'), row.names = FALSE)
  i <- i + 1
}