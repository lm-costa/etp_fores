##buit in fun

source("r/corr_fun.R")

##
files_names <- list.files("data-raw/cfsr/", pattern = "weather")

correction(files_names)

files_names <- list.files("data-raw/cfsr_new/", pattern = "weather")
db <- purrr::map_df(files_names,geral_data)

write.csv(db, "data-raw/database.csv")

