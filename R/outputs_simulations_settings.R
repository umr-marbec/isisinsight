outputs_simulations_settings <- function(directory_path) {
  #directory_path <- "N:/projet_theme/isis_fish/data/ExportsSimulationsScenariosMACCO"
  #directory_path <- "D:/projet_theme/isis_fish/data/ExportsSimulationsScenariosMACCO"
  # 1 - Global variable assignment ----

  # 2 - Global argument check ----
  if (missing(x = directory_path)
      || ! inherits(x = directory_path,
                    what = "character")
      || length(x = directory_path) != 1) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Error, invalid \"directory_path\" argument.")
  }
  # 3 - Global process ----
  simulations_directory <- list.dirs(path = directory_path,
                                     full.names = FALSE,
                                     recursive = FALSE)
  if (length(x = simulations_directory) == 0) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Error, no input simulation available in the directory path.")
  }
  simulations_data <- vector(mode = "list")
  for (simulation_directory_id in seq_len(length.out = length(x = simulations_directory))) {
    simulation_directory_name <- simulations_directory[1]
    current_simulation_metadata <- list("project_name" = stringr::str_extract(string = simulation_directory_name,
                                                                              pattern =  "(?<=sim_)[[:upper:]]+(?=_)"),
                                        "simulation_configuration" = stringr::str_match(simulation_directory_name,
                                                                                        "^(?:[^_]+_){2}((?:[^_]+_)*?[^_]+)(?=_[0-9]{4})")[,2],
                                        "simulation_annual_range" = stringr::str_replace(string = stringr::str_extract(string = simulation_directory_name,
                                                                                                                       pattern =  "[[:digit:]]{4}_[[:digit:]]{4}"),
                                                                                         pattern = "_",
                                                                                         replacement = " to "),
                                        "simulation_date_time" = lubridate::ymd_hm(paste(stringr::str_extract(string = simulation_directory_name,
                                                                                                              pattern =  "[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}(?=-)"),
                                                                                         stringr::str_extract(string = simulation_directory_name,
                                                                                                              pattern =  "[[:digit:]]{2}-[[:digit:]]{2}$"))))
    current_directory_path <- file.path(directory_path,
                                        simulation_directory_name,
                                        "resultExports")
    if (dir.exists(paths = current_directory_path)
        && length(x = list.files(path = current_directory_path)) != 0
        && any(stringr::str_detect(string = list.files(path = current_directory_path),
                                   pattern =  ".csv$"))) {
      current_directory_path_files <- list.files(path = current_directory_path)
      current_directory_path_files_csv <- current_directory_path_files[stringr::str_which(string = current_directory_path_files,
                                                                                          pattern = ".csv$")]




    } else {
      current_data <- list("data" = NULL)
    }

  }
}
