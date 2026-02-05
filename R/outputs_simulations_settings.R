#' @title ISIS-Fish outputs simulations formatting
#' @description
#' Function for ISIS-Fish outputs simulations formatting and manipulation.
#' @param directory_path Mandatory. Class character expected. Directory path of the ISIS-Fish outputs simulations.
#' @return The function returns a list with a length in relation to the number of simulation directory provided. Each element of the list has information about metadata and data (original and improved) associated with the simulation.
#' @export
#' @examples
#' #replace the value of directory_path by a correct path
#' try(outputs_simulations_settings(directory_path = "my/path/to/simulations/directory"))
#'
outputs_simulations_settings <- function(directory_path) {
  # 1 - Global variable assignment ----
  population <- NULL
  scenario_name <- NULL
  step <- NULL
  value <- NULL
  year <- NULL
  zone_population <- NULL
  simulation_name <- NULL
  step_quarter <- NULL
  fleet <- NULL
  catch <- NULL
  # Empty for now
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
  simulation_final <- list()
  scenarios_names_referential <- readr::read_delim(file = system.file("extdata",
                                                                      "scenarios_names_referential.txt",
                                                                      package = "isisinsight"),
                                                   show_col_types = FALSE)
  for (simulation_directory_id in seq_len(length.out = length(x = simulations_directory))) {
    simulation_directory_name <- simulations_directory[simulation_directory_id]
    message(format(x = Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            " - Start data import from simulation directory ",
            simulation_directory_name)
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
                                                                                                              pattern =  "[[:digit:]]{2}-[[:digit:]]{2}$"))),
                                        "scenario_name" = (dplyr::filter(.data = scenarios_names_referential,
                                                                         simulation_name == stringr::str_match(simulation_directory_name,
                                                                                                               "^(?:[^_]+_){3}([^_]+)(?=_)")[,2]) %>%
                                                             dplyr::pull(scenario_name)))
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
      current_simulation_data_ori <- list()
      current_simulation_data_improved <- list()
      for (current_directory_path_files_csv_id in seq_len(length.out = length(x = current_directory_path_files_csv))) {
        if (stringr::str_replace(string = current_directory_path_files_csv[current_directory_path_files_csv_id],
                                 pattern = ".csv",
                                 replacement = "_referential.txt") %in% list.files(path = system.file("extdata",
                                                                                                      package = "isisinsight"))) {
          current_referential <- readr::read_delim(file = system.file("extdata",
                                                                      stringr::str_replace(string = current_directory_path_files_csv[current_directory_path_files_csv_id],
                                                                                           pattern = ".csv",
                                                                                           replacement = "_referential.txt"),
                                                                      package = "isisinsight"),
                                                   show_col_types = FALSE)
          current_csv_data  <- list(readr::read_delim(file = file.path(current_directory_path,
                                                                       current_directory_path_files_csv[current_directory_path_files_csv_id]),
                                                      col_names = current_referential$colname,
                                                      delim = ";",
                                                      col_types = paste0(current_referential$type,
                                                                         collapse = ""),
                                                      locale = readr::locale(decimal_mark = ".")))
          names(x = current_csv_data) <- stringr::str_match(string = current_directory_path_files_csv[current_directory_path_files_csv_id],
                                                            pattern = "(.+)\\.csv$")[, 2]
          current_simulation_data_ori <- c(current_simulation_data_ori,
                                           current_csv_data)
          current_simulation_data_improved <- c(current_simulation_data_improved,
                                                list(NULL))
          names(current_simulation_data_improved)[length(x = current_simulation_data_improved)] <- names(x = current_csv_data)
          if (current_directory_path_files_csv[current_directory_path_files_csv_id] == "BiomasseBeginMonthFeconde.csv") {
            simulation_biomass_fertile <- list(dplyr::filter(.data = current_csv_data[[1]],
                                                             step %% 12 == 0
                                                             & ! (zone_population %in% c("Zone_CelticSea",
                                                                                         "Zone_NorthernArea"))) %>%
                                                 dplyr::mutate(year = step / 12 + as.integer(x = stringr::str_extract(string = current_simulation_metadata$simulation_annual_range,
                                                                                                                      pattern = "^[[:digit:]]+")),
                                                               population = dplyr::case_when(
                                                                 population == "Lophius_piscatorius" ~ "Lophius",
                                                                 .default = population
                                                               )) %>%
                                                 dplyr::summarise(biomass = sum(value),
                                                                  .by = c(year,
                                                                          population)) %>%
                                                 dplyr::mutate(scenario_name = !!current_simulation_metadata$scenario_name))
            names(simulation_biomass_fertile) <- "simulation_biomass_fertile"
            current_simulation_data_improved$BiomasseBeginMonthFeconde <- c(current_simulation_data_improved$BiomasseBeginMonthFeconde,
                                                                            simulation_biomass_fertile)
          }
          if (current_directory_path_files_csv[current_directory_path_files_csv_id] == "AbondanceBeginMonth_Gpe_Janvier.csv") {
            simulation_abundance_gpe_january <-  list(dplyr::mutate(.data = current_csv_data[[1]],
                                                                    population = dplyr::case_when(
                                                                      population == "Lophius_piscatorius" ~ "Lophius",
                                                                      .default = population),
                                                                    scenario_name = !!current_simulation_metadata$scenario_name))
            names(simulation_abundance_gpe_january) <- "simulation_abundance_gpe_january"
            simulation_abundance_january <- list(dplyr::summarise(.data = current_csv_data[[1]],
                                                                  abundance = sum(value),
                                                                  .by = c(population,
                                                                          year)) %>%
                                                   dplyr::mutate(population = dplyr::case_when(
                                                     population == "Lophius_piscatorius" ~ "Lophius",
                                                     .default = population),
                                                     scenario_name = !!current_simulation_metadata$scenario_name))
            names(simulation_abundance_january) <- "simulation_abundance_january"
            current_simulation_data_improved$AbondanceBeginMonth_Gpe_Janvier <- c(current_simulation_data_improved$AbondanceBeginMonth_Gpe_Janvier,
                                                                                  simulation_abundance_gpe_january,
                                                                                  simulation_abundance_january)
          }
          if (current_directory_path_files_csv[current_directory_path_files_csv_id] == "CatchWeightStrMetierQuarter.csv") {
            simulation_catch_weight_fleet <- list(dplyr::mutate(.data = current_csv_data[[1]],
                                                                step_quarter = step_quarter + 1,
                                                                scenario_name = !!current_simulation_metadata$scenario_name) %>%
                                                    dplyr::summarise(catch = sum(value),
                                                                     .by = c(step_quarter,
                                                                             population,
                                                                             fleet,
                                                                             scenario_name)) %>%
                                                    dplyr::relocate(step_quarter,
                                                                    .before = catch))
            names(simulation_catch_weight_fleet) <- "simulation_catch_weight_fleet"
            current_simulation_data_improved$CatchWeightStrMetierQuarter <- c(current_simulation_data_improved$CatchWeightStrMetierQuarter,
                                                                              simulation_catch_weight_fleet)
          }
          if (current_directory_path_files_csv[current_directory_path_files_csv_id] == "CatchWeightPopStrMetStep.csv") {
            browser()

            simulation_catch_weight_fleet_month <- dplyr::rename(.data = current_csv_data[[1]],
                                                                 catch = value) %>%
              dplyr::mutate(step = step + 1,
                            scenario_name = !!current_simulation_metadata$scenario_name,
                            population = dplyr::case_when(
                              population == "Lophius_piscatorius" ~ "Lophius",
                              .default = population)) %>%
              dplyr::relocate(scenario_name,
                              .before = catch)


            # sim_catch_pds_fleet_mois_cum <- file.path(dossier_simul, simPath1[s],"resultExports/CatchWeightPopStrMetStep.csv") %>%
            #   fread(data.table = FALSE,col.names = catchmois_cols) %>%
            #   rename(catch=value) %>%
            #   mutate(
            #     year = as.integer(step / 12 + start_year)
            #   ) %>%
            #   group_by(pop,fleet,year) %>%
            #   arrange(step) %>%
            #   mutate(catchcum=cumsum(catch)) %>%
            #   ungroup() %>%
            #   mutate(step = step+1,
            #          sc_name = sc_newname_str[s],
            #          pop=recode(pop,Lophius_piscatorius ="Lophius")) %>%
            #   select(-c(year,catch))
            #
            # sim_catch_pds_mois_cum <- file.path(dossier_simul, simPath1[s],"resultExports/CatchWeightPopStrMetStep.csv") %>%
            #   fread(data.table = FALSE,col.names = catchmois_cols) %>%
            #   rename(catch=value) %>%
            #   mutate(
            #     year = as.integer(step / 12 + start_year)
            #   ) %>%
            #   group_by(pop,year,step) %>% #somme sur les flottilles
            #   mutate(catchtot=sum(catch)) %>%
            #   select(pop,year,step,catchtot) %>%
            #   distinct() %>%
            #   ungroup() %>%
            #   group_by(pop,year) %>%
            #   arrange(step) %>%
            #   mutate(catchcum=cumsum(catchtot)) %>%
            #   ungroup() %>%
            #   mutate(step = step+1,sc_name = sc_newname_str[s],
            #          pop=recode(pop,Lophius_piscatorius ="Lophius")) %>%
            #   select(-c(year,catchtot))
          }
        } else {
          warning(format(x = Sys.time(),
                         "%Y-%m-%d %H:%M:%S"),
                  " - Warning, no referential available for \"",
                  current_directory_path_files_csv[current_directory_path_files_csv_id],
                  "\" of the simulation directory name \"",
                  simulation_directory_name,
                  "\". Input data avoided.",
                  immediate. = TRUE)
          current_csv_data <- list(NULL)
          names(x = current_csv_data) <- stringr::str_match(string = current_directory_path_files_csv[current_directory_path_files_csv_id],
                                                            pattern = "(.+)\\.csv$")[, 2]
          current_simulation_data_ori <- c(current_simulation_data_ori,
                                           current_csv_data)
        }
      }
      current_simulation_data <- list("data_ori" = current_simulation_data_ori,
                                      "data_improved" = current_simulation_data_improved)
    } else {
      current_simulation_data <- list("data_ori" = NULL,
                                      "data_improved" = NULL)
    }
    current_simulation_final <- list(list("simulation_metadata" = current_simulation_metadata,
                                          "data" = current_simulation_data))
    names(x = current_simulation_final) <- simulation_directory_name
    simulation_final <- c(simulation_final,
                          current_simulation_final)
    message(format(x = Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            " - Successful data import from simulation directory ",
            simulation_directory_name)
  }
  return(simulation_final)
}
