#' @title ISIS-Fish outputs simulations formatting
#' @description
#' Function for ISIS-Fish outputs simulations formatting and manipulation.
#' @param directory_path Mandatory. Class character expected. Directory path of the ISIS-Fish outputs simulations.
#' @param output_path Optional. Default NULL. Output path for saved data from function element "simulations_data_improved_merged". If the value is NULL, nothing will be exported.
#' @return The function returns a list with a length in relation to the number of simulation directory provided. Each element of the list has information about metadata and data (original and improved) associated with the simulation.
#' @export
#' @examples
#' #replace the value of directory_path by a correct path
#' try(outputs_simulations_settings(directory_path = "my/path/to/simulations/directory"))
#'
outputs_simulations_settings <- function(directory_path,
                                         output_path = NULL) {
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
  catch_total <- NULL
  fish_group <- NULL
  group <- NULL
  fishing_mortality <- NULL
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
  if (! is.null(x = output_path)
      && (! inherits(x = output_path,
                     what = "character")
          || length(x = output_path) != 1)) {
    stop(format(x = Sys.time(),
                "%Y-%m-%d %H:%M:%S"),
         " - Error, invalid \"output_path\" argument.")
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
  simulation_final <- vector("list",
                             2)
  names(simulation_final) <- c("simulations",
                               "simulations_data_improved_merged")
  scenarios_names_referential <- readr::read_delim(file = system.file("extdata",
                                                                      "scenarios_names_referential.txt",
                                                                      package = "isisinsight"),
                                                   show_col_types = FALSE)
  param_gestion_isis <- readr::read_delim(file = system.file("extdata",
                                                             "param_gestion_isis.txt",
                                                             package = "isisinsight"),
                                          delim = ";",
                                          col_types = "cdddddddd",
                                          col_names = TRUE)
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
            simulation_catch_weight_fleet_month <- list(dplyr::rename(.data = current_csv_data[[1]],
                                                                      catch = value) %>%
                                                          dplyr::mutate(step = step + 1,
                                                                        scenario_name = !!current_simulation_metadata$scenario_name,
                                                                        population = dplyr::case_when(
                                                                          population == "Lophius_piscatorius" ~ "Lophius",
                                                                          .default = population)) %>%
                                                          dplyr::relocate(scenario_name,
                                                                          .before = catch))
            names(simulation_catch_weight_fleet_month) <- "simulation_catch_weight_fleet_month"
            simulation_catch_weight_fleet_month_cumulative_sums <- list(dplyr::mutate(.data = simulation_catch_weight_fleet_month[[1]],
                                                                                      year = as.integer(x = (step - 1) / 12 + as.integer(x = stringr::str_extract(string = current_simulation_metadata$simulation_annual_range,
                                                                                                                                                                  pattern = "^[[:digit:]]+")))) %>%
                                                                          dplyr::group_by(population,
                                                                                          fleet,
                                                                                          year) %>%
                                                                          dplyr::arrange(step) %>%
                                                                          dplyr::mutate(catch_cumulative = cumsum(x = catch)) %>%
                                                                          dplyr::ungroup() %>%
                                                                          dplyr::select(-c(year,
                                                                                           catch)))
            names(simulation_catch_weight_fleet_month_cumulative_sums) <- "simulation_catch_weight_fleet_month_cumulative_sums"
            simulation_catch_weight_month_cumulative_sums <- list(dplyr::mutate(.data = simulation_catch_weight_fleet_month[[1]],
                                                                                year = as.integer(x = (step - 1) / 12 + as.integer(x = stringr::str_extract(string = current_simulation_metadata$simulation_annual_range,
                                                                                                                                                            pattern = "^[[:digit:]]+")))) %>%
                                                                    dplyr::group_by(population,
                                                                                    year,
                                                                                    step) %>%
                                                                    dplyr::mutate(catch_total = sum(catch)) %>%
                                                                    dplyr::ungroup() %>%
                                                                    dplyr::select(population,
                                                                                  year,
                                                                                  step,
                                                                                  catch_total) %>%
                                                                    dplyr::distinct() %>%
                                                                    dplyr::group_by(population,
                                                                                    year) %>%
                                                                    dplyr::arrange(step) %>%
                                                                    dplyr::mutate(catch_cumulative = cumsum(x = catch_total)) %>%
                                                                    dplyr::ungroup() %>%
                                                                    dplyr::select(-c(year,
                                                                                     catch_total)))
            names(simulation_catch_weight_month_cumulative_sums) <- "simulation_catch_weight_month_cumulative_sums"
            current_simulation_data_improved$CatchWeightPopStrMetStep <- c(current_simulation_data_improved$CatchWeightPopStrMetStep,
                                                                           simulation_catch_weight_fleet_month,
                                                                           simulation_catch_weight_fleet_month_cumulative_sums,
                                                                           simulation_catch_weight_month_cumulative_sums)
          }
          if (current_directory_path_files_csv[current_directory_path_files_csv_id] == "EffortsNominalMetier.csv") {
            simulation_effort_nominal_metier_start_year_month <- list(dplyr::mutate(.data = current_csv_data[[1]],
                                                                                    year = as.integer(step / 12 + as.integer(x = stringr::str_extract(string = current_simulation_metadata$simulation_annual_range,
                                                                                                                                                      pattern = "^[[:digit:]]+"))),
                                                                                    step = step + 1,
                                                                                    scenario_name = !!current_simulation_metadata$scenario_name) %>%
                                                                        dplyr::filter(year == as.integer(x = stringr::str_extract(string = current_simulation_metadata$simulation_annual_range,
                                                                                                                                  pattern = "^[[:digit:]]+"))))
            names(simulation_effort_nominal_metier_start_year_month) <- "simulation_effort_nominal_metier_start_year_month"
            current_simulation_data_improved$EffortsNominalMetier <- c(current_simulation_data_improved$EffortsNominalMetier,
                                                                       simulation_effort_nominal_metier_start_year_month)
          }
          if (current_directory_path_files_csv[current_directory_path_files_csv_id] == "MortalitePecheGroupe.csv") {
            simulation_fishing_mortality_group <- list(dplyr::mutate(.data = current_csv_data[[1]],
                                                                     year = as.integer(step / 12 + as.integer(x = stringr::str_extract(string = current_simulation_metadata$simulation_annual_range,
                                                                                                                                       pattern = "^[[:digit:]]+"))),
                                                                     scenario_name = !!current_simulation_metadata$scenario_name,
                                                                     population = dplyr::case_when(
                                                                       population == "Lophius_piscatorius" ~ "Lophius",
                                                                       .default = population)) %>%
                                                         dplyr::rename(fishing_mortality = value,
                                                                       group = fish_group) %>%
                                                         dplyr::select(year,
                                                                       population,
                                                                       group,
                                                                       fishing_mortality,
                                                                       scenario_name))
            names(simulation_fishing_mortality_group) <- "simulation_fishing_mortality_group"
            current_simulation_data_improved$MortalitePecheGroupe <- c(current_simulation_data_improved$MortalitePecheGroupe,
                                                                       simulation_fishing_mortality_group)
          }
          if (current_directory_path_files_csv[current_directory_path_files_csv_id] == "MortalitePecheTotale.csv") {
            simulation_fishing_mortality_total <- list(dplyr::mutate(.data = current_csv_data[[1]],
                                                                     year = as.integer(step / 12 + as.integer(x = stringr::str_extract(string = current_simulation_metadata$simulation_annual_range,
                                                                                                                                       pattern = "^[[:digit:]]+"))),
                                                                     scenario_name = !!current_simulation_metadata$scenario_name,
                                                                     population = dplyr::case_when(
                                                                       population == "Lophius_piscatorius" ~ "Lophius",
                                                                       .default = population)) %>%
                                                         dplyr::rename(fishing_mortality = value) %>%
                                                         dplyr::select(year,
                                                                       population,
                                                                       fishing_mortality,
                                                                       scenario_name))
            names(simulation_fishing_mortality_total) <- "simulation_fishing_mortality_total"
            current_simulation_data_improved$MortalitePecheTotale <- c(current_simulation_data_improved$MortalitePecheTotale,
                                                                       simulation_fishing_mortality_total)
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
    simulation_final$simulations <- c(simulation_final$simulations,
                                      current_simulation_final)
    if (length(x = current_simulation_final[[1]]$data$data_improved) != 0) {
      if (length(x = simulation_final$simulations_data_improved_merged) == 0) {
        simulation_final$simulations_data_improved_merged <- current_simulation_final[[1]]$data$data_improved
      } else {
        for (id_date_improved in seq_len(length.out = length(x = current_simulation_final[[1]]$data$data_improved))) {
          if (names(x = current_simulation_final[[1]]$data$data_improved)[id_date_improved] %in% names(x = simulation_final$simulations_data_improved_merged)) {
            current_id_simulations_data_improved_merged <- which(x = names(x = simulation_final$simulations_data_improved_merged) == names(x = current_simulation_final[[1]]$data$data_improved[id_date_improved]))
            for (current_id_date_improved in seq_len(length.out = length(x = current_simulation_final[[1]]$data$data_improved[[id_date_improved]]))) {
              if (names(x = current_simulation_final[[1]]$data$data_improved[[id_date_improved]][current_id_date_improved]) %in% names(x = simulation_final$simulations_data_improved_merged[[current_id_simulations_data_improved_merged]])) {
                current_sub_id_simulations_data_improved_merged <- which(x = names(x = simulation_final$simulations_data_improved_merged[[current_id_simulations_data_improved_merged]]) == names(x = current_simulation_final[[1]]$data$data_improved[[id_date_improved]][current_id_date_improved]))
                simulation_final$simulations_data_improved_merged[[current_id_simulations_data_improved_merged]][[current_sub_id_simulations_data_improved_merged]] <- dplyr::bind_rows(simulation_final$simulations_data_improved_merged[[current_id_simulations_data_improved_merged]][[current_sub_id_simulations_data_improved_merged]],
                                                                                                                                                                                        current_simulation_final[[1]]$data$data_improved[[id_date_improved]][[current_id_date_improved]])
              } else {
                simulation_final$simulations_data_improved_merged[[current_id_simulations_data_improved_merged]] <- c(simulation_final$simulations_data_improved_merged[[current_id_simulations_data_improved_merged]],
                                                                                                                      current_simulation_final[[1]]$data$data_improved[[id_date_improved]][current_id_date_improved])
              }
            }
          } else {
            simulation_final$simulations_data_improved_merged <- c(simulation_final$simulations_data_improved_merged,
                                                                   current_simulation_final[[1]]$data$data_improved[id_date_improved])
          }
        }
      }
    }
    message(format(x = Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            " - Successful data import from simulation directory ",
            simulation_directory_name)
  }
  if ("BiomasseBeginMonthFeconde" %in% names(x = simulation_final$simulations_data_improved_merged)) {
    if ("simulation_biomass_fertile" %in% names(x = simulation_final$simulations_data_improved_merged$BiomasseBeginMonthFeconde)) {
      simulation_biomass_fertile_final <- list(dplyr::left_join(x = simulation_final$simulations_data_improved_merged$BiomasseBeginMonthFeconde$simulation_biomass_fertile,
                                                                y = dplyr::select(.data = param_gestion_isis,
                                                                                  population,
                                                                                  b_trigger_isis),
                                                                by = "population"))
      names(simulation_biomass_fertile_final) <- "simulation_biomass_fertile_final"
      simulation_final$simulations_data_improved_merged$BiomasseBeginMonthFeconde <- c(simulation_final$simulations_data_improved_merged$BiomasseBeginMonthFeconde,
                                                                                       simulation_biomass_fertile_final)
    }
  }
  if ("MortalitePecheGroupe" %in% names(x = simulation_final$simulations_data_improved_merged)) {
    if ("simulation_fishing_mortality_group" %in% names(x = simulation_final$simulations_data_improved_merged$MortalitePecheGroupe)) {
      simulation_fishing_mortality_group_final <- list(dplyr::left_join(x = simulation_final$simulations_data_improved_merged$MortalitePecheGroupe$simulation_fishing_mortality_group,
                                                                        y = dplyr::select(.data = param_gestion_isis,
                                                                                          population,
                                                                                          f_msy_isis),
                                                                        by = "population"))
      names(simulation_fishing_mortality_group_final) <- "simulation_fishing_mortality_group_final"
      simulation_final$simulations_data_improved_merged$MortalitePecheGroupe <- c(simulation_final$simulations_data_improved_merged$MortalitePecheGroupe,
                                                                                  simulation_fishing_mortality_group_final)
    }
  }
  if ("MortalitePecheTotale" %in% names(x = simulation_final$simulations_data_improved_merged)) {
    if ("simulation_fishing_mortality_total" %in% names(x = simulation_final$simulations_data_improved_merged$MortalitePecheTotale)) {
      simulation_fishing_mortality_total_final <- list(dplyr::left_join(x = simulation_final$simulations_data_improved_merged$MortalitePecheTotale$simulation_fishing_mortality_total,
                                                                        y = dplyr::select(.data = param_gestion_isis,
                                                                                          population,
                                                                                          f_msy_isis),
                                                                        by = "population"))
      names(simulation_fishing_mortality_total_final) <- "simulation_fishing_mortality_total_final"
      simulation_final$simulations_data_improved_merged$MortalitePecheTotale <- c(simulation_final$simulations_data_improved_merged$MortalitePecheTotale,
                                                                                  simulation_fishing_mortality_total_final)
    }
  }
  if ("EffortsNominalMetier" %in% names(x = simulation_final$simulations_data_improved_merged)) {
    if ("simulation_effort_nominal_metier_start_year_month" %in% names(x = simulation_final$simulations_data_improved_merged$EffortsNominalMetier)) {
      simulation_effort_nominal_metier_start_year_month_total <- dplyr::group_by(.data = simulation_final$simulations_data_improved_merged$EffortsNominalMetier$simulation_effort_nominal_metier_start_year_month,
                                                                                 metier,
                                                                                 step,
                                                                                 scenario_name) %>%
        dplyr::mutate(effort_metier_month = sum(value)) %>%
        dplyr::ungroup() %>%
        dplyr::select(metier,
                      step,
                      effort_metier_month,
                      scenario_name) %>%
        dplyr::distinct()
      simulation_effort_nominal_metier_start_year_month_bau <- dplyr::filter(.data = simulation_effort_nominal_metier_start_year_month_total,
                                                                             scenario_name == "BAU") %>%
        dplyr::rename(effort_metier_month_bau = effort_metier_month) %>%
        dplyr::select(-scenario_name)
      simulation_effort_nominal_metier_start_year_month <- dplyr::filter(.data = simulation_effort_nominal_metier_start_year_month_total,
                                                                         scenario_name != "BAU")
      area_metier <- readr::read_delim(file = system.file("extdata",
                                                          "data_intersections.txt",
                                                          package = "isisinsight"),
                                       delim = ";",
                                       col_types = "cciii",
                                       col_names = TRUE) %>%
        dplyr::filter(area_management != "zbsi_penatules")
      strange_metier <- dplyr::select(.data = simulation_effort_nominal_metier_start_year_month_bau,
                                      metier) %>%
        dplyr::distinct() %>%
        dplyr::filter(stringr::str_detect(string = metier,
                                          pattern = "27.8")) %>%
        dplyr::mutate(area = stringr::str_extract(string = metier,
                                                  pattern = "(?<=27\\.).+"))
      area_metier_8ab <- dplyr::filter(.data = area_metier,
                                       metier %in% c("8.a",
                                                     "8.b")) %>%
        dplyr::rename(area = metier) %>%
        dplyr::right_join(strange_metier,
                          by = "area") %>%
        dplyr::select(-area)
      area_metier <- dplyr::filter(.data = area_metier,
                                   metier %in% c("8.a",
                                                 "8.b")) %>%
        dplyr::bind_rows(area_metier_8ab)
      area_month_openning_referential <- readr::read_delim(file = system.file("extdata",
                                                                              "area_month_openning_referential.txt",
                                                                              package = "isisinsight"),
                                                           col_types = "icic")
      area_month_openning <- dplyr::left_join(x = area_month_openning_referential,
                                              y = scenarios_names_referential,
                                              by = "simulation_name") %>%
        dplyr::select(-simulation_name)
      browser()
      metier_referential <- readLines(con = system.file("extdata",
                                                        "metier_referential.txt",
                                                        package = "isisinsight"))
      effort_management_diagnostic <- dplyr::left_join(x = simulation_effort_nominal_metier_start_year_month,
                                                       y = simulation_effort_nominal_metier_start_year_month_bau,
                                                       by = c("metier",
                                                              "step")) %>%
        dplyr::left_join(area_month_openning,
                         by = c("step",
                                "scenario_name")) %>%
        dplyr::left_join(area_metier,
                         by = c("metier",
                                "area_management")) %>%
        dplyr::rename(openning_metier = openning) %>%
        dplyr::mutate(openning_metier = dplyr::case_when(
          scenario_name == "O-D-3_miles"
          & (! metier %in% !!metier_referential)
          & stringr::str_detect(metier,
                                "0-10") ~ 1,
          .default = openning_metier
        ))
    }
  }
  if (! is.null(x = output_path)
      && length(x = simulation_final$simulations_data_improved_merged) != 0) {
    final_output_path <- file.path(output_path,
                                   paste0(format(x = Sys.time(),
                                                 "%Y%m%d_%H%M%S"),
                                          "_isisfish_simulations_data_improved"))
    dir.create(path = final_output_path)
    for (current_simulations_data_improved_merged_id in seq_len(length.out = length(x = simulation_final$simulations_data_improved_merged))) {
      for (current_simulations_data_improved_merged_sub_id in seq_len(length.out = length(x = simulation_final$simulations_data_improved_merged[[current_simulations_data_improved_merged_id]]))) {
        saveRDS(object = simulation_final$simulations_data_improved_merged[[current_simulations_data_improved_merged_id]][[current_simulations_data_improved_merged_sub_id]],
                file = file.path(final_output_path,
                                 paste0(names(x = simulation_final$simulations_data_improved_merged[[current_simulations_data_improved_merged_id]][current_simulations_data_improved_merged_sub_id]),
                                        ".rds")))
      }
    }
    message(format(x = Sys.time(),
                   "%Y-%m-%d %H:%M:%S"),
            " - Successful data export in the output directory \"",
            final_output_path,
            "\"")
  }
  return(simulation_final)
}
