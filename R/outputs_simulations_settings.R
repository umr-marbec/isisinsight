#' @title ISIS-Fish outputs simulations formatting
#' @description
#' Function for ISIS-Fish outputs simulations formatting and manipulation.
#' @param directory_path Mandatory. Class character expected. Directory path of the ISIS-Fish outputs simulations.
#' @param output_path Optional. Default NULL. Class character expected. Output path for saved data from function element "simulations_data_improved_merged". If the value is NULL, nothing will be exported.
#' @param output_format Mandatory. Default "rds". Class character expected. Output(s) format expected. You wan choose between .rds or .csv (with ";" for the delimiter).
#' @param input_colnames Mandatory. Default TRUE. Class logical expected. Does the input(s) file(s) contain colnames. You can use the next argument to define more precisely the input file(s) concern.
#' @param input_colnames_ids_names Optional. Default NULL. Class character expected. Related to the previous argument, this one defines which input contains colname. If NULL, all the input contains colname.
#' @return The function returns a list with a length in relation to the number of simulation directory provided. Each element of the list has information about metadata and data (original and improved) associated with the simulation.
#' @export
#' @importFrom rlang .data
#' @examples
#' #replace the value of directory_path by a correct path
#' try(outputs_simulations_settings(directory_path = "my/path/to/simulations/directory"))
#'
outputs_simulations_settings <- function(directory_path,
                                         output_path = NULL,
                                         output_format = "rds",
                                         input_colnames = TRUE,
                                         input_colnames_ids_names = NULL) {
  # 1 - Global argument check ----
  ## 1.1 - directory_path ----
  if (rlang::is_missing(directory_path)) {
    stop("The `directory_path` argument is required.")
  }
  checkmate::assert_character(x = directory_path,
                              len = 1)
  ## 1.2 - output_path ----
  if (rlang::is_missing(output_path)) {
    stop("The `output_path` argument is required.")
  }
  checkmate::assert_character(x = output_path,
                              len = 1,
                              null.ok = TRUE)
  ## 1.3 - output_format ----
  if (rlang::is_missing(output_format)) {
    stop("The `output_format` argument is required.")
  }
  checkmate::assert_choice(x = output_format,
                           choices = c("rds",
                                       "csv"))
  ## 1.4 - input_colnames ----
  if (rlang::is_missing(input_colnames)) {
    stop("The `input_colnames` argument is required.")
  }
  checkmate::assert_logical(x = input_colnames,
                            len = 1)
  ## 1.5 - input_colnames_ids_names ----
  if (rlang::is_missing(input_colnames_ids_names)) {
    stop("The `input_colnames_ids_names` argument is required.")
  }
  checkmate::assert_character(x = input_colnames_ids_names,
                              min.len = 1,
                              null.ok = TRUE,
                              unique = TRUE)
  # 2 - Global process ----
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
  parameters_gestion_isis <- readr::read_delim(file = system.file("extdata",
                                                                  "parameters_gestion_isis.txt",
                                                                  package = "isisinsight"),
                                               delim = ";",
                                               col_types = "cdddddddd",
                                               col_names = TRUE)
  parameters_gestion_isis_final <- parameters_gestion_isis %>%
    dplyr::mutate(population = dplyr::replace_when(x = .data$population,
                                                   .data$population == "Lophius_piscatorius" ~ "Lophius"))
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
                                                                         .data$simulation_name == stringr::str_match(simulation_directory_name,
                                                                                                                     "^(?:[^_]+_){3}([^_]+)(?=_)")[,2]) %>%
                                                             dplyr::pull(.data$scenario_name)))
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
          if (input_colnames == TRUE
              && (is.null(x = input_colnames_ids_names)
                  || stringr::str_remove(string = current_directory_path_files_csv[current_directory_path_files_csv_id],
                                         pattern = "\\.csv$") %in% input_colnames_ids_names)) {
            current_csv_data  <- list(readr::read_delim(file = file.path(current_directory_path,
                                                                         current_directory_path_files_csv[current_directory_path_files_csv_id]),
                                                        col_names = current_referential$colname,
                                                        delim = ";",
                                                        col_types = paste0(current_referential$type,
                                                                           collapse = ""),
                                                        locale = readr::locale(decimal_mark = "."),
                                                        skip = 1))

          } else {
            current_csv_data  <- list(readr::read_delim(file = file.path(current_directory_path,
                                                                         current_directory_path_files_csv[current_directory_path_files_csv_id]),
                                                        col_names = current_referential$colname,
                                                        delim = ";",
                                                        col_types = paste0(current_referential$type,
                                                                           collapse = ""),
                                                        locale = readr::locale(decimal_mark = ".")))
          }
          names(x = current_csv_data) <- stringr::str_match(string = current_directory_path_files_csv[current_directory_path_files_csv_id],
                                                            pattern = "(.+)\\.csv$")[, 2]
          current_simulation_data_ori <- c(current_simulation_data_ori,
                                           current_csv_data)
          current_simulation_data_improved <- c(current_simulation_data_improved,
                                                list(NULL))
          names(current_simulation_data_improved)[length(x = current_simulation_data_improved)] <- names(x = current_csv_data)
          if (current_directory_path_files_csv[current_directory_path_files_csv_id] == "BiomasseBeginMonthFeconde.csv") {
            simulation_biomass_spawning <- list(dplyr::filter(.data = current_csv_data[[1]],
                                                              .data$step %% 12 == 0
                                                              & ! (.data$zone_population %in% c("Zone_CelticSea",
                                                                                                "Zone_NorthernArea"))) %>%
                                                  dplyr::mutate(year = .data$step / 12 + as.integer(x = stringr::str_extract(string = current_simulation_metadata$simulation_annual_range,
                                                                                                                             pattern = "^[[:digit:]]+")),
                                                                population = dplyr::case_when(
                                                                  .data$population == "Lophius_piscatorius" ~ "Lophius",
                                                                  .default = .data$population
                                                                )) %>%
                                                  dplyr::summarise(biomass = sum(.data$value),
                                                                   .by = c(.data$year,
                                                                           .data$population)) %>%
                                                  dplyr::mutate(scenario_name = !!current_simulation_metadata$scenario_name))
            names(simulation_biomass_spawning) <- "simulation_biomass_spawning"
            current_simulation_data_improved$BiomasseBeginMonthFeconde <- c(current_simulation_data_improved$BiomasseBeginMonthFeconde,
                                                                            simulation_biomass_spawning)
          }
          if (current_directory_path_files_csv[current_directory_path_files_csv_id] == "AbondanceBeginMonth_Gpe_Janvier.csv") {
            simulation_abundance_gpe_january <-  list(dplyr::mutate(.data = current_csv_data[[1]],
                                                                    population = dplyr::case_when(
                                                                      .data$population == "Lophius_piscatorius" ~ "Lophius",
                                                                      .default = .data$population),
                                                                    scenario_name = !!current_simulation_metadata$scenario_name) %>%
                                                        dplyr::rename(abundance = .data$value))
            names(simulation_abundance_gpe_january) <- "simulation_abundance_gpe_january"
            simulation_abundance_january <- list(dplyr::summarise(.data = current_csv_data[[1]],
                                                                  abundance = sum(.data$value),
                                                                  .by = c(.data$population,
                                                                          .data$year)) %>%
                                                   dplyr::mutate(population = dplyr::case_when(
                                                     .data$population == "Lophius_piscatorius" ~ "Lophius",
                                                     .default = .data$population),
                                                     scenario_name = !!current_simulation_metadata$scenario_name))
            names(simulation_abundance_january) <- "simulation_abundance_january"
            current_simulation_data_improved$AbondanceBeginMonth_Gpe_Janvier <- c(current_simulation_data_improved$AbondanceBeginMonth_Gpe_Janvier,
                                                                                  simulation_abundance_gpe_january,
                                                                                  simulation_abundance_january)
          }
          if (current_directory_path_files_csv[current_directory_path_files_csv_id] == "CatchWeightStrMetierQuarter.csv") {
            simulation_catch_weight_fleet <- list(dplyr::mutate(.data = current_csv_data[[1]],
                                                                step_quarter = .data$step_quarter + 1,
                                                                scenario_name = !!current_simulation_metadata$scenario_name) %>%
                                                    dplyr::summarise(catch = sum(.data$value),
                                                                     .by = c(.data$step_quarter,
                                                                             .data$population,
                                                                             .data$fleet,
                                                                             .data$scenario_name)) %>%
                                                    dplyr::relocate(.data$step_quarter,
                                                                    .before = .data$catch))
            names(simulation_catch_weight_fleet) <- "simulation_catch_weight_fleet"
            current_simulation_data_improved$CatchWeightStrMetierQuarter <- c(current_simulation_data_improved$CatchWeightStrMetierQuarter,
                                                                              simulation_catch_weight_fleet)
          }
          if (current_directory_path_files_csv[current_directory_path_files_csv_id] == "CatchWeightPopStrMetStep.csv") {
            simulation_catch_weight_fleet_month <- list(dplyr::rename(.data = current_csv_data[[1]],
                                                                      catch = .data$value) %>%
                                                          dplyr::mutate(step = .data$step + 1,
                                                                        scenario_name = !!current_simulation_metadata$scenario_name,
                                                                        population = dplyr::case_when(
                                                                          .data$population == "Lophius_piscatorius" ~ "Lophius",
                                                                          .default = .data$population)) %>%
                                                          dplyr::relocate(.data$scenario_name,
                                                                          .before = .data$catch))
            names(simulation_catch_weight_fleet_month) <- "simulation_catch_weight_fleet_month"
            simulation_catch_weight_fleet_month_cumulative_sums <- list(dplyr::mutate(.data = simulation_catch_weight_fleet_month[[1]],
                                                                                      year = as.integer(x = (.data$step - 1) / 12 + as.integer(x = stringr::str_extract(string = current_simulation_metadata$simulation_annual_range,
                                                                                                                                                                        pattern = "^[[:digit:]]+")))) %>%
                                                                          dplyr::group_by(.data$population,
                                                                                          .data$fleet,
                                                                                          .data$year) %>%
                                                                          dplyr::arrange(.data$step) %>%
                                                                          dplyr::mutate(catch_cumulative = cumsum(x = .data$catch)) %>%
                                                                          dplyr::ungroup() %>%
                                                                          dplyr::select(-c(.data$year,
                                                                                           .data$catch)))
            names(simulation_catch_weight_fleet_month_cumulative_sums) <- "simulation_catch_weight_fleet_month_cumulative_sums"
            simulation_catch_weight_month_cumulative_sums <- list(dplyr::mutate(.data = simulation_catch_weight_fleet_month[[1]],
                                                                                year = as.integer(x = (.data$step - 1) / 12 + as.integer(x = stringr::str_extract(string = current_simulation_metadata$simulation_annual_range,
                                                                                                                                                                  pattern = "^[[:digit:]]+")))) %>%
                                                                    dplyr::group_by(.data$population,
                                                                                    .data$year,
                                                                                    .data$step) %>%
                                                                    dplyr::mutate(catch_total = sum(.data$catch)) %>%
                                                                    dplyr::ungroup() %>%
                                                                    dplyr::select(.data$population,
                                                                                  .data$year,
                                                                                  .data$step,
                                                                                  .data$catch_total) %>%
                                                                    dplyr::distinct() %>%
                                                                    dplyr::group_by(.data$population,
                                                                                    .data$year) %>%
                                                                    dplyr::arrange(.data$step) %>%
                                                                    dplyr::mutate(catch_cumulative = cumsum(x = .data$catch_total)) %>%
                                                                    dplyr::ungroup() %>%
                                                                    dplyr::select(-c(.data$year,
                                                                                     .data$catch_total)))
            names(simulation_catch_weight_month_cumulative_sums) <- "simulation_catch_weight_month_cumulative_sums"
            current_simulation_data_improved$CatchWeightPopStrMetStep <- c(current_simulation_data_improved$CatchWeightPopStrMetStep,
                                                                           simulation_catch_weight_fleet_month,
                                                                           simulation_catch_weight_fleet_month_cumulative_sums,
                                                                           simulation_catch_weight_month_cumulative_sums)
          }
          if (current_directory_path_files_csv[current_directory_path_files_csv_id] == "EffortsNominalMetier.csv") {
            simulation_effort_nominal_metier_start_year_month <- list(dplyr::mutate(.data = current_csv_data[[1]],
                                                                                    year = as.integer(.data$step / 12 + as.integer(x = stringr::str_extract(string = current_simulation_metadata$simulation_annual_range,
                                                                                                                                                            pattern = "^[[:digit:]]+"))),
                                                                                    step = .data$step + 1,
                                                                                    scenario_name = !!current_simulation_metadata$scenario_name) %>%
                                                                        dplyr::filter(.data$year == as.integer(x = stringr::str_extract(string = current_simulation_metadata$simulation_annual_range,
                                                                                                                                        pattern = "^[[:digit:]]+"))))
            names(simulation_effort_nominal_metier_start_year_month) <- "simulation_effort_nominal_metier_start_year_month"
            current_simulation_data_improved$EffortsNominalMetier <- c(current_simulation_data_improved$EffortsNominalMetier,
                                                                       simulation_effort_nominal_metier_start_year_month)
          }
          if (current_directory_path_files_csv[current_directory_path_files_csv_id] == "MortalitePecheGroupe.csv") {
            simulation_fishing_mortality_group <- list(dplyr::mutate(.data = current_csv_data[[1]],
                                                                     year = as.integer(.data$step / 12 + as.integer(x = stringr::str_extract(string = current_simulation_metadata$simulation_annual_range,
                                                                                                                                             pattern = "^[[:digit:]]+"))),
                                                                     scenario_name = !!current_simulation_metadata$scenario_name,
                                                                     population = dplyr::case_when(
                                                                       .data$population == "Lophius_piscatorius" ~ "Lophius",
                                                                       .default = .data$population)) %>%
                                                         dplyr::rename(fishing_mortality = .data$value,
                                                                       group = .data$fish_group) %>%
                                                         dplyr::select(.data$year,
                                                                       .data$population,
                                                                       .data$group,
                                                                       .data$fishing_mortality,
                                                                       .data$scenario_name))
            names(simulation_fishing_mortality_group) <- "simulation_fishing_mortality_group"
            current_simulation_data_improved$MortalitePecheGroupe <- c(current_simulation_data_improved$MortalitePecheGroupe,
                                                                       simulation_fishing_mortality_group)
          }
          if (current_directory_path_files_csv[current_directory_path_files_csv_id] == "MortalitePecheTotale.csv") {
            simulation_fishing_mortality_total <- list(dplyr::mutate(.data = current_csv_data[[1]],
                                                                     year = as.integer(.data$step / 12 + as.integer(x = stringr::str_extract(string = current_simulation_metadata$simulation_annual_range,
                                                                                                                                             pattern = "^[[:digit:]]+"))),
                                                                     scenario_name = !!current_simulation_metadata$scenario_name,
                                                                     population = dplyr::case_when(
                                                                       .data$population == "Lophius_piscatorius" ~ "Lophius",
                                                                       .default = .data$population)) %>%
                                                         dplyr::rename(fishing_mortality = .data$value) %>%
                                                         dplyr::select(.data$year,
                                                                       .data$population,
                                                                       .data$fishing_mortality,
                                                                       .data$scenario_name))
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
  if ("AbondanceBeginMonth_Gpe_Janvier" %in% names(x = simulation_final$simulations_data_improved_merged)) {
    if ("simulation_abundance_gpe_january" %in% names(x = simulation_final$simulations_data_improved_merged$AbondanceBeginMonth_Gpe_Janvier)) {
      parameters_biological <- readr::read_delim(file = system.file("extdata",
                                                                    "parameters_biological.txt",
                                                                    package = "isisinsight"),
                                                 delim = ";",
                                                 col_types = "ciiddddd",
                                                 col_names = TRUE)
      abundance_length <- dplyr::select(.data = parameters_biological,
                                        -.data$age) %>%
        dplyr::left_join(simulation_final$simulations_data_improved_merged$AbondanceBeginMonth_Gpe_Janvier$simulation_abundance_gpe_january,
                         by = c("population",
                                "group")) %>%
        dplyr::group_by(.data$population,
                        .data$scenario_name,
                        .data$year,
                        .data$length) %>%
        dplyr::mutate(abundance_length = sum(.data$abundance)) %>%
        dplyr::ungroup() %>%
        dplyr::distinct() %>%
        dplyr::select(-.data$abundance)
      length_mean_lc_lfm <- dplyr::group_by(.data = abundance_length,
                                            .data$population,
                                            .data$scenario_name,
                                            .data$year) %>%
        dplyr::mutate(median_abundance_length = stats::median(x = .data$abundance_length)) %>%
        dplyr::filter(.data$abundance_length > .data$median_abundance_length) %>%
        dplyr::mutate(abundance_total_suppress_median = sum(.data$abundance_length)) %>%
        dplyr::reframe(length_mean = sum(.data$abundance_length * .data$length) / .data$abundance_total_suppress_median,
                       lc = min(.data$length),
                       lfm = 0.75 * .data$lc + 0.25 * .data$l_inf,
                       l_inf = .data$l_inf,
                       l_mat = .data$l_mat,
                       l_opt = (2 / 3) * .data$l_inf) %>%
        dplyr::distinct()
      proportion_mega <- dplyr::mutate(.data = abundance_length,
                                       lopt10 = (2 / 3) * .data$l_inf * 1.1) %>%
        dplyr::group_by(.data$population,
                        .data$scenario_name,
                        .data$year) %>%
        dplyr::mutate(abundance_total = sum(.data$abundance_length)) %>%
        dplyr::filter(.data$length > .data$lopt10) %>%
        dplyr::reframe(proportion_mega = sum(.data$abundance_length) / .data$abundance_total) %>%
        dplyr::distinct()
      length_mean_lc_lfm_proportion_mega <- list(dplyr::left_join(x = length_mean_lc_lfm,
                                                                  y = proportion_mega,
                                                                  by = c("population",
                                                                         "scenario_name",
                                                                         "year")) %>%
                                                   tidyr::pivot_longer(cols = .data$length_mean:.data$proportion_mega,
                                                                       names_to = "indicateur",
                                                                       values_to = "value"))
      names(length_mean_lc_lfm_proportion_mega) = "length_mean_lc_lfm_proportion_mega"
      simulation_final$simulations_data_improved_merged$AbondanceBeginMonth_Gpe_Janvier <- c(simulation_final$simulations_data_improved_merged$AbondanceBeginMonth_Gpe_Janvier,
                                                                                             length_mean_lc_lfm_proportion_mega)
    }
  }
  if ("BiomasseBeginMonthFeconde" %in% names(x = simulation_final$simulations_data_improved_merged)) {
    if ("simulation_biomass_spawning" %in% names(x = simulation_final$simulations_data_improved_merged$BiomasseBeginMonthFeconde)) {
      simulation_biomass_spawning_final <- list(dplyr::left_join(x = simulation_final$simulations_data_improved_merged$BiomasseBeginMonthFeconde$simulation_biomass_spawning,
                                                                 y = dplyr::select(.data = parameters_gestion_isis_final,
                                                                                   .data$population,
                                                                                   .data$b_trigger_isis),
                                                                 by = "population"))
      names(simulation_biomass_spawning_final) <- "simulation_biomass_spawning_final"
      simulation_final$simulations_data_improved_merged$BiomasseBeginMonthFeconde <- c(simulation_final$simulations_data_improved_merged$BiomasseBeginMonthFeconde,
                                                                                       simulation_biomass_spawning_final)
    }
  }
  if ("EffortsNominalMetier" %in% names(x = simulation_final$simulations_data_improved_merged)) {
    if ("simulation_effort_nominal_metier_start_year_month" %in% names(x = simulation_final$simulations_data_improved_merged$EffortsNominalMetier)) {
      simulation_effort_nominal_metier_start_year_month_total <- dplyr::group_by(.data = simulation_final$simulations_data_improved_merged$EffortsNominalMetier$simulation_effort_nominal_metier_start_year_month,
                                                                                 .data$metier,
                                                                                 .data$step,
                                                                                 .data$scenario_name) %>%
        dplyr::mutate(effort_metier_month = sum(.data$value)) %>%
        dplyr::ungroup() %>%
        dplyr::select(.data$metier,
                      .data$step,
                      .data$effort_metier_month,
                      .data$scenario_name) %>%
        dplyr::distinct()
      simulation_effort_nominal_metier_start_year_month_bau <- dplyr::filter(.data = simulation_effort_nominal_metier_start_year_month_total,
                                                                             .data$scenario_name == "BAU") %>%
        dplyr::rename(effort_metier_month_bau = .data$effort_metier_month) %>%
        dplyr::select(-.data$scenario_name)
      simulation_effort_nominal_metier_start_year_month <- dplyr::filter(.data = simulation_effort_nominal_metier_start_year_month_total,
                                                                         .data$scenario_name != "BAU")
      area_metier <- readr::read_delim(file = system.file("extdata",
                                                          "data_intersections.txt",
                                                          package = "isisinsight"),
                                       delim = ";",
                                       col_types = "cciii",
                                       col_names = TRUE) %>%
        dplyr::filter(.data$area_management != "zbsi_penatules")
      strange_metier <- dplyr::select(.data = simulation_effort_nominal_metier_start_year_month_bau,
                                      .data$metier) %>%
        dplyr::distinct() %>%
        dplyr::filter(stringr::str_detect(string = .data$metier,
                                          pattern = "27.8")) %>%
        dplyr::mutate(area = stringr::str_extract(string = .data$metier,
                                                  pattern = "(?<=27\\.).+"))
      area_metier_8ab <- dplyr::filter(.data = area_metier,
                                       .data$metier %in% c("8.a",
                                                           "8.b")) %>%
        dplyr::rename(area = .data$metier) %>%
        dplyr::right_join(strange_metier,
                          by = "area") %>%
        dplyr::select(-.data$area)
      area_metier <- dplyr::filter(.data = area_metier,
                                   .data$metier %in% c("8.a",
                                                       "8.b")) %>%
        dplyr::bind_rows(area_metier_8ab)
      area_month_openning_referential <- readr::read_delim(file = system.file("extdata",
                                                                              "area_month_openning_referential.txt",
                                                                              package = "isisinsight"),
                                                           col_types = "icic")
      area_month_openning <- dplyr::left_join(x = area_month_openning_referential,
                                              y = scenarios_names_referential,
                                              by = "simulation_name") %>%
        dplyr::select(-.data$simulation_name)
      metier_referential <- readLines(con = system.file("extdata",
                                                        "metier_referential.txt",
                                                        package = "isisinsight"))
      effort_management_diagnostic <- list(dplyr::left_join(x = simulation_effort_nominal_metier_start_year_month,
                                                            y = simulation_effort_nominal_metier_start_year_month_bau,
                                                            by = c("metier",
                                                                   "step")) %>%
                                             dplyr::left_join(area_month_openning,
                                                              by = c("step",
                                                                     "scenario_name")) %>%
                                             dplyr::left_join(area_metier,
                                                              by = c("metier",
                                                                     "area_management")) %>%
                                             dplyr::rename(openning_metier = .data$openning) %>%
                                             dplyr::mutate(openning_metier = dplyr::case_when(
                                               .data$area_management == "Zone_Coraux" ~ 0,
                                               (.data$scenario_name == "O-D-3_miles"
                                                & (! .data$metier %in% !!metier_referential)
                                                & stringr::str_detect(.data$metier,
                                                                      "0-10"))
                                               | (.data$scenario_name == "O-G-Frayere"
                                                  & (! stringr::str_detect(.data$metier,
                                                                           "G"))
                                                  & (! stringr::str_detect(.data$metier,
                                                                           "O")))
                                               | (.data$scenario_name %in% c("G-Trim1",
                                                                             "G-Fevr",
                                                                             "G-1/3-Trim1")
                                                  & (! stringr::str_detect(.data$metier,
                                                                           "G"))) ~ 1,
                                               .default = .data$openning_metier),
                                               effort_metier_month_management = dplyr::case_when(
                                                 .data$number_cell_metier == 0 ~ 0,
                                                 .default = (.data$effort_metier_month * .data$number_cell_intersection / .data$number_cell_metier)
                                               ),
                                               effort_metier_month_management_area = .data$effort_metier_month_management * .data$openning_metier) %>%
                                             dplyr::group_by(.data$area_management,
                                                             .data$scenario_name) %>%
                                             dplyr::summarise(effort_management = sum(.data$effort_metier_month_management,
                                                                                      na.rm = TRUE),
                                                              effort_manangement_area = sum(.data$effort_metier_month_management_area,
                                                                                            na.rm = TRUE),
                                                              effort_bau = sum(.data$effort_metier_month_bau,
                                                                               na.rm = TRUE)) %>%
                                             dplyr::mutate(effort_reduction_management_area = dplyr::case_when(
                                               .data$effort_bau == 0 ~ 0,
                                               .default = ((.data$effort_bau - .data$effort_manangement_area) / .data$effort_bau) * 100
                                             )) %>%
                                             dplyr::select(.data$scenario_name,
                                                           .data$area_management,
                                                           .data$effort_management,
                                                           .data$effort_manangement_area,
                                                           .data$effort_reduction_management_area))
      names(effort_management_diagnostic) <- "effort_management_diagnostic"
      simulation_final$simulations_data_improved_merged$EffortsNominalMetier <- c(simulation_final$simulations_data_improved_merged$EffortsNominalMetier,
                                                                                  effort_management_diagnostic)
    }
  }
  if ("MortalitePecheGroupe" %in% names(x = simulation_final$simulations_data_improved_merged)) {
    if ("simulation_fishing_mortality_group" %in% names(x = simulation_final$simulations_data_improved_merged$MortalitePecheGroupe)) {
      simulation_fishing_mortality_group_final <- list(dplyr::left_join(x = simulation_final$simulations_data_improved_merged$MortalitePecheGroupe$simulation_fishing_mortality_group,
                                                                        y = dplyr::select(.data = parameters_gestion_isis_final,
                                                                                          .data$population,
                                                                                          .data$f_msy_isis),
                                                                        by = "population"))
      names(simulation_fishing_mortality_group_final) <- "simulation_fishing_mortality_group_final"
      simulation_final$simulations_data_improved_merged$MortalitePecheGroupe <- c(simulation_final$simulations_data_improved_merged$MortalitePecheGroupe,
                                                                                  simulation_fishing_mortality_group_final)
    }
  }
  if ("MortalitePecheTotale" %in% names(x = simulation_final$simulations_data_improved_merged)) {
    if ("simulation_fishing_mortality_total" %in% names(x = simulation_final$simulations_data_improved_merged$MortalitePecheTotale)) {
      simulation_fishing_mortality_total_final <- list(dplyr::left_join(x = simulation_final$simulations_data_improved_merged$MortalitePecheTotale$simulation_fishing_mortality_total,
                                                                        y = dplyr::select(.data = parameters_gestion_isis_final,
                                                                                          .data$population,
                                                                                          .data$f_msy_isis),
                                                                        by = "population"))
      names(simulation_fishing_mortality_total_final) <- "simulation_fishing_mortality_total_final"
      simulation_final$simulations_data_improved_merged$MortalitePecheTotale <- c(simulation_final$simulations_data_improved_merged$MortalitePecheTotale,
                                                                                  simulation_fishing_mortality_total_final)
    }
  }
  ## 2.x - Output(s) export(s) ----
  if (! is.null(x = output_path)
      && length(x = simulation_final$simulations_data_improved_merged) != 0) {
    final_output_path <- file.path(output_path,
                                   paste0(format(x = Sys.time(),
                                                 "%Y%m%d_%H%M%S"),
                                          "_isisfish_simulations_data_improved"))
    dir.create(path = final_output_path)
    for (current_simulations_data_improved_merged_id in seq_len(length.out = length(x = simulation_final$simulations_data_improved_merged))) {
      for (current_simulations_data_improved_merged_sub_id in seq_len(length.out = length(x = simulation_final$simulations_data_improved_merged[[current_simulations_data_improved_merged_id]]))) {
        if (output_format == "rds") {
          saveRDS(object = simulation_final$simulations_data_improved_merged[[current_simulations_data_improved_merged_id]][[current_simulations_data_improved_merged_sub_id]],
                  file = file.path(final_output_path,
                                   paste0(names(x = simulation_final$simulations_data_improved_merged[[current_simulations_data_improved_merged_id]][current_simulations_data_improved_merged_sub_id]),
                                          ".rds")))
        } else {
          readr::write_csv2(x = simulation_final$simulations_data_improved_merged[[current_simulations_data_improved_merged_id]][[current_simulations_data_improved_merged_sub_id]],
                            file = file.path(final_output_path,
                                             paste0(names(x = simulation_final$simulations_data_improved_merged[[current_simulations_data_improved_merged_id]][current_simulations_data_improved_merged_sub_id]),
                                                    ".csv")))
        }
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
