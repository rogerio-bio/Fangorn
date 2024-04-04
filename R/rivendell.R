#' Automated Evaluation of SDM Models and CBI Calculation with rivendell Function
#'
#' This function streamlines the evaluation of species distribution models (SDMs) by computing key performance metrics such as the Area Under the Curve (AUC), True Skill Statistic (TSS), and Continuous Boyce Index (CBI). It supports decision-making by generating prediction rasters and compiling a comprehensive results table. Suitable for use with models created via SDMtune or other SDM packages.
#'
#' @param input A S4 object from SDMtune package (SDMmodel or SDMmodelCV objects).
#' @param test A data frame containing the test occurrences for model evaluation.
#' @param variables A Spatrast of environmental variables for prediction.
#' @param p The presence data frame with "Longitude" and "Latitude" columns.
#' @param bg The background data frame with "Longitude" and "Latitude" columns.
#' @param threshold The threshold selection criterion ('maxSSS' or 'maxtSSS').
#' @param remove_prediction Whether to remove prediction objects after processing - TRUE or FALSE.
#' @param identifier An optional identifier to append to model names for output differentiation.
#' @param output_dir Directory for saving output files, defaults to current directory.
#'
#' @return Returns a summary data.frame with AUC, TSS, selected threshold, omission rate, CBI, OPR, UPR, PPI and PAI for each evaluated model.
#'
#'@examples
#' @examples
#' \dontrun{
#' Acquire environmental variables
#' files <- list.files(path = file.path(system.file(package = "dismo"), "ex"), pattern = "grd",full.names = TRUE)
#' predictors <- terra::rast(files)
#'
#' Prepare presence and background locations
#' p <- virtualSp$presence
#' bg <- virtualSp$background
#'
#' Rename the "x" column to "Longitude" and the "y" column to "Latitude"
#'names(p_coords)[names(p_coords) == "x"] <- "Longitude"
#'names(p_coords)[names(p_coords) == "y"] <- "Latitude"
#'names(bg_coords)[names(bg_coords) == "x"] <- "Longitude"
#'names(bg_coords)[names(bg_coords) == "y"] <- "Latitude"
#'
#'Create SWD object
#'data <- prepareSWD(species = "Virtual species",
#'                   p = p_coords,
#'                   a = bg_coords,
#'                   env = predictors,
#'                   categorical = "biome")
#' # Split presence locations in training (80%) and testing (20%) datasets
#'datasets <- trainValTest(data,
#'                         test = 0.2,
#'                         only_presence = TRUE)
#'train <- datasets[[1]]
#'test <- datasets[[2]]
#'
#' Train a model
#'model <- train(method = "Maxnet",
#'               data = train,
#'               fc = "l")
#'
#'Define the hyperparameters to test
#'h <- list(reg = 1:2,
#'          fc = c("lqp", "lqph"))
#'
#'Run the function using the AUC as metric
#'output <- gridSearch(model,
#'                     hypers = h,
#'                     metric = "auc",
#'                     test = test)
#'
#'rivendell(output, test, variables, p, bg, "maxSSS", remove_prediction = FALSE, identifier = "run_1")
#'}
#'
#' @export
rivendell <- function(input, test, variables, p, bg, threshold, remove_prediction, identifier = FALSE, output_dir = ".") {
  # Check required packages
  packages <- c("crayon", "dismo", "enmSdmX", "SDMtune", "terra")
  missing_packages <- packages[!packages %in% installed.packages()[,"Package"]]
  if (length(missing_packages) > 0) {
    stop("One or more required packages not installed: ", paste(missing_packages, collapse = ", "))
  }

  # Validate 'p' and 'bg' data frames
  required_columns <- c("Longitude", "Latitude")
  if (!all(required_columns %in% names(p)) || !all(required_columns %in% names(bg))) {
    stop("Both 'p' and 'bg' must be data frames with 'Longitude' and 'Latitude' columns.")
  }

  # Validate threshold selection
  if (!threshold %in% c("maxSSS", "maxtSSS")) {
    stop("Invalid threshold. Choose 'maxSSS' or 'maxtSSS'.")
  }

  # Function to apply color based on AUC value
  color_auc <- function(auc) {
    if (auc <= 0.5) crayon::red(crayon::bold(auc))
    else if (auc >= 0.6 & auc < 0.7) crayon::yellow(crayon::bold(auc))
    else if (auc >= 0.7 & auc < 0.8) crayon::blue(crayon::bold(auc))
    else if (auc >= 0.8) crayon::green(crayon::bold(auc))
    else as.character(auc)  # Default, no color
  }

  # Function to apply color based on TSS value
  color_tss <- function(tss) {
    if (tss <= 0.2) crayon::red(crayon::bold(tss))
    else if (tss >= 0.3 & tss < 0.5) crayon::yellow(crayon::bold(tss))
    else if (tss >= 0.5 & tss < 0.8) crayon::blue(crayon::bold(tss))
    else if (tss >= 0.8) crayon::green(crayon::bold(tss))
    else as.character(tss)  # Default, no color
  }

  # Function to apply color based on CBI value
  color_cbi <- function(cbi) {
    if (cbi >= 0.9) crayon::green(crayon::bold(cbi))
    else if (cbi >= 0.8) crayon::blue(crayon::bold(cbi))
    else crayon::black(crayon::bold(cbi))  # Default for values < 0.8
  }

  # Function to apply color based on omssion value
  color_value2 <- function(value) {
    if (value <= 0.10) crayon::green(crayon::bold(value))
    else crayon::red(crayon::bold(value))
  }

  # Remove duplicated entries based on 'fc' and 'reg'
  unique_combinations <- unique(input@results[, c("fc", "reg")])

  # Initialize an empty list to hold model names
  model_names <- list()

  for (i in 1:nrow(unique_combinations)) {
    fc <- unique_combinations$fc[i]
    reg <- unique_combinations$reg[i]

    # Find the first model that matches the fc and reg combination
    model_index <- which(input@results$fc == fc & input@results$reg == reg)[1]
    model <- input@models[[model_index]]
    model_name <- paste(fc, reg, ifelse(is.null(identifier), "", identifier), sep = "_")  # Correctly include identifier in model name

    # Check if model_name is empty and skip if true
    if(nchar(model_name) == 0) {
      next
    }

    # Store the model name with its index for later use
    model_names[[model_index]] <- model_name
  }

  # Initialize results list
  all_results <- list()

  # Process each model
  for (i in seq_along(model_names)) {  # Ensure iteration over model_names
    model_name <- model_names[[i]]

    # Skip if model_name is not defined or empty
    if(is.null(model_name) || nchar(model_name) == 0) {
      next
    }

    model <- input@models[[i]]

    # Combine cross-validation
    model_cv <- SDMtune::combineCV(model)

    # Calculate AUC and TSS
    auc_value <- SDMtune::auc(model_cv, test = test)
    tss_value <- SDMtune::tss(model_cv, test = test)

    cat("\nAUC and TSS values for", model_name, ":\n")
    cat("AUC : ", color_auc(auc_value), "\n")
    cat("TSS : ", color_tss(tss_value), "\n")

    # Model prediction
    p_model <- dismo::predict(model_cv, data = variables, type = "cloglog")
    assign(paste0("p_", model_name), p_model, envir = .GlobalEnv)

    # Combine cross-validation and calculate thresholds
    thresholds_result <- SDMtune::thresholds(model_cv, type = "cloglog", test = test)
    assign(paste0("thresholds_result_", model_name), thresholds_result, envir = .GlobalEnv)

    # Calculate PPI
    threshold_value <- thresholds_result[3, 2]
    conf_matrix <- SDMtune::confMatrix(model_cv, test = test, th = threshold_value, type = "cloglog")
    rohirrim_results <- rohirrim(conf_matrix)

    # Find the row corresponding to the specified threshold
    if (threshold == "maxSSS") {
      threshold_row <- thresholds_result[thresholds_result$Threshold == "Maximum training sensitivity plus specificity", ]
    } else if (threshold == "maxtSSS") {
      threshold_row <- thresholds_result[thresholds_result$Threshold == "Maximum test sensitivity plus specificity", ]
    }

    # Extract values based on selected threshold
    value1 <- threshold_row$`Cloglog value`
    value2 <- ifelse(threshold == "maxSSS", threshold_row$`Training omission rate`, threshold_row$`Test omission rate`)

    # Print the extracted information
    if (threshold == "maxSSS") {
      cat("\nThresholds Information (Maximum training sensitivity plus specificity) for", model_name, ":\n")
      cat("Cloglog Value: ", value1, "\n")
      cat("Training Omission Rate: ", color_value2(value2), "\n")
    } else if (threshold == "maxtSSS") {
      cat("\nThresholds Information (Maximum test sensitivity plus specificity) for", model_name, ":\n")
      cat("Cloglog Value: ", value1, "\n")
      cat("Test Omission Rate: ", color_value2(value2), "\n")
    }

    # CBI calculation
    species_presence <- terra::vect(p, geom = c("Longitude", "Latitude"), crs = "WGS84")
    background_points <- terra::vect(bg, geom = c("Longitude", "Latitude"), crs = "WGS84")

    # Extract values of prediction raster
    pres <- extract(p_model, species_presence)[, 1]
    contrast <- extract(p_model, background_points)[, 1]

    # Converts to numeric
    pres <- as.numeric(pres)
    contrast <- as.numeric(contrast)

    # Calculate Boyce Index using evalContBoyce function
    cbiMax <- enmSdmX::evalContBoyce(pres, contrast, na.rm = TRUE)
    assign(paste0("cbi_", model_name), cbiMax, envir = .GlobalEnv)  # Assign cbi object with model name

    cat("Boyce Index (CBI) for", model_name, ":", color_cbi(cbiMax), "\n")

    results_table <- data.frame(
      Model = model_name,
      AUC = auc_value,
      TSS = tss_value,
      Threshold = value1,
      Omission = value2,
      CBI = cbiMax,
      OPR = rohirrim_results$OPR,
      UPR = rohirrim_results$UPR,
      PPI = rohirrim_results$PPI,
      PAI = rohirrim_results$PAI
    )
    all_results[[i]] <- results_table
  }

  # Conditional removal of prediction object or export raster
  if (remove_prediction) {
    lapply(names(all_results), function(name) rm(list = paste0("p_", name), envir = .GlobalEnv))
  } else {
    lapply(names(all_results), function(name) {
      raster_file <- file.path(output_dir, paste0("p_", name, ".tif"))
      terra::writeRaster(get(paste0("p_", name), envir = .GlobalEnv), filename = raster_file, overwrite = TRUE)
    })
  }

  # Combine results and export to CSV
  final_results_table <- do.call(rbind, all_results)
  csv_file <- file.path(output_dir, paste("results_table_combined_", identifier, ".csv", sep = ""))
  write.csv(final_results_table, file = csv_file, row.names = FALSE)

  cat(crayon::bold$magenta("Final results:\n"))

  return(final_results_table)
}
