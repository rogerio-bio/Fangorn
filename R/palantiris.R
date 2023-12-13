#' Automate steps for SDMtune models and CBI calculation
#'
#' #' It calculates Area Under the Receiver Operating Characteristic Curve (AUC), True Skill Statistic (TSS),
#' prediction raster, and Boyce Index (CBI) for each model. The results are combined into a final table and
#' exported as a CSV file. Additionally, prediction raster files are saved in GeoTIFF format.
#'
#' @param models A list of SDMmodel or SDMmodelCV object.
#' @param test A data frame containing the test occurrences for model evaluation.
#' @param variables A Spatrast environmental variables for prediction.
#' @param p The species presence data frame with "Longitude" and "Latitude" columns.
#' @param bg The background points data frame with "Longitude" and "Latitude" columns.
#' @param output_dir The directory to save the output files. Default is the current working directory.
#'
#' @return A data frame summarizing the AUC, TSS, Cloglog Value, Test Omission Rate, and CBI for each model.
#'
#' @examples
#' \dontrun{
#'   # Example Usage:
#'   palantiris(model_list, test_data, env_variables, species_presence, background_points)
#' }
#'
#' @import crayon
#' @import dismo
#' @import enmSdmX
#' @import SDMtune
#' @import terra
#'
#'@examples
#' Acquire environmental variables
files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd",
                    full.names = TRUE)

variables <- terra::rast(files)

# Prepare presence and background locations
p <- SDMtune::virtualSp$presence
bg <- SDMtune::virtualSp$background

# Rename the "x" column to "Longitude" and the "y" column to "Latitude"
names(p)[names(p) == "x"] <- "Longitude"
names(p)[names(p) == "y"] <- "Latitude"

names(bg)[names(bg) == "x"] <- "Longitude"
names(bg)[names(bg) == "y"] <- "Latitude"

# Create SWD object
data <- SDMtune::prepareSWD(species = "Virtual species",
                            p = p,
                            a = bg,
                            env = variables,
                            categorical = "biome")

# Split presence locations in training (80%) and testing (20%) datasets
datasets <- SDMtune::trainValTest(data,
                                  test = 0.2,
                                  only_presence = TRUE)
train <- datasets[[1]]
test <- datasets[[2]]

# Train a model
model <- SDMtune::train(method = "Maxnet",
                        data = train,
                        fc = "l")

# Define the hyperparameters to test
h <- list(reg = 1:2,
          fc = c("lqp", "lqph"))

# Run the function using the AUC as metric
output <- SDMtune::gridSearch(model,
                              hypers = h,
                              metric = "auc",
                              test = test)
output@results

om1 <- output@models [[1]]
om2 <- output@models [[2]]
om3 <- output@models [[3]]
om4 <- output@models [[4]]

models <-list(om1 = om1, om2 = om2, om3 = om3, om4 = om4)

#' @export
palantiris <- function(models, test, variables, p, bg, output_dir = ".") {
  # Check required packages
  if (!requireNamespace("crayon", quietly = TRUE)||
      !requireNamespace("dismo", quietly = TRUE) ||
      !requireNamespace("enmSdmX", quietly = TRUE) ||
      !requireNamespace("SDMtune", quietly = TRUE) ||
      !requireNamespace("terra", quietly = TRUE))  {
    stop("One or more required packages not installed.")
  }

  # If models is not a list, convert it to a list
  if (!is.list(models)) {
    models <- list(models)
  }

  # Initialize all_results as an empty list
  all_results <- list()

  # Generate model names based on the names of the models
  model_names <- names(models)
  if (any(model_names == "")) {
    model_names[model_names == ""] <- paste0("Model_", seq_along(models))[model_names == ""]
  }

  # Iterate over the list of models
  for (i in seq_along(models)) {
    model <- models[[i]]
    model_name <- model_names[i]

    # Validate jaguar and bg data frames
    stopifnot(
      is.data.frame(p),
      is.data.frame(bg),
      setequal(names(p), c("Longitude", "Latitude")),
      setequal(names(bg), c("Longitude", "Latitude"))
    )

    # AUC and TSS calculation
    auc_value <- SDMtune::auc(model, test = test)
    tss_value <- SDMtune::tss(model, test = test)

    cat("\nAUC and TSS values for", model_name, ":\n")
    cat("AUC : ", auc_value, "\n")
    cat("TSS : ", tss_value, "\n")

    # Prediction
    p_model <- dismo::predict(model, data = variables, type = "cloglog")
    assign(paste0("p_", model_name), p_model, envir = .GlobalEnv)  # Assign prediction object with model name

    # Define a pool of phrases
    phrases <- c(
      "Exporting Fangorn's ecological portrait...",
      "Mapping species distribution in Middle-earth...",
      "Treebeard is analyzing your request...",
      "Galadriel is inspecting the ecological models you provided...",
      "Arwen is contemplating the nuances of your species distribution...",
      "The Orcs are assessing the invasive potential in your distribution models...",
      "Sauron is delving into the heart of your distribution models, searching for control...",
      "The Uruk-hai are mapping out the conquest potential in your ecological models...",
      "The Black Riders are shadowing the paths revealed by your distribution analysis...",
      "Mount Doom's fiery depths reveal the hidden influences shaping your species distributions..."
    )

    # Randomly select a phrase from the pool
    selected_phrase <- sample(phrases, 1)

    # Print the selected phrase with different colors
    if (grepl("Galadriel", selected_phrase, ignore.case = TRUE)) {
      cat(crayon::yellow$bold("\n", selected_phrase, "\n"))
    } else if (grepl("Arwen", selected_phrase, ignore.case = TRUE)) {
      cat(crayon::cyan$bold("\n", selected_phrase, "\n"))
    } else if (grepl("Orc|Sauron|Black Riders|Mount Doom's|Uruk-hai", selected_phrase, ignore.case = TRUE)) {
      cat(crayon::red$bold("\n", selected_phrase, "\n"))
    } else {
      cat(crayon::green$bold("\n", selected_phrase, "\n"))
    }

    # Export prediction raster
    raster_file <- file.path(output_dir, paste0("p_", model_name, ".tif"))
    terra::writeRaster(p_model, filename = raster_file, overwrite = TRUE)

    # Combine cross-validation and calculate thresholds
    model_cv <- SDMtune::combineCV(model)
    thresholds_result <- SDMtune::thresholds(model_cv, type = "cloglog", test = test)

    # Find the row corresponding to "Maximum test sensitivity plus specificity"
    max_sensitivity_row <- thresholds_result[thresholds_result$Threshold == "Maximum test sensitivity plus specificity", ]

    # Extract Cloglog value and Test omission rate
    cloglog_value <- max_sensitivity_row$`Cloglog value`
    test_omission_rate <- max_sensitivity_row$`Test omission rate`

    # Print the extracted information
    cat("\nThresholds Information (Maximum test sensitivity plus specificity) for", model_name, ":\n")
    cat("Cloglog Value: ", cloglog_value, "\n")
    cat("Test Omission Rate: ", test_omission_rate, "\n")

    # Assign thresholds object with model name
    assign(paste0("ths_", model_name), thresholds_result, envir = .GlobalEnv)

    # Define a pool of phrases
    phrases <- c(
      "Frodo maps the impact of the Boyce Index, a burden as heavy as the One Ring..",
      "Gandalf seeks insights in the arcane calculations of the Boyce Index, as formidable as facing the Balrog..",
      "Aragorn charts the path through the treacherous terrains of the Boyce Index, akin to his ranger journeys..",
      "Gimli mines insights from the complex calculations of the Boyce Index, challenges surpassing the Mines of Moria.."
    )

    # Randomly select a phrase from the pool
    selected_phrase <- sample(phrases, 1)

    # Print the selected phrase with different colors
    if (grepl("Frodo", selected_phrase, ignore.case = TRUE)) {
      cat(crayon::green$bold("\n", selected_phrase, "\n"))
    } else if (grepl("Gandalf", selected_phrase, ignore.case = TRUE)) {
      cat(crayon::yellow$bold("\n", selected_phrase, "\n"))
    } else if (grepl("Aragorn", selected_phrase, ignore.case = TRUE)) {
      cat(crayon::red$bold("\n", selected_phrase, "\n"))
    } else if (grepl("Gimli", selected_phrase, ignore.case = TRUE)) {
      cat(crayon::black$bold("\n", selected_phrase, "\n"))
    } else {
      cat(crayon::reset("\n", selected_phrase, "\n"))
    }

    # CBI calculation
    species_presence <- terra::vect(p, geom = c("Longitude", "Latitude"), crs = "WGS84")
    background_points <- terra::vect(bg, geom = c("Longitude", "Latitude"), crs = "WGS84")

    # Extract values of prediction raster
    pres <- terra::extract(p_model, species_presence)[, 1]
    contrast <- terra::extract(p_model, background_points)[, 1]

    # Converts to numeric
    pres <- as.numeric(pres)
    contrast <- as.numeric(contrast)

    # Calculate Boyce Index using evalContBoyce function
    cbiMax <- enmSdmX::evalContBoyce(pres, contrast, na.rm = TRUE)
    assign(paste0("cbi_", model_name), cbiMax, envir = .GlobalEnv)  # Assign cbi object with model name

    # Print only the CBI value
    cat("Boyce Index (CBI) for", model_name, ":", cbiMax, "\n")

    results_table <- data.frame(
      Model = model_name,
      AUC = auc_value,
      TSS = tss_value,
      Threshold = cloglog_value,
      Omission = test_omission_rate,
      CBI = cbiMax
    )

    # Append results to the list
    all_results[[i]] <- results_table
  }

  # Combine results for all models into a single table
  final_results_table <- do.call(rbind, all_results)

  # If only one model is provided, convert the final_results_table to a data frame
  if(length(models) == 1) {
    final_results_table <- as.data.frame(final_results_table)
  }

  # Export final results table as CSV
  csv_file <- file.path(output_dir, "results_table.csv")
  write.csv(final_results_table, file = csv_file, row.names = FALSE)

  cat(crayon::bold$cyan("Final results:\n"))

  # Return the final results table
  return(final_results_table)
}
