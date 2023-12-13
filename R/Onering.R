#' Onering Function
#'
#' This function automate the calculation of AUC, TSS, prediction, thresholds, and CBI.
#'
#' @param model SDMmodel or SDMmodelCV object.
#' @param model_name character. The name of the model.
#' @param test numeric. The percentage of data withhold for testing.
#' @param variables Spatrast stack.
#' @param p data.frame Presence data with Longitude/Latitude.
#' @param bg data.frame Background data with Longitude/Latitude.
#' @param output_dir The output directory for writing the raster prediction.
#'
#' @return A data frame with AUC, TSS, Threshold, Omission and CBI values.
#'
#' @import crayon
#' @import dismo
#' @import enmSdmX
#' @import SDMtune
#' @import terra
#'
#' @examples
#' Onering(model, "my_model", test, variables, p, bg)
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

#' @export
Onering <- function(model, model_name, test, variables, p, bg, output_dir = ".") {
  # Check required packages
  if (!requireNamespace("crayon", quietly = TRUE)||
      !requireNamespace("dismo", quietly = TRUE) ||
      !requireNamespace("enmSdmX", quietly = TRUE) ||
      !requireNamespace("SDMtune", quietly = TRUE) ||
      !requireNamespace("terra", quietly = TRUE))  {
    stop("One or more required packages not installed.")
  }

  # AUC and TSS calculation
  auc_value <- SDMtune::auc(model, test = test)
  tss_value <- SDMtune::tss(model, test = test)

  cat("\nAUC and TSS values:\n")
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
  cat("\nThresholds Information (Maximum test sensitivity plus specificity):\n")
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
  cat("Boyce Index (CBI):", cbiMax, "\n")

  # Organize results in a data frame
  results_table <- data.frame(
    AUC = auc_value,
    TSS = tss_value,
    Threshold = cloglog_value,
    Omission = test_omission_rate,
    CBI = cbiMax
  )

  cat(crayon::bold$cyan("Final results:\n"))

  return(results_table)
}
# Example usage:
Onering(model, "model_name", test, variables, p, bg)
