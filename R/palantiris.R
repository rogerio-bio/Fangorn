#' Run Ecological Niche Modeling and Analysis with Palantiris
#'
#' This function, palantiris, performs Ecological Niche Modeling (ENM) and analysis using a set of models.
#' It calculates Area Under the Receiver Operating Characteristic Curve (AUC), True Skill Statistic (TSS),
#' prediction raster, and Boyce Index (CBI) for each model. The results are combined into a final table and
#' exported as a CSV file. Additionally, prediction raster files are saved in GeoTIFF format.
#'
#' @param models A list of ecological models. Each model should be a fitted ENM.
#' @param test A data frame containing the test occurrences for model evaluation.
#' @param variables A data frame containing the environmental variables for prediction.
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
#' @import dismo
#' @import enmSdmX
#' @import SDMtune
#' @import terra
#' @import crayon
#'
#' @export
