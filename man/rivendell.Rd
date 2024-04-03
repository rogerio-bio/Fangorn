\name{rivendell}
\alias{rivendell}
\title{Automate evaluation of species distribution models with AUC, TSS, Boyce Index, OPR, UPR, PPI and PAI}
\description{
Automates the process of evaluating species distribution models using metrics such as AUC (Area Under the Curve), TSS (True Skill Statistic), prediction raster generation,CBI (Continuous Boyce Index), OPR (over-prediction rate), UPR (under-prediction rate), PPI (Potential Presence Increment) and PAI (Potential Absence Increment) through packages like SDMtune and enmSdmX. It generates suitability maps for each model, collects analysis results in a structured data.frame, and exports the results for easy review.}
\usage{
rivendell(input, test, variables, p, bg, threshold, remove_prediction = TRUE, identifier = NULL, output_dir = ".")
}
\arguments{
  \item{input}{A S4 object of SDMmodel or SDMmodelCV objects representing trained species distribution models.}
  \item{test}{A test dataset for model evaluation.}
  \item{variables}{A Spatrast stack of environmental variables used for prediction.}
  \item{p}{Presence data as a data.frame with 'Longitude' and 'Latitude' columns.}
  \item{bg}{Background data as a data.frame with 'Longitude' and 'Latitude' columns.}
  \item{threshold}{Character string specifying the threshold selection criterion: 'maxSSS' (Maximum training sensitivity plus specificity) or 'maxtSSS' (Maximum test sensitivity plus specificity).}
  \item{remove_prediction}{Logical. If TRUE, prediction objects are removed after processing; if FALSE, they are retained.}
  \item{identifier}{Optional. A string to append to model names in output files, e.g., "run_1".}
  \item{output_dir}{Optional directory path for saving output files. Defaults to the current working directory.}

}
\value{A data.frame summarizing the analysis results for each model, including AUC, TSS, selected threshold, omission rate, CBI, OPR, UPR, PPAI and PAI}
\author{Rogério Nunes}

\examples{
\dontrun{
files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd",
                    full.names = TRUE)

predictors <- terra::rast(files)

# Prepare presence and background locations
p <- virtualSp$presence
bg <- virtualSp$background

# Create SWD object
data <- prepareSWD(species = "Virtual species",
                   p = p_coords,
                   a = bg_coords,
                   env = predictors,
                   categorical = "biome")
# Split presence locations in training (80%) and testing (20%) datasets
datasets <- trainValTest(data,
                         test = 0.2,
                         only_presence = TRUE)
train <- datasets[[1]]
test <- datasets[[2]]

# Train a model
model <- train(method = "Maxnet",
               data = train,
               fc = "l")

# Define the hyperparameters to test
h <- list(reg = 1:2,
          fc = c("lqp", "lqph"))

# Run the function using the AUC as metric
output <- gridSearch(model,
                     hypers = h,
                     metric = "auc",
                     test = test)

rivendell(output, test, variables, p, bg, "maxSSS", remove_prediction = FALSE, identifier = "run_1")
}
}

\references{
  \itemize{
    \item Smith A, Murphy S, Henderson D, Erickson K (2023). "Improving species distribution models with precise data." Global Ecology & Biogeography, 32, -13. doi:10.1111/geb.13628.
    \item Vignali S, Barras AG, Arlettaz R, Braunisch V. SDMtune: An R package for species distribution model tuning. Ecol Evol. 2020; 10: 11488–11506. https://doi.org/10.1002/ece3.6786.
    \item Breiner, F. T., Nobis, M. P., Bergamini, A., & Guisan, A. (2018). Small models, big gains: Improving prediction of species distribution with few occurrences. Methods in Ecology and Evolution, 9(4), 802-808. https://doi.org/10.1111/2041-210X.12957
  }
}
