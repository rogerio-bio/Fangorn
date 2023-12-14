\name{Onering}
\alias{Onering}
\title{Automate SDMtune and enmSdmX Boyce Index analysis
}
\description{Automates the process of evaluating one species distribution models using various metrics, including AUC (Area Under the Curve), TSS (True Skill Statistic), and prediction, while also generating and exporting suitability maps through SDMtune package. It collects the results in a structured data.frame for easy analysis and additionally calculates the Boyce Index (CBI) to assess model performance and reliabilit}
\usage{
Onering(model, model_name, test, variables, p, bg, threshold,output_dir = ".")
}
\arguments{
  \item{model}{SDMmodel or SDMmodelCV object.}
  \item{model_name}{A character string specifying the name of the model.}
  \item{test}{ A test dataset used for evaluation.}
  \item{variables}{Spatrast stack of environmental variables}
  \item{p}{A data.frame containing presence data with Longitude/Latitude information.}
  \item{bg}{A data.frame containing background data with Longitude/Latitude information}
  \item{threshold}{character. character.maxSSS (Maximum training sensitivity plus specificity) or maxtSSS (Maximum test sensitivity plus specificity)}
  \item{output_dir}{An optional output directory for writing the raster prediction. The default is the current directory.}
}
\value{Returns a data.frame with analysis results.}
\author{Rogério Nunes}
\examples{
\dontrun{
# Acquire environmental variables
files <- list.files(path = file.path(system.file(package = "dismo"), "ex"),
                    pattern = "grd",
                    full.names = TRUE)

predictors <- terra::rast(files)

# Prepare presence and background locations
p <- virtualSp$presence
bg <- virtualSp$background

#

# Create SWD object
data <- prepareSWD(species = "Virtual species",
                   p = p_coords,
                   a = bg_coords,
                   env = predictors,
                   categorical = "biome")

# Split presence locations in training and testing
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
output@results

om1 <- output@models [[1]]

# Example usage:
Onering(om1, "om1", test, predictors, p_coords, bg_coords)
}
}

\references{
\itemize{
\item Smith A, Murphy S, Henderson D, Erickson K (2023). “Including imprecisely georeferenced specimens improves accuracy of species distribution models and estimates of niche breadth.” Global Ecology & Biogeography, 32, -13. doi:10.1111/geb.13628.
\item Vignali S, Barras AG, Arlettaz R, Braunisch V. SDMtune: An R package to tune and evaluate species distribution models. Ecol Evol. 2020; 10: 11488–11506. https://doi.org/10.1002/ece3.6786.
 Breiner, F. T., Nobis, M. P., Bergamini, A., & Guisan, A. (2018). Optimizing ensembles of small models for predicting the distribution of species with few occurrences. Methods in Ecology and Evolution, 9(4), 802-808. https://doi.org/10.1111/2041-210X.12957
}
}

