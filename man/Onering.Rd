\name{Onering}
\alias{Onering}
\title{Automate SDMtune and enmSdmX Boyce Index analysis
}
\usage{
Onering(model, model_name, test, variables, p, bg, output_dir = ".")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{SDMmodel or SDMmodelCV object
}
  \item{model_name}{character. The name of the model.
}
  \item{test}{ teste dataset
}
  \item{variables}{Spatrast stack
}
  \item{p}{data.frame Presence data with Longitude/Latitude
}
  \item{bg}{data.frame Background data with Longitude/Latitude
}
  \item{output_dir}{The output directory for writing the raster prediction. Uses the directory from the session
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
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

