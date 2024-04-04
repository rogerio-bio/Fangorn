text <- paste0("
 _____
|  ___|_ _ _ __   __ _  ___  _ __ _ __
| |_ / _` | '_ \ / _` |/ _ \| '__| '_ \
|  _| (_| | | | | (_| | (_) | |  | | | |
|_|  \__,_|_| |_|\__, |\___/|_|  |_| |_|
                 |___/                   version ",
               utils::packageVersion("Fangorn"),
               "\n\nTo cite this package in publications type: citation(\"Fangorn\").")


.onAttach <- function(libname,
                      pkgname) {

  packageStartupMessage(text)
}
