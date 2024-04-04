# Fangorn 1.0.5

* Fixed an issue that could occur when *combineCV* analysis was run after calculating AUC and TSS. The *combineCV* has been moved to the top of the loop to ensure consistency in extracting such results.

# Fangorn 1.0.4

* Major rework of the `palantiris` function.
* Documentation updates and corrections for `palantiris`.
* Introduction of a new function, `rivendell`.
* `rivendell` analyzes multiple models (tested up to 100), excluding duplicates and offering options to export (or not) suitability maps and append identifiers to model names for easier output management.
* `rivendell` is designed to work specifically with S4 objects from SDMtune package. If you have a list of models, we recommend using the `palantiris` function instead, which provides robust functionality for such scenario. 
* Corrections for `Onering` documentation


# Fangorn 1.0.3

* Fixed a bug where thresholds results were not saving in the environment

# Fangorn 1.0.2

* Add an option to bypass the combineCV analysis if the object is an SDMmodel
* Reduced the lenght of phrases between analysis


# Fangorn 1.0.1

* Added the option to chose between maxSSS (Maximum training sensitivity plus specificity) or maxtSSS (Maximum test sensitivity plus specificity) for when calculating thresholds, in `Onering` and `palantiris`
* Added NEWS.md for package updates

# Fangorn 1.0.0

* First release
