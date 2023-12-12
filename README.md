
<img src="https://github.com/rogerio-bio/Fangorn/assets/90930770/17034e97-f469-4b02-b1b3-c7afc326ec24" alt="fangorn package" min-width="200px" max-width="200px" width="250px" align="right">

# Fangorn
<img src="https://img.shields.io/badge/R_package-3276b5" />

## 📃 Description

**Fangorn** simplifies key steps in SDMtune model analyses by automating AUC, TSS, prediction and seamlessly integrates with the enmSdmX package to calculate the CBI (Boyce Index).

Utilize the *Onering* function for a single model or employ the *palantiris* function for a list of models.

Additionally, *rohirrim* calculates auxiliary metrics proposed by [Barbosa et al., (2013)](https://onlinelibrary.wiley.com/doi/10.1111/ddi.12100), by providing important results for SDM's, such as Over-prediction rate (OPR), Under-prediction rate (UPR), Potential Presence Increment (PPI), and Potential Absence Increment (PAI) from a confusion matrix

## 🚀 Instalation

You can install the package from GitHub using the following command:

``` r
remotes::install_github('rogerio-bio/Fangorn', dependencies=TRUE)
```

## 💻 Usage

### *Onering*
``` r
library (terra)
library (SDMtune)
library (enmSdmX)
library (dismo)
library (Fangorn)

# Call the function
Onering(model, "model_name", test, variables, p, bg)

AUC and TSS values:
AUC :  0.775805 
TSS :  0.3779693 
Predict - Maxent  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s - 00:03:46.6

 The Orcs are assessing the invasive potential in your distribution models...

Thresholds Information (Maximum test sensitivity plus specificity):
Cloglog Value:  0.5910706 
Test Omission Rate:  0.05206847 

 Frodo maps the impact of the Boyce Index, a burden as heavy as the One Ring.. 
Boyce Index (CBI): 0.6716715 
Final results:
       AUC       TSS Threshold   Omission       CBI
1 0.775805 0.3779693 0.5910706 0.05206847 0.6716715

```
### *palantiris*

``` r
> palantiris(model_list, test, variables, p, bg)

AUC and TSS values for lq_01_2.58 :
AUC :  0.775805 
TSS :  0.3779693 
Predict - Maxent  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s - 00:03:41.3

  Exporting Fangorns ecological portrait...

Thresholds Information (Maximum test sensitivity plus specificity) for lq_01_2.58 :
Cloglog Value:  0.5910706 
Test Omission Rate:  0.05206847 

 Gimli mines insights from the complex calculations of the Boyce Index, challenges surpassing the Mines of Moria.. 
Boyce Index (CBI) for lq_01_2.58 : 0.6716715 

AUC and TSS values for lqph_01_2.58 :
AUC :  0.8395547 
TSS :  0.5243239 
Predict - Maxent  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s - 00:12:10.4

 Sauron is delving into the heart of your distribution models, searching for control... 

Thresholds Information (Maximum test sensitivity plus specificity) for lqph_01_2.58 :
Cloglog Value:  0.3266038 
Test Omission Rate:  0.02442939 

 Gandalf seeks insights in the arcane calculations of the Boyce Index, as formidable as facing the Balrog..
Boyce Index (CBI) for lqph_01_2.58 : 0.9036226 
Final results:
         Model       AUC       TSS Threshold   Omission       CBI
1   lq_01_2.58 0.7758050 0.3779693 0.5910706 0.05206847 0.6716715
2 lqph_01_2.58 0.8395547 0.5243239 0.3266038 0.02442939 0.9036226
```

## *rohirrim*

You will need a data.frame object that contains TP, TN, FP and FN information, which are obtained from a confusion matrix.

``` r
# Use the function rohirrim to calculate the auxiliary metrics

rohirrim(obj)

#The function provides a data.frame object with all the metricas calculated
  OPR   UPR    PPI    PAI
  0.5   0.5  0.3333 -0.3333
```

## 👾 Issues and Bugs

If you find any issues or bugs, please open an issue on the [Issues page](https://github.com/rogerio-bio/Fangorn/issues).

## References
Márcia Barbosa, A., Real, R., Muñoz, A.-.-R. and Brown, J.A. (2013), New measures for assessing model equilibrium and prediction mismatch in species distribution models. Diversity Distrib., 19: 1333-1338. https://doi.org/10.1111/ddi.12100.

Smith A, Murphy S, Henderson D, Erickson K (2023). “Including imprecisely georeferenced specimens improves accuracy of species distribution models and estimates of niche breadth.” Global Ecology & Biogeography, 32, -13. doi:10.1111/geb.13628. 

Vignali S, Barras AG, Arlettaz R, Braunisch V. SDMtune: An R package to tune and evaluate species distribution models. Ecol Evol. 2020; 10: 11488–11506. https://doi.org/10.1002/ece3.6786.
