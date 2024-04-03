
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
Onering(model, "model_name", test, variables, p, bg, "maxSSS")

AUC and TSS values:
AUC :  0.7787997 
TSS :  0.3963085 
Predict - Maxent  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s - 00:03:25  

 The Black Riders are shadowing the paths revealed by your distribution analysis... 

Thresholds Information (Maximum test sensitivity plus specificity):
Cloglog Value:  0.3954995 
Test Omission Rate:  0.01282651 

 Frodo maps the impact of the Boyce Index, a burden as heavy as the One Ring.. 
Boyce Index (CBI): 0.6914735 
Final results:
        AUC       TSS Threshold   Omission       CBI
1 0.7787997 0.3963085 0.3954995 0.01282651 0.6914735

```
### *palantiris*

``` r
> palantiris(model_list, test, variables, p, bg, "maxSSS")

AUC and TSS values for lq_01_2.58 :
AUC :  0.7792154 
TSS :  0.3981394 
Predict - Maxent  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s - 00:03:25.9

 Sauron is delving into the heart of your distribution models, searching for control... 

Thresholds Information (Maximum training sensitivity plus specificity) for lq_01_2.58 :
Cloglog Value:  0.3659253 
Training Omission Rate:  0.1966161 
Boyce Index (CBI) for lq_01_2.58 : 0.7155504 

AUC and TSS values for lq_05_2.58 :
AUC :  0.7787997 
TSS :  0.3963085 
Predict - Maxent  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s - 00:03:22.6

 Treebeard is analyzing your request... 

Thresholds Information (Maximum training sensitivity plus specificity) for lq_05_2.58 :
Cloglog Value:  0.3595249 
Training Omission Rate:  0.1904901 
Boyce Index (CBI) for lq_05_2.58 : 0.6914735 
Final results:
       Model       AUC       TSS Threshold  Omission       CBI
1 lq_01_2.58 0.7792154 0.3981394 0.3659253 0.1966161 0.7155504
2 lq_05_2.58 0.7787997 0.3963085 0.3595249 0.1904901 0.6914735
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

## *rivendell* - This function was added in version 1.0.4

``` r
> rivendell(op, test, predictors, jaguar, bg, "maxSSS", remove_prediction = TRUE , identifier = "7.5")

AUC and TSS values for lqph_3.1_7.5 :
AUC :  0.763560275482655 
TSS :  0.382962525464953 
Predict - Maxent  ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  100% | ETA:  0s - 00:06:53.7
Thresholds Information (Maximum training sensitivity plus specificity) for lqph_3.1_7.5 :
Cloglog Value:  0.3858708 
Training Omission Rate:  0.1487696 
Boyce Index (CBI) for lqph_3.1_7.5 : 0.999988351776354

 Model       AUC       TSS Threshold  Omission       CBI       OPR        UPR      PPI        PAI
1  lqph_3.1_7.5 0.7635603 0.3829625 0.3858708 0.1487696 0.9999884 0.9344828 0.01101726 11.87206 -0.4550185
2    lq_0.6_7.5 0.7526314 0.3706172 0.3742082 0.1498881 0.9162143 0.9361958 0.01205490 12.01305 -0.4604223
3  lqph_4.1_7.5 0.7619091 0.3833312 0.3891604 0.1465324 0.9999767 0.9351796 0.01112553 12.01044 -0.4603222
```


## 👾 Issues and Bugs

If you find any issues or bugs, please open an issue on the [Issues page](https://github.com/rogerio-bio/Fangorn/issues).

## References
Márcia Barbosa, A., Real, R., Muñoz, A.-.-R. and Brown, J.A. (2013), New measures for assessing model equilibrium and prediction mismatch in species distribution models. Diversity Distrib., 19: 1333-1338. https://doi.org/10.1111/ddi.12100.

Smith A, Murphy S, Henderson D, Erickson K (2023). “Including imprecisely georeferenced specimens improves accuracy of species distribution models and estimates of niche breadth.” Global Ecology & Biogeography, 32, -13. doi:10.1111/geb.13628. 

Vignali S, Barras AG, Arlettaz R, Braunisch V. SDMtune: An R package to tune and evaluate species distribution models. Ecol Evol. 2020; 10: 11488–11506. https://doi.org/10.1002/ece3.6786.
