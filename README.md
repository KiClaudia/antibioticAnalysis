# antibioticAnalysis
analyzing physiological variables for green iguana antibiotic study

# 6/10/2022
Rstudio testing
github testing

# 6/14/2022
created code stdizingdata.R for standardizing dROM and OXY values
In the future, we can use scale(x, center=TRUE, scale=TRUE). center is whether the mean should be subtracted, scale is whether to divide by std. did OSI too

Created new data sheet AbxMasterData_with_OSI.csv

AbxMasterData is data set without OSI, the original

# 7/21/2022
New script abx_mass in folder code, 2 way rm anova looking at effects of antibiotics on mass (only doing first two time points, not LPS challenge)
New script abx_totri "" ---> log transformed
New script abx_osi ""
New script abx_glucose ""
New script abx_bka, beta regression --> decimal transformed

# 7/22/2022
New script abxLPS_mass in folder code. 3 way rm anova looking at effects of antibiotics and LPS on mass over entire study
New script for OSI ''
New script for glucose ''
New script for totri ''

# 7/25/2022
New script for lysis. Non-parametric, need kruskal wallis
New script for agglutination, non-parametric, need kruskal wallis
New script for BKA, used beta regression

# 4/19/2023
We can't look at effect of just antibiotics because there is no Jan20 time point. 
New script abxLPS_igy, 3 way rm anova looking at effects of anx AND LPS over entire study
Updated in manuscript too (results)