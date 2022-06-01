
Simulation study - LCA with distal outcome using IPW 

## Authors
- Tra T. Le | Tilburg University
- Prof. dr. Jeroen K. Vermunt | Tilburg Univeristy (Supervisor)
- MSc. Felix Clouth | Tilburg University (Co-supervisor)

## Description
These are the supplementary materials to the First-year Paper written as part of the Research Master in Social and Behavioral Science at Tilburg University under the supervision of Prof.dr. Jeroen Vermunt and MSc. Felix Clouth. 

The repository consists of the codes for the **Exemplary Data Analysis**, LatentGOLD codes for the main **Simulation Study**, R codes to produce tables and figures, and step-by-step example of the process.

## Exemplary Data Analysis
The steps for the exemplary data analysis are as follows

1. Run create_exemplarydata.lgs to generate a synthetic data set 'exemplary.sav'.
2. Run step1model.lgs and save the posterior probabilities in 'classification.txt'
3. Run 1-stepmodel.lgs and save the posterior probabilities and classification results in 'classification2.txt'
4. Use 'classification.txt' and step3model.lgs 
- Run step3-adjusted (ML method and proportional assignment) to get propensity scores saved in 'propensity1.txt'
- Run step3-unadjusted (no correction method and modal assignment) to get propensity scores saved in 'propensity2.txt'
5. Run schuler.lgs with 'propensity2.txt' 
6. Run yamaguchi.lgs with 'classification2.txt'
7. Run braive-tra.lgs with 'propensity1.txt'

## Simulation study in batch mode
1. The GenData.lgs file was used to generate the data set ‘data.sav’.
2. The Step1OneStep.lgs file with the simulated data set ‘data.sav’ were used to:
- run Step 1 model (estimate LC model with only the response indicators) and get
the posterior probabilities in ‘data0.sav’.
- run One-step model (estimate LC model with the response indicators and the confounders) and get the posterior probabilities and classification results in ‘data1.sav’.
3. The Step3.lgs file with the posterior probabilities ‘data0.sav’ were used to:
- run the adjusted Step 3 model (ML correction method and proportional assignment) to get the propensity scores saved in ‘data2.sav’.
- run the standard Step 3 model (no correction method) to get the propensity scores saved in ‘data3.sav’.
4. The Schuler.lgs file with ‘data3.sav’ data set were used to run the final step of Schuler et al. (2014) method.
5. The Yama.lgs file with ‘data1.sav’ data set were used to run the final step of Yamaguchi (2015) method.
6. The Bray_Tra_Naive.lgs file with ‘data2.sav’ data set were used to run the final step of Bray et al. (2019), our method, and the naive method.

We used R to generate the above six LatentGOLD syntaxes and run them in batch mode in a loop for 500 replications as follows:
```
"~\ LatentGOLD6 .0\ lg60 " GenData.lgs Step1.lgs Step3.lgs Schuler.lgs Yamaguchi.lgs Bray_Tra_Naive.lgs / b / r 500
```
## R Codes
I provided the R Codes to generate LatentGOLD syntax files and read the results from LatentGOLD for each condition. The R Codes to generate graphs and tables are only for 1 condition so they need to be modified to use for other conditions by simply changing the working directory and file names to match the conditions.
