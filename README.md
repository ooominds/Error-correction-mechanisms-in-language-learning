# Error correction mechanisms in language learning

This repository contains the `R` code for the analyses reported in 

> Ez-zizi, A., Divjak, D., and Milin, P. (2023). Error-correction mechanisms in language learning: modeling individuals. To appear in *Language Learning*.

## Data

The data can be downloaded from The University of Birmingham Institutional Research Archive (UBIRA) at \<Link\>. Once downloaded, please place the .csv files in the folder named `Data`. 

`Data_train.csv` and `Data_test.csv` contains the data generated from the training and test phases of the language learning task. `Data_WM.csv`, `Data_SRT.csv` and `Demographics.csv` contains the data generated from the working memory task, implicit learning task and the demographic questionnaire, respectively. `events_abstract.csv` encodes the 29 abstract events used in the test phase and is needed for the analysis that compares the different learning strategies (see the section "Comparison_strategies" below).

## Code

There are 4 folders, within each of which scripts are named with prefix numbers to show the order in which they should be executed. All scripts start with a short description that explains what they do.

### I) Model_fitting

This folder contains the necessary code to replicate the results reported in the two sections "Learned noun-verb form association weights" and "Participant-model match rates" as well as to prepare some of the data that is necessary to run the other analyses that relate to model fitting. The script `7.Plots_learningrates.R` generates Figure S6.  

### II) Comparison_strategies

The script contained in this folder compares R-W against four rule-based strategies as discussed in the "Comparison between the Rescorla-Wagner model and other decision strategies" section and produces Figure 3.  

### III) Matches_with_RW

This is to produce the results reported in the sections: 
- Relationship between the model's activation-based measures and participants' choices and response times, including Table 3 (and its full version in Appendix S8) and Figure 4. 
- Level of agreement between participants through the lens of the model, including Figure 5. 

### IV) Individual_differences

This is to replicate the results reported in the "Relationship between model-fit quality and individual difference measures" section. First, run `1.prepare_data.R` to generate the data necessary to run the multiple regression analysis that uses the individual difference variables, then use `2.lm_analysis.R` to run the regression analysis. This should also produce Table 4, Table S9 and Figure 6.    

## Contributors

This package was written by Adnane Ez-zizi. Petar Milin provided the implementation of the Rescorla-Wagner algorithm (`all_learning.R`).

