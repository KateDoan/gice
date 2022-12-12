This repository contains the code for the project: Evaluating tobacco control policies in e-cigarette era: a modelling study

1. Please set the working directory in R to this folder (i.e. the parent folder containing sub-folders: code, data, references) before running the codes

2. In the "code" sub-directory:
+ "figures" contains the code to plot the figures for the paper
+ "scenario_macro" contains the code for the macro-simulation of the scenarios
+ "scenario_micro" contains the code for the micro-simulation of the scenarios
+ "tables" contains the code for the table in the appendices of the paper
+ "transitions" contains the code to find the transition probabilities

The order that I ran the code is illustrated in the file code_run_sequence

3. In the "data" sub-directory:
+ "japan", "singapore", "uk" contain the data for Japan, Singapore, and the UK respectively
+ "PATH1" contain the data for the US PATH 1 study, up to Wave 3
+ "singapore" contains smoking_data file which is the smoking data up to 2017 that we obtained from MOH

4. The main reference for the micro-simulation code is the paper:
https://www.ncbi.nlm.nih.gov/pubmed/29587047

  Krijkamp, E. M., Alarid-Escudero, F., Enns, E. A., Jalal, H. J., Hunink, M. G. M., & Pechlivanoglou, P. (2018). Microsimulation Modeling for Health Decision Sciences Using R: A Tutorial. Medical Decision Making, 38(3), 400–422. https://doi.org/10.1177/0272989X18754513

5. Links for JAGS:
+ JAGS manual: https://web.sgh.waw.pl/~atoroj/ekonometria_bayesowska/jags_user_manual.pdf

+ JAGS Coursera: https://www.coursera.org/learn/mcmc-bayesian-statistics#syllabus

+ NOTE: You may want to use Stan instead of JAGS (https://mc-stan.org/)
