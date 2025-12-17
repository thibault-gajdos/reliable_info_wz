# Reliability of Information

This repository contains the code to preprocess, analyze, and conduct modelling on the behavioural data collected to investigate how people integrate the reliability of information in decision-making.

## Repository structure
```text                                                
|   analysis_fit.R                                              # Conduct the analysis of fitted Stan models
|   analysis_loo.R                                              # Conduct LOO analysis and plot the results
|   analysis_preliminary.R                                      # Do preliminary analysis, statistics, and visualizations on the preprocessed data
|   fit.R                                                       # Fit the preprocessed data to Stan models and save the results
|   preprocessing.R                                             # Preprocess the data and save it to results
|   README.md
|
+---data                                                        # Where the behvaioural data are
|
+---results                                                     # Where the saved results are
|   |   data_preprocessed.rdata                                 # Preprocessed data saved
|   |   performance.rdata                                       # Performance data saved to be analyzed in preliminary analysis
|
\---stan                                                        # Where the Stan models are
        basic.stan
        basic_seq.stan
        log_basic.stan
        log_seq_basic.stan
```
