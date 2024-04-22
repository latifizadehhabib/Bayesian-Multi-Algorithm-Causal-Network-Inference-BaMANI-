# Bayesian-Multi-Algorithm-Causal-Network-Inference

BaMANI: Bayesian Multi-Algorithm Causal Network Inference
Introduction
BaMANI (Bayesian Multi-Algorithm Causal Network Inference) is an advanced software tool designed for Bayesian network inference. It leverages a variety of algorithms and incorporates user-defined constraints to improve the accuracy of inferring structure and causality within networks from observational data. BaMANI is invaluable for researchers and practitioners across domains such as biology, medicine, and social sciences, where understanding causal relationships is crucial.

Features
Blacklist Creation: Users can specify relationships to exclude from consideration based on domain knowledge.
Ensemble of Algorithms: Utilizes multiple structural learning algorithms to generate a comprehensive list of potential arcs.
Data File Upload: Supports CSV format for uploading datasets and constraint lists (blacklist/whitelist).
Interactive Visualization: Offers diagnostic plots, DAG networks, and conditional probability query visualization.
Customizable Filtering: Enables users to filter potential arcs based on strength and Bayesian Information Criterion (BIC) scores.

-------------------------------------------------
For the local machine: Run the "app.R" code
-------------------------------------------------
### Note for Users Facing Issues with Python Libraries

If you are trying to run the code and are having trouble even though you installed the pandas and dagma libraries, follow these steps. Before calling any Python-related commands, ensure that the correct virtual environment or Conda environment is activated with use_virtualenv() or use_condaenv().
    
1. Open "run_algorithm_directed.R" file
2. Uncomment this line   # use_python("C:/Users/AppData/Local/Programs/Python/python.exe", required = TRUE)
4. Change the path to where python.exe is located on your computer
5. Restart R Studio and run

-------------------------------------------------
