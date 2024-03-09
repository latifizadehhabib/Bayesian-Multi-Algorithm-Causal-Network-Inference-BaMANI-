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
Installation
Clone the repository to your local machine:

git clone https://github.com/KlinkeLab/BaMANI.git
-------------------------------------------------
Navigate into the BaMANI directory:

cd BaMANI
-------------------------------------------------

Implementation: 
-------------------------------------------------

<p align="center">
<img src="images/login.png" alt="Login Page" title="Log in Page" width="600" height="300">
</p>

Step 1: Creating and Specifying a Blacklist
Objective: Input a blacklist to outline prior knowledge and remove implausible arcs.
Process: Specify nodes and arcs to be excluded based on domain knowledge.

<p align="center">
<img src="images/blacklist.png" alt="blacklist" title="blacklist Page" width="200" height="300">
</p>


Step 2: Generating an Ensemble of Potential Arcs
Upload your data file in CSV format, along with blacklist and whitelist files.
Choose from various structure learning algorithms and proceed with 'Run Discovery'.

<p align="center">
<img src="images/Setting.png" alt="Setting Page" title="Setting Page" width="600" height="300">
</p>

<p align="center">
  <img src="images/file_upload.png" alt="Upload Data" width="300" height="300"/>
  <img src="images/choose_dir_alg.png" alt="Choose Directed Algorithms" width="300" height="300"/>
</p>

Step 3: Filtering Potential Arcs
Create a whitelist based on BIC and network complexity, adjusting thresholds as needed.
The process involves setting arc strength thresholds and removing arcs that create cycles.

<p align="center">
<img src="images/acyclic.png" alt="Acyclicity" title="Acyclicity check before Final whitelist" width="600" height="300">
</p>

<p align="center">
<img src="images/final_whitelist.png" alt="Setting Page" title="final_whitelist" width="300" height="300">
 </p>


Step 4: Learning the Network Structure Utilize both the blacklist and whitelist to refine the network's structure.
Review the final network for consistency with domain knowledge. (Adjust the usage steps based on how users interact with your software, including command-line arguments, configuration files, etc.)



