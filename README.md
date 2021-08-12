Pedigree-Accumulation-2021
==========================

[![DOI](https://zenodo.org/badge/375413829.svg)](https://zenodo.org/badge/latestdoi/375413829)


This is a guidance document for the GitHub Repository “Pedigree-Accumulation-2021”, and it contains additional information to help guide researchers interested in reproducing the results in the manuscript “Pedigree Accumulation Analysis: Combining Methods from Community Ecology and Population Genetics for breeding adult estimation”. Below in a bulleted summary for the directory structure within the repository. Folder names are bulleted in Bold followed by one or more sentences that describe the contents in each folder. Sub-bullets indicate subfolders within each main directory.

- <strong>Known datasets/</strong>
  - <strong>Chinook salmon/</strong> *Contains an R script used to estimate Ns using Chao and Jackknife methods with subsets of offspring found in the pedigrees in Input/, which are subsequently used to generate a figure that is saved to Output/.*
    - <strong>Input/</strong> *Contains all assembled pedigrees, along with a summary table of adult and offspring sample sizes for the Chinook salmon empirical example.*
    - <strong>Output/</strong>*Contains all bootstrapped estimates and the figure created with those data.*
  - <strong>Lake Sturgeon/</strong> *Contains an R script used to estimate Ns using Chao and Jackknife methods with subsets of offspring found in pedigrees in Input/, which are subsequently used to generate a figure that is saved to Output/.*
    - <strong>Input/</strong> *Contains all assembled pedigrees for the Lake Sturgeon empirical example.*
    - <strong>Output/</strong> *Contains all bootstrapped estimates and the figure created with those data.*
- <strong>Simulations/</strong> *Contains seven different scripts that can be used to generate various types of data necessary to generated figures analyzed as part of the manuscript, all of which start with “Data for Figure”. The titles are relatively explicit, but the user can read the “ABOUT” section at the top of each script to determine their specific objective(s).  Note that each script saves the data to Output/. Accordingly, nearly all (n = 7 out of 9) scripts that were used to generate figures associated this the manuscript (i.e., those that begin with “Figure”) can be run without simulating new data. The “Figure” scripts also save the figures analyzed in the manuscript to Output/. The remaining 2 “Figure” scripts (used to generate figures S9 and S10) simulate the data and create the associated graphics in a single script. This folder also contains two other files: the first is simply an excel spreadsheet that contains a summary of the parameters evaluated for each type of simulation; the second is a set of six different functions that have now been formally created into an R library “mater”.  Please visit https://github.com/nicksard/mater for additional details.*
  - <strong>Output/</strong> *Contains three types of data. First, all figures associated with the manuscript are found in this folder. Second, all summary statistics about the breeding matrices for each type of simulation are saved here – all associated files start with “simulation”. Finally, the individual Chao and Jackknife estimates associated with each simulated breeding matrix are saved in this folder as well.*

Additional comment: \
Please note the working directory for all scripts will need to be changed to where ever the repository is implemented on a local machine.
