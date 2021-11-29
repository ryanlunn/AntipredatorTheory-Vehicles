# AntipredatorTheory-Vehicles
R scripts, supplementary material, and data needed to reproduce the results of Lunn et al. (Pre-print/In Submission).


## Code 

**S5_Code_Generating_Quantitative_Predictions.R:** The code used to generate quantitative predictions for each model with data from DeVault et al. 2015.  

**S5_Code_Simulation.R:** The code used to evaluate the FEAR Hypothesis, Looming Stimulus Hypothesis, and Bayesian optimal escape model sensitivity to vehicle approach speed for different behavioral rules of escape. The raw data from the simulation evaluation displayed in the paper is available upon request. 


## Data

**DeVaultData.csv:** Empirical data from DeVault et al. 2015 used to generate quantitative predictions for different models of escape behavior. See *S5_Code_Generating_Quantitative_Predictions.R*

**FID_Speed_Review.csv:** The data compiled for the review of the literature on how different species FID at different approach speeds. The dataset was used to parameterize the simulation of different FID at different approach speeds in *S5_Code_Simulation.R*. The column ***"Species"*** refers to the common name of the species found in different papers with data on FID and approach speed. The column ***"n"*** refers to number of observations for that species reported in the paper.The column ***"Stimulus Type"*** refers to the type of threat (i.e., Car, Human, Cyclist, etc) that approached the species eliciting an escape response. The column ***"Speeds"*** refers to the speed in meters per second at which a species was approached. The column ***"FID"*** refers to the distance in meters at whicha  spieces escaped from an approaching threat. The column ***"sd"*** refers to the reported standard deviation in FID when a species was approached multiple times. The columns ***"min"*** and ***"max"*** refers to the  the minmum and maximum FID reported for different species in a given study. The column ***"Paper"*** refers to the study in which the data was drawn from. 

**species_mass.csv:** Empirical data on the mass of different species from Dunning et al.2007. 


## Manuscript

**Review_MS_Final.pdf:** The orginal manuscript describing the purpose, methods, and findings for the review *"Can we use anti-predator behavior theory to predict wildlife responses to high-speed vehicles?"* by Ryan Lunn, Bradley Blackwell, Travis DeVault, and Esteban Fernández-Juricic. ***CAUTION:The article is currently in the submission process and has not yet been peer-reviewed.*** 

**Review_Figure_Final.pdf:** The orginal figures which accompany the manuscript for the study *"Can we use anti-predator behavior theory to predict wildlife responses to high-speed vehicles?"* by Ryan Lunn, Bradley Blackwell, Travis DeVault, and Esteban Fernández-Juricic. 


## Supplementary Materials

**Supplementary_Materials_1.pdf:** An overview of the hypothesis, predictions, and assumptions for each model of antipredator escape behavior reviewed. When applicable we show graphs with the quantitative predictions at different approach speeds. The code use to produce these figures and predictions can be found in *S5_Code_Generating_Quantitative_Predictions.R:*

**Supplementary_Materials_2.pdf:** A list of the papers for which we had empirical data on FID for different approach speeds. We provide information and visualization of the AD, FID, and slope and intercept of the relationship between FID and approach speed distributions. The   

**Supplementary_Materials_3.pdf:** The results of the simulation used to estimate the slope range for a spatial margin of safety. 

**Supplementary_Materials_4.pdf:** A figure demonstrating the concept of critical vehicle approach speed discussed in the manuscript. 

**Supplementary_Materials_5.pdf:** The files *S5_Code_Generating_Quantitative_Predictions.R:* and *S5_Code_Simulation.R:* in pdf. format






