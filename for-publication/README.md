Data for "Contrasting patterns of risk from human and non-human predators shape temporal activity of prey"
---

As a globally widespread apex predator, humans have unprecedented lethal and non-lethal effects on prey populations and ecosystems. Yet compared to non-human predators, little is known about the drivers and consequences of human hunting behavior. Here, we characterized the hunting modes, habitat selection, and harvest success of 483 rifle hunters in California using high-resolution GPS data. We used Hidden Markov Models to characterize fine-scale behavior, and k-means clustering to group hunters by hunting mode, on the basis of their time spent in each behavioral state. Hunters exhibited three distinct and successful hunting modes (“coursing”, “stalking”, and “sit-and-wait”), with stalking as the most successful strategy. Across hunting modes, there was variation in patterns of selection for roads, topography, and habitat cover, with important differences in habitat use of successful and unsuccessful hunters across modes. Our study indicates that hunters can successfully employ a diversity of harvest strategies, and that hunting success is mediated by the interacting effects of hunting mode and landscape features. Such results highlight the breadth of human hunting modes, even within a single hunting technique, and lend insight into the varied ways that humans exert predation pressure on wildlife.

## Description of the data scripts

### Data files

#### Raw input data

##### **igotu_data_3min_covariates.csv**

Movement trajectories of hunters, subsampled to a 3 minute fix interval. Used as input in 01-identify-starting-parameters.R and 02-run-hmm-model.R. Each row corresponds to a unique GPS location, with associated metadata. Columns include: 

* ID: unique identifier of individual hunter movement trajectory
* Longitude
* Latitude
* DateTime: local date and time of the GPS fix, in Pacific Daylight Time
* Ruggedness: calculated with Vector Ruggedness Measure in ArcGIS; measure of ruggednes in neighborhood of 900 meters squared
* Viewshed: calculated with Viewshed 2 tool in ArcGIS; measure of relative amount of visible area in 400m buffer from point
* Road_Distance: distance to nearest road, in meters, Chaparral_120m: density of chaparral habitat around point, in 120 meter window
* Woodland_120m: density of woodland habitat around point, in 120 meter window

##### **igotu_metadata.csv**

Metadata associated with iGotU movement trajectories. Each row corresponds to a unique hunter GPS trajectory for which data were successfully recovered. Used as input in 04-success-models.R., 06-explore-harvest-times.R. Columns include: 

* ID: unique identifier of individual hunter movement trajectory
* Date: YYYY-MM-DD
* Year
* Harvest: Y = yes, N = no
* Harvest_time: time of deer harvest, HH:MM
* Hunt_type: Single_day or Multi_day

##### **all-available-point-cov.csv**

Covariate values for all available points in the study area, at a 30x30 meter resolution. Each row corresponds to a unique location (grid cell). Used in 05-rsf-by-behavior.R as input for Resource Selection Functions (available points sampled randomly for each individual hunter). Columns include: 

* Ruggedness: calculated with Vector Ruggedness Measure in ArcGIS; measure of ruggednes in neighborhood of 900 meters squared
* Viewshed: calculated with Viewshed 2 tool in ArcGIS; measure of relative amount of visible area in 400m buffer from point
* Road_Distance: distance to nearest road, in meters
* Chaparral_120m: density of chaparral habitat around point, in 120 meter window
* Woodland_120m: density of woodland habitat around point, in 120 meter window

#### Derived data

##### **hmm_data_with_model_predictions_annotated.csv**

Movement trajectories of hunters, subsampled to a 3 minute fix interval, with annotated model predictions and scaled covariates. Each row corresponds to a unique GPS location, with associated metadata. Used as input for 03-cluster-analysis.R. Created by 02-run-hmm-model.R. Columns include: 

* ID: unique identifier of individual hunter movement trajectory
* step: step length
* angle: turn angle
* x: longitude
* y: latitude
* DateTime: local date and time of the GPS fix, in Pacific Daylight Time
* Ruggedness: calculated with Vector Ruggedness Measure in ArcGIS; measure of ruggednes in neighborhood of 900 meters squared
* Viewshed: calculated with Viewshed 2 tool in ArcGIS; measure of relative amount of visible area in 400m buffer from point
* Road_Distance: distance to nearest road, in meters
* Chaparral_120m: density of chaparral habitat around point, in 120 meter window
* Woodland_120m: density of woodland habitat around point, in 120 meter window
* Elapsed_Time_Sunrise: elapsed time from sunrise, in hours
* Harvest: whether or not the trajectory was associated with a successful hunt, Y = Yes, N = No
* Ruggedness_scale, Viewshed_scale, Road_Distance_scale, Chaparral_120m_scale, Woodland_120m_scale, Sunrise_Scale: normalised covariates
* State: predicted most likely behavioral state from HMM: Driving, Stationary, Walking
* Stationary_Prob: probability that location is in stationary state
* Walking_Prob: probability that location is in walking state
* Driving_Prob: probability that location is in driving state
* state_2stationary: predicted most likey behavioral state from HMM, splitting stationary locations into Stationary_onroad and Stationary_offroad

##### **hunter_cluster_success_long.csv**

Information about behavioral states and hunting modes of hunters. There is one row for each behavioral state for each trajectory (long format). Used as input for 04-success-models.R, 05-rsf-by-behavior.R, and 06-explore-harvest-times.R. Created by 03-cluster-analysis.R. Columns include: 

* ID: unique identifier of individual hunter movement trajectory
* State: predicted most likely behavioral state from HMM: Driving, Walking, Stationary_onroad and Stationary_offroad
* Percentage: percentage of points in the trajectory that are predicted to be in that state
* Cluster
* Harvest: whether or not the trajectory was associated with a successful hunt, Y = Yes, N = No


### Scripts

##### **01-identify-starting-parameters.R**

Identifies the optimal starting parameters for the Hidden Markov Model (for 02-run-hmm-model.R), following the procedure outlined in this vignette: https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-starting-values.pdf Takes igotu_data_3min_covariates.csv as input.

##### **02-run-hmm-model.R**

Runs Hidden Markov Model to identify behavioral states associated with hunter GPS trajectories. Input file is igotu_data_3min_covariates.csv. Output file is data frame annotated with probable behavioral states: hmm_data_with_model_predictions_annotated.csv.

##### **03-cluster-analysis.R**

Conducts k-means clustering of individual hunter trajectories, based on the modeled time spent in each behavioral state (determined by HMM in 02-run-hmm-model.R). Input file is hmm-data-with-model-predictions-annotated.csv. Output file is hunter_cluster_success_long.csv.

##### **04-success-models.R**

Models explaining success of hunters, based on hunting mode, year, and single vs multi-day hunt. Input files are hunter_cluster_success_long.csv and igotu_metadata.csv.

##### **05-rsf-by-behavior.R**

Runs Resource Selection Functions to assess habitat selection for successful and unsuccessful hunters in each of the three hunting modes (Coursing, Stalking, Sit-and-Wait). Input files are all-available-point-cov.csv and igotu_data_3min_covariates.csv.

##### **06-explore-harvest-times.R**

Explores diel patterns of deer harvest by hunters across hunting modes. Scales times to radians based on solar time, and compares distributions using Anderson-Darling test. Input files are hunter_cluster_success_long.csv and igotu_metadata.csv.


## Sharing/access Information

The data was not derived from another source.
