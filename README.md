# hopland-hunting
Exploration of Hopland hunter GPS data for McInturff et al forthcoming

### Methods

#### Hunter data

_Include information on data collection from dissertation__

We successfully recovered data from 307 of the 324 hunter loggers. We discarded an additional 82 duplicate tracks where two or more hunters traveled together for the duration of the hunt, as based on visual inspection of the tracks, to avoid pseudoreplication. [Alex, should we add anything else here?] This resulted in the use of 225 tracks for analysis.

We clipped all tracks to the area in which hunting was permitted, to ensure we were capturing active hunting behavior. We resampled all tracks to a fix rate of 3 minutes using the amt::track_resample() function. We believe this time scale is appropriate for fine-scale analysis of hunter decision-making and movement, and is also appropriate given GPS error and computational limitations. We then removed any point that was >1.2 km from the previous point, which corresponds to a movement speed of at least 15 mph given a 3 minute fix rate. This is the speed limit in the study area, and also corresponds to a speed threshold at which drivers can no longer be considered to be actively hunting (searching for deer). This cut-off removed clear outliers due to GPS error or locations where the hunters were leaving the property at higher speeds. This resulted in the removal of 657 of 46,383 points (1.4%), leaving 45,726 steps for analysis. 


#### Spatial data

_Describe information on spatial covariates_

We generated rasters corresponding to environmental features that we expected to influence hunter behavior.
- Distance to road
- Viewshed
- Density of different vegetation types (woodland, grassland, chaparral) within 150 meters
- Ruggedness

We quantified ruggedness at three different spatial scales (_describe what 9, 25, 49 correspond to_). The three measures of ruggedness were highly correlated with one another (r > 0.70). Preliminary modeling indicated that rugged9(?) resulted in the best-fit models of hunter behavior, and we therefore used this fine-scale measurement of ruggedness in all subsequent modeling. Grassland density and woodland density were also correlated, and we dropped grassland density as preliminary modeling suggested that woodland density was a better predictor of hunter behavior.

We extracted spatial covariates at each point using the raster package in R, and we calculated the elapsed time since sunrise for each point. We scaled and centered all covariates prior to modeling. 

#### Data analysis

To explore behavioral patterns of hunters, we used the moveHMM package to fit a hidden Markov model to the hunter movement data. We ran a global model with all predictors (distance to road, viewshed, ruggedness, woodland density, chaparral density, and time since sunrise).

We assigned movement points to one of three behavioral states, as exploratory modeling suggested that three-state models performed better than two-state models (based on AIC), and best corresponded to our understanding of hunter behavior. The three states correspond to (1) resting or processing harvested deer, (2) moving on foot, and (3) moving by vehicle. 

We followed guidance from the moveHMM package authors when choosing initial parameter values (Michelot & Langrock 2019 - https://cran.r-project.org/web/packages/moveHMM/vignettes/moveHMM-starting-values.pdf). We included a zero mass parameter for step length given the high proportion of step lengths equal to 0 (18% of all steps). To determine whether our models were sensitive to initial parameter choice, we ran 100 iterations of the null model with randomly-chosen starting parameters for step length mean, step length standard deviation, step length zero mass, and turning angle concentration. Our model converged on the same parameters for 68 of 100 of the iterations, and this model was the best-fitting (i.e., with the largest likelihood), indicating numerical stability. We then used the parameter values from the best model as our starting values for all subsequent modeling. 

We used the viterbi function in the moveHMM package to determine the most probable behavioral state at each step for each hunter, based on the global model. We then determined the percentate of time that each hunter spent in each of the three behavioral states.

We then used k-means clustering to group hunters by hunting mode, on the basis of their time spent in each behavioral state. We determined the optimal value of k using the elbow method heuristic (we plotted the total within-cluster sum of squares as a function of k, and determined the value of k at which this sum of squares began declining linearly). We then compared harvest success rates across hunting modes.


### Results

Based on our interpretation of step length and turn angle parameters, State 1 corresponds to a stationary state (searching, resting, or processing deer), State 2 corresponds to walking on foot, and State 3 corresponds to driving in a vehicle. We found that, overall, hunters spend 32% of time in the stationary state, 27% of time moving on foot, and 41% of time searching by vehicle. Model pseudoresiduals for step length and turn angle were normally distributed, indicating suitable model fit.

We identified three clusters of hunting mode, based on k-means clustering. The first cluster of hunters, which we are calling the "drivers" (n=85), spent on average 59% of the time driving (compared to 21% stationary and 20% walking). The second cluster of hunters, the "waiters" (n=75), spent 49% of the time stationary (compared to 19% walking and 32% driving). The third cluster of hunters, the "walkers" (n=65), spent 45% of the time walking (compared to 27% stationary and 28% driving). 

The success of hunters in harvesting a deer did not vary across the three hunting modes. The harvest rate was 9.1% for "drivers," 9.0% for "walkers," and 11.0% for "sitters."
