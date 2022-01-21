# hopland-hunting
Exploration of Hopland hunter GPS data for McInturff et al forthcoming

**Methods**

We successfully recovered data from 308 of the 324 hunter loggers. We discarded an additional 82 duplicate tracks where two or more hunters traveled together for the duration of the hunt, as based on visual inspection of the tracks, to avoid pseudoreplication. [Alex, should we add anything else here?] This resulted in the use of 226 tracks for analysis. (227?)

We clipped all tracks to the study area to ensure we were capturing active hunting behavior (8 hunters left the property briefly in the midday).

We resampled all tracks to a fix rate of 3 minutes using the amt::track_resample() function. We feel this time scale is appropriate for fine-scale analysis of hunter decision-making and movement, and is also appropriate given GPS error and computational limitations. (Sensitivity analysis—can do models with 5 min, and/or 1 min, and see if our inferences differ meaningfully.)

We removed any points that was >1.2 km from the previous point, which corresponds to a movement speed of at least 15 mph given a 3 minute fix rate. This is the speed limit in the study area,  This threshold removed clear outliers due to GPS error and marked a clear cut-off in the speed distribution. This resulted in the removal of XX of XX points (XX%). 

We extracted spatial covariates at each point using the raster package in R.
