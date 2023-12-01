# no covariate model

model_orig <- read.csv("hmm_data_with_model_predictions_annotated.csv") %>% 
    dplyr::select(ID, DateTime, state_2stationary) %>% 
    dplyr::mutate(DateTime = as.POSIXct(DateTime)) %>% 
    dplyr::rename(state_2stationary_orig = state_2stationary)
model_nocov <- read.csv("hmm_data_with_model_predictions_annotated_nocov.csv") %>% 
    dplyr::select(ID, DateTime, state_2stationary) %>% 
    dplyr::mutate(DateTime = as.POSIXct(DateTime)) %>% 
    dplyr::rename(state_2stationary_nocov = state_2stationary)

test <- dplyr::left_join(model_orig, model_nocov)

test %>% 
    count(state_2stationary_orig, state_2stationary_nocov)

# compare clusters
cluster_orig <- read.csv("hunter_cluster_success_long.csv") %>% 
    dplyr::select(ID, Cluster, Harvest) %>% 
    dplyr::rename(Cluster_orig = Cluster) %>% 
    unique()
cluster_nocov <- read.csv("hunter_cluster_success_long_nocov.csv") %>% 
    dplyr::select(ID, Cluster, Harvest) %>% 
    dplyr::rename(Cluster_nocov = Cluster) %>% 
    unique()
cluster <- dplyr::left_join(cluster_orig, cluster_nocov)

cluster %>% 
    count(Cluster_orig, Cluster_nocov)
