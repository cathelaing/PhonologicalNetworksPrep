# Updated 30th March 2023

# Prepare data for sharing on OSF

source("0_prelims.R")

# global thresholds

globalthresholds_AOP_lyon <- feather::read_feather("Data/globalthresholds_AOP_lyon_rand.feather") %>% 
  filter(threshold == 0.25)
globalthresholds_AOP_providence <- feather::read_feather("Data/globalthresholds_AOP_providence_rand.feather") %>% 
  filter(threshold == 0.25) %>% 
  mutate(corpus = "English")  ## added to prep data, re-run and then remove in final script (don't remove threshold filter)
globalthresholds_AOP <- rbind(globalthresholds_AOP_lyon, globalthresholds_AOP_providence)

feather::write_feather(globalthresholds_AOP, "Data/repofiles/globalthresholds_AOP_rand.feather")

full_lyon <- feather::read_feather("Data/globalthresholds_AOP_lyon_rand.feather") %>% 
  filter(threshold == .99)
full_providence <- feather::read_feather("Data/globalthresholds_AOP_providence_rand.feather") %>% 
  filter(threshold == .99)
full_thresholds <- rbind(full_lyon, full_providence)

feather::write_feather(full_thresholds, "Data/repofiles/full_thresholds_rand.feather")

# correlation data for establishing connectivity

globalthresholds_corr_lyon <- feather::read_feather("Data/globalthresholds_corr_lyon_rand.feather") %>% 
  mutate(corpus = "French")
globalthresholds_corr_providence <- feather::read_feather("Data/globalthresholds_corr_providence_rand.feather") %>% 
  mutate(corpus = "English")
globalthresholds_corr <- rbind(globalthresholds_corr_lyon, globalthresholds_corr_providence) %>%
  write_csv("Data/repofiles/globalthresholds_corr_rand.csv")

all_distances_P <- feather::read_feather("Data/globaldistance_Providence_rand.feather") %>% 
  mutate(corpus = "English")
all_distances_L <- feather::read_feather("Data/globaldistance_Lyon_rand.feather") %>% 
  mutate(corpus = "French")

## too big to save on repo so create a small DF instead

# all_distances_ungrouped <- rbind(all_distances_L, all_distances_P) %>% 
#   dplyr::select(Speaker, age, data_type, distance_norm, corpus) %>% 
#   #group_by(corpus, data_type) %>%
#   summarise(mean_dist = mean(distance_norm),
#             sd_dist = sd(distance_norm),
#             med_dist = median(distance_norm)) %>%
#   mutate(Q1 = mean_dist-sd_dist,
#          corpus = "all",
#          data_type = "all") 
# 
# all_distances <- rbind(all_distances_L, all_distances_P) %>% 
#   dplyr::select(Speaker, age, data_type, distance_norm, corpus) %>% 
#   group_by(corpus, data_type) %>%
#   summarise(mean_dist = mean(distance_norm),
#             sd_dist = sd(distance_norm),
#             med_dist = median(distance_norm)) %>%
#   mutate(Q1 = mean_dist-sd_dist) %>%
#   rbind(all_distances_ungrouped) %>%
#   write_csv("Data/repofiles/all_distances_rand.csv")

## data is too big to include in main repo so generate it here and save as an image

rename_vars <- c(
  `actual` = "Actual",
  `target` = "Target"
)

distance_density_plot <- ggplot(all_distances, aes(x = distance_norm, fill = data_type)) +
  geom_density(alpha = 0.3) +
  geom_vline(xintercept = 0.25, linetype = "twodash", linewidth = 1, colour = "black") +
  xlab("Phonological distance (scaled)") +
  scale_fill_discrete(labels = rename_vars) +
  facet_wrap(~corpus, ncol=2) +
  theme_bw() +
  theme(legend.title = element_blank())

regression_data_P <- feather::read_feather("Data/regression_data_providence_rand.feather")
regression_data_L <- feather::read_feather("Data/regression_data_lyon_rand.feather")
regression_data_rand <- rbind(regression_data_L, regression_data_P) %>%
  write_feather("Data/repofiles/regression_data_rand.feather")

regression_data_L %>% filter(is.na(PAQ_scaled_target))
