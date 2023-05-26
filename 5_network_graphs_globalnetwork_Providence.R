# Updated 24th February 2023

################################## SET UP #######################################

# Load data

globaldistance_providence <- feather::read_feather("Data/globaldistance_Providence.feather")
globalthresholds_providence <- feather::read_feather("Data/globalthresholds_Providence.feather")

first_instance <- read_csv("Data/first_instance_Providence.csv")

networksize <- globaldistance_providence %>%         # create a mock dataset that imitates the size of the network at each month
  group_by(Speaker, age) %>%
  distinct(gloss1, .keep_all = TRUE) %>%
  tally() %>%
  rename("numNodes" = "n") %>%
  mutate(age = as.numeric(age)) %>%
  tibble::rownames_to_column("L1")

# vocabsize_full <- vocabsize %>%
#   filter(age < 31) %>%
#   left_join(WPs) %>%
#   mutate(WP = ifelse((is.na(WP) & vocab_size > 5 & vocab_size < 25), 5, WP),
#          WP = ifelse((is.na(WP) & vocab_size > 25 & vocab_size < 50), 25, WP),
#          WP = ifelse((is.na(WP) & vocab_size > 50 & vocab_size < 100), 50, WP),
#          WP = ifelse((is.na(WP) & vocab_size > 100 & vocab_size < 300), 100, WP))

# Set colour properties

fine = 500 # this will adjust the resolving power for colour of nodes
pal = colorRampPalette(c('red','green'))   # set up colour palette for nodes
cut.off <- .25  # set threshold cut off

# colour palette legend:

color_num = 0:max(globalthresholds_providence$degree) #create a color palette of the same size as the number of vertices.
color_spectrum <- pal(length(unique(color_num)))
ordered <- order(color_num) # map the pallete to the order of values on vertices
color <- vector(length = length(ordered), mode="double")

for(i in 1:length(ordered)){
  color[ordered[i]] <- color_spectrum [i]
}

############################### CREATE DATAFRAMES ##################################

ages <- globalthresholds_providence %>% filter(data_type == "target") %>% dplyr::select(Speaker, gloss1, age)

age_summ <- ages %>%
  mutate(subj_session = paste(Speaker, age, sep="_"),
         age = as.numeric((age)))

age_list <- age_summ %>%
  split(., f = .$subj_session)

globaldistance_actual_base <- globaldistance_providence %>%              # create edges
  filter(data_type == "actual") %>%
  distinct(Speaker, gloss1, age, distance, .keep_all = TRUE) %>%
    rename("from" = "gloss1",
         "to" = "gloss2",
         "weight" = "distance_norm")

globaldistance_target_base <- globaldistance_providence %>%             # create edges
  filter(data_type == "target") %>%
  distinct(Speaker, gloss1, age, distance, .keep_all = TRUE) %>%
  rename("from" = "gloss1",
         "to" = "gloss2",
         "weight" = "distance_norm")

globaldistance_degree_target <- globalthresholds_providence %>%         # create nodes base df
  filter(data_type == "target" & age == 30) %>%
  dplyr::select(Speaker, gloss1, degree) %>%
  left_join(first_instance) %>%
  rename("from" = "gloss1") %>%
  mutate(AOP = as.numeric(AOP),
         degree = as.numeric(degree))# %>%
  #left_join(WPs) #%>%
  #feather::write_feather("Data/globaldistance_degree_target_RED.feather")
 # feather::write_feather("Data/globaldistance_degree_target_providence.feather")

globaldistance_degree_actual <- globalthresholds_providence %>%        # create nodes base df
  filter(data_type == "actual" & age == 30) %>%
  dplyr::select(Speaker, gloss1, degree) %>%
  left_join(first_instance) %>%
  rename("from" = "gloss1") %>%
  mutate(AOP = as.numeric(AOP),
         degree = as.numeric(degree))# %>%
  #left_join(WPs)# %>%
  #feather::write_feather("Data/globaldistance_degree_actual_RED.feather") 
  #feather::write_feather("Data/globaldistance_degree_actual_providence.feather")

globalnodes_target <- globaldistance_target_base %>%      # create nodes
  select(-to) %>% 
  distinct(from, Speaker, age, .keep_all = TRUE) %>% 
  mutate(age = as.numeric(age))

globalnodes_actual <- globaldistance_actual_base %>%     # create nodes
  select(-to) %>% 
  distinct(from, Speaker, age, .keep_all = TRUE) %>% 
  mutate(age = as.numeric(age))              

########################### DATA LOOP ##################################

# Create graph data for each child
# in the global network, the data is considered at each month + all previous months, as specified in age <= element$age

globalgraphdata_actual <- lapply(age_list, FUN = function(element) {
  edges_net <- globaldistance_actual_base %>% filter(Speaker == element$Speaker & age == element$age) %>% distinct(word_pair, distance, .keep_all = TRUE)
  nodes_net <- globalnodes_actual %>% filter(Speaker == element$Speaker & age == element$age)
  net_plot <- graph_from_data_frame(d=edges_net, vertices=nodes_net, directed=F) 
  net_plot_threshold <- delete.edges(net_plot, which(E(net_plot)$weight > cut.off))    # delete edges with a threshold above .25
  })

#plot(globalgraphdata_actual$William_18)

globalgraphdata_target <- lapply(age_list, FUN = function(element) {
  edges_net <- globaldistance_target_base %>% filter(Speaker == element$Speaker & age == element$age) %>% distinct(word_pair, distance, .keep_all = TRUE)
  nodes_net <- globalnodes_target %>% filter(Speaker == element$Speaker & age == element$age)
  net_plot <- graph_from_data_frame(d=edges_net, vertices=nodes_net, directed=F) 
  net_plot_threshold <- delete.edges(net_plot, which(E(net_plot)$weight > cut.off))    # delete edges with a threshold above .25
})

# par(mfrow=c(1,2))
# plot(globalgraphdata_actual$Alex_19)
# plot(globalgraphdata_target$Alex_20)

# save(globalgraphdata_actual, file="Data/globalgraphdata_actual_providence.Rdata")
# save(globalgraphdata_target, file="Data/globalgraphdata_target_providence.Rdata")


######################## Calculating small-world properties - age data #####################

# Average path length for each subject in each session

globalpathlength_actual <- lapply(globalgraphdata_actual, FUN = function(element) {
  path_length <- mean_distance(element, directed = F)
  mean_k <- mean(degree(element))
  output <- merge(path_length, mean_k)
})

globalpathlength_actual_base <- melt(globalpathlength_actual) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("path_length" = "x",
         "mean_k" = "y") %>%
  separate(L1, into = c("Speaker", "age"), sep = "_") %>%
  mutate(data_type = "actual") 


globalpathlength_target <- lapply(globalgraphdata_target, FUN = function(element) {
  path_length <- mean_distance(element, directed = F)
  mean_k <- mean(degree(element))
  output <- merge(path_length, mean_k)
})

globalpathlength_target_base <- melt(globalpathlength_target) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  rename("path_length" = "x",
         "mean_k" = "y") %>%
  separate(L1, into = c("Speaker", "age"), sep = "_") %>%
  mutate(data_type = "target") 

globalpathlength <- rbind(globalpathlength_actual_base, globalpathlength_target_base)

globalpathlength <- globalpathlength %>% 
  mutate(age = as.numeric(age)) %>%
  left_join(networksize) %>%
  dplyr::select(-L1) %>%
  mutate(lowerCI = NA,
         upperCI = NA,
         lowerQuantile = NA,
         upperQuantile = NA)

feather::write_feather(globalpathlength, "Data/globalpathlength_providence.feather")


# Clustering coefficient for each subject in each session

globalclusteringcoef_actual_global <- lapply(globalgraphdata_actual, FUN = function(element) {
  global_clustering_coef <- transitivity(element)
})

globalclusteringcoef_actual_global_base <- melt(globalclusteringcoef_actual_global) %>%
  rename("clust_coef_global" = "value") %>%
  separate(L1, into = c("Speaker", "age"), sep = "_") %>%
  mutate(data_type = "actual",
         age = as.numeric(age))

globalclusteringcoef_actual_avg <- lapply(globalgraphdata_actual, FUN = function(element) {
  average_clustering_coef <- transitivity(element, type = "average")
})

globalclusteringcoef_actual_avg_base <- melt(globalclusteringcoef_actual_avg) %>%
  rename("clust_coef_avg" = "value") %>%
  separate(L1, into = c("Speaker", "age"), sep = "_") %>%
  mutate(data_type = "actual",
         age = as.numeric(age))

globalclusteringcoef_target_global <- lapply(globalgraphdata_target, FUN = function(element) {
  global_clustering_coef <- transitivity(element)
})

globalclusteringcoef_target_global_base <- melt(globalclusteringcoef_target_global) %>%
  rename("clust_coef_global" = "value") %>%
  separate(L1, into = c("Speaker", "age"), sep = "_") %>%
  mutate(data_type = "target",
         age = as.numeric(age))

globalclusteringcoef_target_avg <- lapply(globalgraphdata_target, FUN = function(element) {
  average_clustering_coef <- transitivity(element, type = "average")
})

globalclusteringcoef_target_avg_base <- melt(globalclusteringcoef_target_avg) %>%
  rename("clust_coef_avg" = "value") %>%
  separate(L1, into = c("Speaker", "age"), sep = "_") %>%
  mutate(data_type = "target",
         age = as.numeric(age))

globalclusteringcoef_global <- rbind(globalclusteringcoef_actual_global_base, globalclusteringcoef_target_global_base)
globalclusteringcoef_avg <- rbind(globalclusteringcoef_actual_avg_base, globalclusteringcoef_target_avg_base)


globalsmallworlddata <- globalpathlength %>% 
  left_join(globalclusteringcoef_global) %>%
  left_join(globalclusteringcoef_avg) 

# Random graphs for comparison

# Erdos Reyni

global_ErdosRenyi_sample <- lapply(networksize$numNodes, FUN = function(element) {
  erdos_reyni <- sample_gnp(element, 0.05, directed = FALSE, loops = FALSE)
})

global_ErdosRenyi_path <- lapply(global_ErdosRenyi_sample, FUN = function(element) {
  path_length <- mean_distance(element, directed = F)
})

global_ErdosRenyi_path_base <- melt(global_ErdosRenyi_path) %>%
  rename("path_length" = "value") %>%
  mutate(data_type = "Erdos_Renyi",
         L1 = as.character(L1)) %>%
  left_join(networksize) %>%
  dplyr::select(-L1)

global_ErdosRenyi_meank <- lapply(global_ErdosRenyi_sample, FUN = function(element) {
  mean_k <- mean(degree(element))
})

global_ErdosRenyi_meank_base <- melt(global_ErdosRenyi_meank) %>%
  rename("mean_k" = "value") %>%
  mutate(data_type = "Erdos_Renyi",
         L1 = as.character(L1)) %>%
  left_join(networksize) %>%
  dplyr::select(-L1)

global_ErdosRenyi_clusteringcoef_avg <- lapply(global_ErdosRenyi_sample, FUN = function(element) {
  average_clustering_coef <- transitivity(element, type = "average")
})

global_ErdosRenyi_clusteringcoef_avg_base <- melt(global_ErdosRenyi_clusteringcoef_avg) %>%
  rename("clust_coef_avg" = "value") %>%
  mutate(data_type = "Erdos_Renyi",
         L1 = as.character(L1)) %>%
  left_join(networksize) %>%
  dplyr::select(-L1)

global_ErdosRenyi_clusteringcoef_global <- lapply(global_ErdosRenyi_sample, FUN = function(element) {
  average_clustering_coef <- transitivity(element)
})

global_ErdosRenyi_clusteringcoef_global_base <- melt(global_ErdosRenyi_clusteringcoef_global) %>%
  rename("clust_coef_global" = "value") %>%
  mutate(data_type = "Erdos_Renyi",
         L1 = as.character(L1)) %>%
  left_join(networksize) %>%
  dplyr::select(-L1)

globalErdosRenyi <- global_ErdosRenyi_clusteringcoef_avg_base %>%
  left_join(global_ErdosRenyi_meank_base) %>%
  left_join(global_ErdosRenyi_clusteringcoef_global_base) %>%
  left_join(global_ErdosRenyi_path_base) %>%
  mutate(lowerCI = NA,
         upperCI = NA,
         lowerQuantile = NA,
         upperQuantile = NA)

# Watts Strogatz - actual forms

# I'm having some difficulty making a decision about what k to set each small-world network. It seems to make most sense to set it according to the mean k for each 
# month's data; this might have to change when I run this analysis on the global network

globalpathlength_T <- globalpathlength %>% 
  left_join(networksize) %>% filter(data_type == "target") %>%
  #rename("network_size" = "n") %>%
  mutate(subj_session = paste(Speaker, age, sep="_"))

global_WattsStrogatz_sample_target <- lapply(1:nrow(globalpathlength_T), FUN = function(row) { 
  element <- as.data.frame(globalpathlength_T[row, ])
  size <- (element$numNodes)-1
  nei <- element$mean_k
  smallworldnetwork <- sample_smallworld(1, size+1, nei, 0.05, loops = FALSE, multiple = FALSE)
  path_length <- mean_distance(smallworldnetwork, directed = F)
  mean_k <- mean(degree(smallworldnetwork))
  clust_coef_avg <- transitivity(smallworldnetwork, type = "average")
  clust_coef_global <- transitivity(smallworldnetwork)
  output <- rbind(path_length, mean_k, clust_coef_avg, clust_coef_global)
})

global_WattsStrogatz_data_target <- melt(global_WattsStrogatz_sample_target) %>%          
  mutate(L1 = as.character(L1)) %>%
  pivot_wider(names_from = Var1, values_from = value) %>%
  left_join(networksize, by = "L1") %>%
  mutate(data_type = "WS_target")  %>%
  dplyr::select(-L1, -Var2)

globalpathlength_A <- globalpathlength %>% 
  left_join(networksize) %>% filter(data_type == "actual") %>%
  #rename("network_size" = "n") %>%
  mutate(subj_session = paste(Speaker, age, sep="_"))

global_WattsStrogatz_sample_actual <- lapply(1:nrow(globalpathlength_A), FUN = function(row) { 
  element <- as.data.frame(globalpathlength_A[row, ])
  size <- (element$numNodes)-1
  nei <- element$mean_k
  smallworldnetwork <- sample_smallworld(1, size+1, nei, 0.05, loops = FALSE, multiple = FALSE)
  path_length <- mean_distance(smallworldnetwork, directed = F)
  mean_k <- mean(degree(smallworldnetwork))
  clust_coef_avg <- transitivity(smallworldnetwork, type = "average")
  clust_coef_global <- transitivity(smallworldnetwork)
  output <- rbind(path_length, mean_k, clust_coef_avg, clust_coef_global)
})

global_WattsStrogatz_data_actual <- melt(global_WattsStrogatz_sample_actual) %>%         
  mutate(L1 = as.character(L1)) %>%
  pivot_wider(names_from = Var1, values_from = value) %>%
  left_join(networksize, by = "L1") %>%
  mutate(data_type = "WS_actual")  %>%
  dplyr::select(-L1, -Var2)

globalWattsStrogatz <- rbind(global_WattsStrogatz_data_target, global_WattsStrogatz_data_actual) %>%
  mutate(lowerCI = NA,
         upperCI = NA,
         lowerQuantile = NA,
         upperQuantile = NA)

globalsmallworlddata_comparison <- rbind(globalsmallworlddata, globalWattsStrogatz, globalErdosRenyi)

feather::write_feather(globalsmallworlddata_comparison, "Data/globalsmallworlddata_comparison_providence.feather")



