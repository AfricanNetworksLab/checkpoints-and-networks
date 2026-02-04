#African road network centrality
#Lacey Harris-Coble 1/26/2026
#network construction code adapted from https://r-spatial.org/r/2019/09/26/spatial-networks.html


#install libraries
library(tidyverse)
library(sf)
library(sfnetworks)
library(tidygraph)


#load road network shapefile
africa_road = st_read("C:/Users/lharriscoble/OneDrive - University of Florida/Downloads/projected (1)/2017proj.shp")

#visually check that shapefile loaded correctly
plot(africa_road)



#construct road network
#create unique edge IDS
edges = africa_road %>%
  mutate(edgeID = c(1:n()))

edges

#create nodes at the beginning and end of each edge ID
nodes = edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L1) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))

nodes

#give each node a unique ID
nodes = nodes %>%
  mutate(xy = paste(.$X, .$Y)) %>% 
  mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  select(-xy)

nodes

#combine node and edge indices
source_nodes = nodes %>%
  filter(start_end == 'start') %>%
  pull(nodeID)

target_nodes = nodes %>%
  filter(start_end == 'end') %>%
  pull(nodeID)

edges = edges %>%
  mutate(from = source_nodes, to = target_nodes)

edges

#remove duplicate nodes
nodes = nodes %>%
  distinct(nodeID, .keep_all = TRUE) %>%
  select(-c(edgeID, start_end)) %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs(st_crs(edges))

nodes


#create network graph with tbl_graph
graph = tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)

graph


#calculate node centrality metrics
africa_road_net = graph %>%
  activate("nodes") %>%
   mutate(cen_deg = tidygraph::centrality_degree(),
         cen_bet = tidygraph::centrality_betweenness(),
         cen_eig = tidygraph::centrality_eigen())


#create normalized centrality metrics - only works on df data type
#min-max normaliztion
#convert graph to dataframe
africa_df = as.data.frame(africa_road_net)


#function for min-max normalization
normalize <- function(x) {
  return((x- min(x)) /(max(x)-min(x)))
}


#calculate z-score normalization
#issue with functions returning a matrix, need to convert to vector before appending to df
centrality_degree_min_max = normalize(africa_df$cen_deg)
centrality_degree_zscore = scale(africa_df$cen_deg)
centrality_degree_zscore_vec = as.vector(centrality_degree_zscore)

centrality_betweenness_min_max = normalize(africa_df$cen_bet)
centrality_betweenness_zscore = scale(africa_df$cen_bet)
centrality_betweenness_zscore_vec = as.vector(centrality_betweenness_zscore)

centrality_eigenvector_min_max = normalize(africa_df$cen_eig)
centrality_eigenvector_zscore = scale(africa_df$cen_eig)
centrality_eigenvector_zscore_vec = as.vector(centrality_eigenvector_zscore)

africa_df$deg_norm = centrality_degree_min_max
africa_df$deg_zscore = centrality_degree_zscore_vec
africa_df$bet_norm = centrality_betweenness_min_max
africa_df$bet_zscore = centrality_betweenness_zscore_vec
africa_df$eig_norm = centrality_eigenvector_min_max
africa_df$eig_zscore = centrality_eigenvector_zscore_vec


#convert from dataframe back to sf object
africa_road_net_2 = st_as_sf(africa_df)
africa_road_net_3 = as_sfnetwork(africa_road_net_2)


#write shapefiles
graph_nodes = africa_road_net_3 %>% activate(nodes) %>% as_tibble() %>% st_as_sf()
graph_edges = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf()

st_write(graph_nodes, "nodes_africa_road_net_3.shp")
st_write(graph_edges, "edges_africa_road_net_3.shp")

