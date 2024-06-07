



# DBSCAN clustering -------------------------------------------------------

eps <- n_clusters_dbscan(males_minimal, min_size = 0.1)
eps
plot(eps)

rez_dbscan <- cluster_analysis(males_minimal, method = "dbscan", dbscan_eps = 0.5)

dbscan_class <- predict(rez_dbscan)  %>% 
  as_tibble() %>% 
  bind_cols(males_minimal)

ggplot(dbscan_class,  #%>% 
       # filter(value!=0),
       aes(x = crpc_width, y = claw_height, color=as.factor(value))) +
  geom_point() +
  labs(x="Carapace width", y="Claw height", color="Cluster")+
  mytheme