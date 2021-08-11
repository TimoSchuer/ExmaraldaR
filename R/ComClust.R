com_clust <- function(data,variables = c(1:ncol(data)),weight= 1){
  data[,variables] <- data[,variables] %>% dplyr::mutate(dplyr::across(where(purrr::negate(is.factor)), as.factor))
  adjmatrix <- 1- cluster::daisy(data[,variables], metric = c("gower"), weights = weight) # ungewichteter Vergleich, ausgenommen e0, e1, da nur ein auftreten überhaupt

  x <- as.matrix(adjmatrix) #schreibt Aehnlichkeitsmatrix in ein R Objekt

  g1 <- igraph:::graph_from_adjacency_matrix(x, mode = c("lower"), weighted = TRUE, diag = FALSE, add.colnames = NULL, add.rownames = NA) #erstellt Graphfile aus unterer H??lfte der Aehnlichkeitsmatrix

  cfg <- igraph::cluster_louvain(g1, weights = NULL) #sucht im Graphen nach Gruppen

  #k <- plot(cfg, g1, vertex.size=10, vertex.label.font=20, family="serif", edge.width=E(g1)$weight, sub= stringr::str_c("Modularity=",modularity(cfg))) #Zeichnet Graphen samt Gruppierungen

  group <- as.factor(igraph:::membership(cfg))
  data <- cbind(data,group)

  return(list(data,cfg, g1,
              plot(cfg, g1, vertex.size=10, vertex.label.font=20, family="serif", edge.width=E(g1)$weight, sub= stringr::str_c("Modularity=",modularity(cfg)))))
}
