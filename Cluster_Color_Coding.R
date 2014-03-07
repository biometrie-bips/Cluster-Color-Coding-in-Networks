# fuer Martin

# WD setzen!!!
setwd("./Arbeit/BIPS/fuer Martin/")

# Expansions/Inflations-Matrix laden
load("./inflMatrix.RData")


library("GeneNet")
# Example E. Coli data set (102 genes)
data(ecoli)

### Step 1: Estimate partial correlation matrix
pc <- ggm.estimate.pcor(ecoli)

### Step 2: Assign p-values, q-values, empirical posterior probabilities to all potential edges in the network
ecoli.edges <- network.test.edges(pc, direct=F, fdr=TRUE)

### Step 3: Decide which edges to include in the network
ecoli.net <- extract.network(ecoli.edges)

### Step 4: Plot the graph with iGraph
node.labels <- colnames(ecoli)
igr1 <- network.make.igraph(ecoli.net, node.labels)
plot(igr1, main="Ecoli Network", edge.color="green")


plot(igr1, main="Ecoli Network", vertex.color=c("red", "blue", "green", "yellow"), edge.color="black",
     )




##########################################
# sofern deine Matrix auch infl.norm heißt, kannst du ab hier den Code laufen lassen

# rausschreiben, welche knoten sich clustern
Cluster <- vector(mode="list")
for(i in seq_along(infl.norm[,1])){
  name1 <- row.names(infl.norm)[i]
  temp.name <- NULL
  for(j in seq_along(infl.norm[,1])){
    a <- NULL
    if(infl.norm[i,j] != 0){
      a <- colnames(infl.norm)[j]
    }
    temp.name <- c(temp.name, a)
  }
  cluster.temp <- c(name1, temp.name)
  if(length(cluster.temp)==1){
    cluster.temp <- NULL
  }
  Cluster[[i]] <- cluster.temp
}
rm(i,j)


# aus dieser Liste alle Elemente entfernen, die leer sind,
# und neue Liste erstellen, die nur noch die gefundenen Cluster enthält
list.delete <- NULL
for(i in seq_along(Cluster)){
  if(length(Cluster[[i]]) == 0){
    list.delete <- c(list.delete, i)
  }
}
Cluster2 <- Cluster[-list.delete]
rm(i)

# noch alle Doppelungen entfernen

for(i in seq_along(Cluster2)){
  Cluster2[[i]] <- Cluster2[[i]][!duplicated(Cluster2[[i]])]
}
rm(i)


# kommen Knoten in mehreren Clustern vor?
duplicated(unlist(Cluster2))
table(duplicated(unlist(Cluster2)))
# ja, 3!!!



# Achtung, ab hier wirds "schmutzig": mir ist es jetzt egal, zu welchem
# Cluster ein Knoten farblich zugeordnet wird, wenn er in mindestens zwei 
# Clustern vorkommt. Das ist nur für DIESEN FALL akzeptabel, weil es um das
# Prinzip der farblichen Darstellung geht.


# Die Kanten alle identisch in Dicke und Art zeichnen:
E(igr1)$lty <- 1 # durchgehende Linie
E(igr1)$width <- 1.5 # gleiche Dicke


summary(igr1)


