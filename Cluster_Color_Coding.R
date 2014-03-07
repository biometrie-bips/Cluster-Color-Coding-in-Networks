# R-Code zur farbigen darstellung von Clustern in Netzwerken
# (fuer Martin)

# von Moritz Hanke
# Erstellungsdatum: 2014-03-07


# WD setzen!!!
setwd("./Arbeit/BIPS/fuer Martin/")

# Expansions/Inflations-Matrix laden
load("./inflMatrix.RData")


library("GeneNet")

# Code aus dem Paket GeneNet; wird benötigt um den Beispiel-Datensatz
# und damit den Beispielgraphen zu erstellen
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





##########################################
# sofern deine Matrix auch infl.norm heißt, kannst du ab hier den Code laufen lassen
# theoretisch kann also jedes igraph-Objekt reingepackt werden und anshcließend eine
# farbige Plottung erfolgen. Aber Vorsicht vor Knoten-Doppelunegn in und zwischen
# Clustern!!!!

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



# Die Kanten alle identisch in Dicke und Art zeichnen:
E(igr1)$lty <- 1 # durchgehende Linie
E(igr1)$width <- 1.5 # gleiche Dicke


# kommen Knoten in mehreren Clustern vor?
duplicated(unlist(Cluster2))
table(duplicated(unlist(Cluster2)))
# ja, 3!!!


# !!!!!!!!!!!!!!!!!!!!!!!!
# DON'T TRY THIS AT HOME!!!!!!
# Achtung, ab hier wirds "schmutzig": mir ist es jetzt egal, zu welchem
# Cluster ein Knoten farblich zugeordnet wird, wenn er in mindestens zwei 
# Clustern vorkommt. Das ist nur für DIESEN FALL akzeptabel, weil es um das
# Prinzip der farblichen Darstellung geht.



# ein kleiner Farben-Zufallsgenerator:
ran.color <- function(n.color){
  hex <- c(seq(0,9), "A", "B", "C", "D", "E", "F")
  vec.color <- NULL
  for(i in 1:n.color){
    hex.temp <- (paste(c("#",sample(hex, 6, replace=T)), collapse=""))
    vec.color <- c(vec.color, hex.temp)
  }
  return(vec.color)
}


farbanzahl <- length(Cluster2)


# ab hier kann durch immer wieder erneutes ausführen des Codes solange 
# rumgespielt werden, bis einem die Farben gefallen
# Alternativ kann natürlich auch ein Vector erstellt werden, der die 
# gewünschten Farben als Name oder Hex beinhaltet: 
# farben <- c(<welchen farben auch immer>)

farben <- ran.color(farbanzahl)

for(i in seq_along(Cluster2)){
  V(igr1)[Cluster2[[i]]]$color <- farben[i]
}

plot(igr1, main="Ecoli Network", edge.color="black")


