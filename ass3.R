library(igraph)

setwd("C:\\Users\\pc\\Dropbox\\Raz&Livnat\\lastYear\\b\\ofrit\\ass3")
ga.data <- read.csv('ga_edgelist.csv', header=TRUE)
g <- graph.data.frame(ga.data, directed=FALSE)
summary(g)
g$layout <- layout.fruchterman.reingold(g)
# Remove self-loops is exist
g <- simplify(g)
# Set seed (for layout)
set.seed(100)
plot(g)
degree(g)
closeness(g)
max(closeness(g))
betweenness(g)
max(betweenness(g))
vec<-graph.eigen(g)$vectors
max(vec)
#__________________community detection___________________
g2<-g
gn<-edge.betweenness.community(g)
V(g)$membership <- gn$membership
V(g)$membership
max(V(g)$membership)
gn$modularity
modularity(gn)

# color communities
V(g) [ membership == 1 ]$color <- "green"
V(g) [ membership == 2 ]$color <- "blue"
V(g) [ membership == 3 ]$color <- "red"
V(g) [ membership == 4 ]$color <- "yellow"
V(g) [ membership == 5 ]$color <- "gray"
V(g) [ membership == 6 ]$color <- "black"
V(g) [ membership == 7 ]$color <- "purple"
#V(g)$size <- 6
#table(clusters(g)$csize)
plot(g, layout=layout.fruchterman.reingold)

w<-walktrap.community(g2)
V(g2)$membership <- w$membership
V(g2)$membership
max(V(g2)$membership)
w$modularity
modularity(w)

# color communities
V(g2) [ membership == 1 ]$color <- "green"
V(g2) [ membership == 2 ]$color <- "blue"
V(g2) [ membership == 3 ]$color <- "red"
V(g2) [ membership == 4 ]$color <- "yellow"
V(g2) [ membership == 5 ]$color <- "gray"
V(g2) [ membership == 6 ]$color <- "black"
V(g2) [ membership == 7 ]$color <- "purple"
#V(g)$size <- 6
#table(clusters(g)$csize)
plot(g2, layout=layout.fruchterman.reingold)
