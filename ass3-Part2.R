library(igraph)

setwd("C:\\Users\\USER\\Dropbox\\Raz&Livnat\\lastYear\\b\\ofrit\\ass3")
ga.data <- read.csv('GraphData.csv', header=TRUE)

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
plot(g)

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
V(g) [ membership == 8 ]$color <- "darkgoldenrod"
V(g) [ membership == 9 ]$color <- "brown3"
V(g) [ membership == 10 ]$color <- "burlywood3"
V(g) [ membership == 11 ]$color <- "darkmagenta"
V(g) [ membership == 12 ]$color <- "lightgoldenrod4"
V(g) [ membership == 13 ]$color <- "lightsalmon1"
V(g) [ membership == 14 ]$color <- "lightslateblue"
V(g) [ membership == 15 ]$color <- "navy"
V(g) [ membership == 16 ]$color <- "olivedrab3"
V(g) [ membership == 17 ]$color <- "orangered1"
V(g) [ membership == 18 ]$color <- "palegreen1"
V(g) [ membership == 19 ]$color <- "red4"
V(g) [ membership == 20 ]$color <- "seagreen"
V(g) [ membership == 21 ]$color <- "tan1"
V(g) [ membership == 22 ]$color <- "steelblue3"
V(g) [ membership == 23 ]$color <- "violetred2"
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
V(g2) [ membership == 8 ]$color <- "tan1"
#V(g)$size <- 6
#table(clusters(g)$csize)
plot(g2, layout=layout.fruchterman.reingold)
