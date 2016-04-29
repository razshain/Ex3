#עבודה 3 – יישום שיטות לניתוח נתונים
## מגישות רז שיין 201054103 , ליבנת גרשוני 301792792 ##
###האנטומיה של גריי ###
```{r} 
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
``` 
1. מדדי מרכזיות לשחקנים:
<ol>
<li>&#x202b;Betweeness- השחקן sloan הוא בעל ה- Betweeness הגבוה ביותר 115.3667</li>
<li>&#x202b;Closeness – לשחקנית torres יש את ה- Closeness הגבוה ביותר והוא 0.003194888</li>
<li>&#x202b;Eigencetor – לשחקן  karev יש את ה- Eigencetor הגבוה ביותר והוא 0.5027688</li>
</ol>

```{r} 
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

plot(g2, layout=layout.fruchterman.reingold)
``` 
2.	האלגוריתמים לזיהוי קהילות בהם בחרנו הם:
<ol>
&#x202b;הראשון מסוג Top-down ונקרא Girvan & Newman והשני short random walks model:
<li>Girvan & Newman 
![alt text](https://github.com/razshain/Ex3/blob/master/1.png "p1")
short random walks mode:
![alt text](https://github.com/razshain/Ex3/blob/master/2.png "p2")
</li>
<li>&#x202b;בשני האלגוריתמים השונים התקבלו אותו מספר של קהילות אשר גודלן שונה, באלגוריתם Girvan & Newman  גודלן נע החל מ- ועד לכ- 8 אנשים בקבוצה. באלגוריתם short random walks mode גודלן נע החל מ-2 אנשים ועד ל13.</li>
<li>&#x202b;ערכי ה- modularity
<ol>
<li>&#x202b;עבור Girvan & Newman הערך הוא- 0.5774221</li>
<li>&#x202b;עבור short random walks הערך הוא - 0.5147059</li>
</ol>
</li>
</ol>




###חלק ב- רשת לבחירתנו ###
בחרנו ברשת מהאינטרנט, הממפה חוגי ספורט לילד, כאשר כל צלע בין קודקודים של חוגי ספורט היא ילד הרשום לשניהם. ברשץ קיימים 91 קודקודים ו1360 קשתות, הגרף הוא גרף לא מכוון (Undirected) ןשמי (Named).

```{r} 
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
``` 
1. מדדי מרכזיות לחוגים:
<ol>
<li>&#x202b;Betweeness- החוג German Club הוא בעל ה- Betweeness הגבוה ביותר 323.9115</li>
<li>&#x202b;Closeness – לחוג  Spanish Club יש את ה- Closeness הגבוה ביותר והוא 0.009009009</li>
<li>&#x202b;Eigencetor – לחוג  Forensics (National Forensics League) יש את ה- Eigencetor הגבוה ביותר והוא 0.1828494</li>
</ol>

```{r} 
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

``` 
2.	האלגוריתמים לזיהוי קהילות בהם בחרנו הם:
<ol>
&#x202b;הראשון מסוג Top-down ונקרא Girvan & Newman והשני short random walks model:
<li>Girvan & Newman 
![alt text](https://github.com/razshain/Ex3/blob/master/3.png "p3")
short random walks mode:
![alt text](https://github.com/razshain/Ex3/blob/master/4.png "p4")
</li>
<li>&#x202b;באלגוריתם Girvan & Newman  התקבלו 23 קהילות שונות כאשר הקהילה הגדולה ביותר מונה כ-69 משתתפים והקטנה ביותר מונה 1
באלגוריתם short random walks mode התקבלו 8 קהילות שונות כאשר הקהילה הגדולה ביותר מונה כ- 27 משתתפים והקטנה ביותר מונה 1 .</li>
<li>ערכי ה- modularity
<ol>
<li>&#x202b;עבור Girvan & Newman הערך הוא- 0.02009326</li>
<li>&#x202b;עבור short random walks הערך הוא - 0.1045196</li>
</ol>
</li>
</ol>
