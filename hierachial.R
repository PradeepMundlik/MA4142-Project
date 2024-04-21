library(dplyr)
weather.df <- read.csv("shuffle_file.csv")
head(weather.df)
weather <- select(weather.df,-Formatted.Date,-Loud.Cover,-Daily.Summary,-Summary,-Precip.Type)
weather.norm <- sapply(weather,scale)
head(weather.norm)
distance <- dist(weather.norm[,c(1,3,4,5,6,7)], method = "euclidean")
hc <- hclust(distance, method = "ward.D")
plot(hc, hang = -1, ann = FALSE)
cluster <- cutree(hc, k = 3)
one = 0
two = 0
three = 0
for (x in cluster){
  if (x==1){
    one = one+1
  }
  if (x==2){
    two = two+1
  }
  if (x==3){
    three = three+1
  }
}
one
two
three
heatmap(as.matrix(weather.norm), Colv = NA, hclustfun = hclust,col=rev(paste("gray",1:99,sep="")))
