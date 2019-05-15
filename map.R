#Maps
library(ggmap)
sfMap <- qmap("San Francisco", zoom = 12, color = "bw")
map=get_map(location=c(lon=median(sp$X), lat=median(sp$Y)), zoom = 12, source="stamen")
sfMap +
  geom_point(aes(x=sp$X, y=sp$Y, size=new.category),   
             color="pink", data=sp, alpha=.5) + 
  geom_point(aes(x=sp$X, y=sp$Y, shape = new.category), alpha=.5)
#-------------------------------#
#Zoom in top right corner
map=get_map(location=c(lon=-122.45, lat=37.75), zoom = 13, source="stamen")
ggmap(map) +
  geom_point(aes(x=sp$X, y=sp$Y, size=new.category),   
             color="darkred", data=sp, alpha=.5) + 
  geom_point(aes(x=sp$X, y=sp$Y, shape = new.category), alpha=.5)
#-------------------------------#
#Zoom in a little more
map=get_map(location=c(lon=-122.410, lat=37.783), zoom = 15, source="stamen")
ggmap(map) +
  geom_point(aes(x=sp$X, y=sp$Y, color=new.category), size=3,  
              data=sp, alpha=.5) + 
  geom_point(aes(x=sp$X, y=sp$Y, shape=factor(sp$PdDistrict)), alpha=.5)
#-------------------#
library(ggmap)
library(ggplot2)
library(dplyr)
sp1=newtrain[sample(nrow(newtrain),10000,replace=FALSE),]
sfMap <- qmap("San Francisco", zoom = 12, color = "bw")
map_crime <- function(crime_df, crime) {
  filtered <- filter(crime_df, newCategory %in% crime)
  plot <- sfMap + 
    geom_point(data=filtered, aes(x=X, y=Y, color=newCategory), alpha=0.5, size=3)
  return(plot)
}
map_crime(sp1, c("THEFT","OTHER"))
map_crime(sp1, c("VIOLENCE","SOCIAL SECURE"))
map_crime(sp1, c("FINANCIAL","PERSON","SEX CRIME"))
map_crime(sp1, c("SEX CRIME"))

h2015=newtrain[which(newtrain$year=="2015"),]
h2013=newtrain[which(newtrain$year=="2013"),]
map_crime(h2013,c("VIOLENCE","SEX CRIME"))
map_crime(h2015,c("VIOLENCE","SEX CRIME"))


