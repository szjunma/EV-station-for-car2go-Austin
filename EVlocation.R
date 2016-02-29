library(ggmap)
library(grid)
library(dplyr)
library(ggplot2)
library(broom)
library(amap)

time.df <- read.csv('data/1Timedcar2go_week.csv', header = T)
#car location plot
p1<- ggmap(get_map(location = 'austin', zoom = 12), extent = 'device')+ 
  geom_point(aes(x = Longitude, y = Latitude), data = time.df, alpha = 0.1, color = '#D55E00', size = 4) +
  theme(legend.position = 'none')
p1.2<- ggmap(get_map('domain, austin', zoom = 15), extent = 'device')+
  geom_point(aes(x = Longitude, y = Latitude), data = time.df, alpha = 0.1, color = '#D55E00', size = 4) +
  ggtitle('The Domain')+
  theme(legend.position = 'none', 
        plot.title = element_text(size = rel(2)),
        panel.border = element_rect(colour = "black", fill = NA, size=2))

plot_inset <- function(name, p1, p2){
  png(name, width=1280, height=1280)
  grid.newpage()
  v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
  v2<-viewport(width = 0.2, height = 0.2, x = 0.18, y = 0.83) #plot area for the inset map
  print(p1,vp=v1) 
  print(p2,vp=v2)
  dev.off()  
}

plot_inset('1.png', p1, p1.2)

# png('1.png', width=1280, height=1280)
# grid.newpage()
# v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
# v2<-viewport(width = 0.2, height = 0.2, x = 0.18, y = 0.83) #plot area for the inset map
# print(p1,vp=v1) 
# print(p1.2,vp=v2)
# dev.off()  

#wss vs no. of centroids
set.seed(18)
wss <- data.frame(clusterNo = seq(1,50), wss = rep(0, 50))

for (i in 1:50){
  
  clust.k <-time.df %>% select(Longitude, Latitude) %>% kmeans(i, iter.max=500)
  wss$wss[i] <- sum(clust.k$tot.withinss)
}
p2 <- ggplot(wss)+geom_point(aes(clusterNo, wss), size = 4, shape = 1, color='#009E73')+
  xlab('No. of Centroids') + ylab('WSS') +
  theme_bw(18)

png('2.png', width=640, height=480)
print(p2) 
dev.off() 

#50 charging station
clust <- time.df %>% select(Longitude, Latitude) %>% kmeans(50, iter.max=500)
p3<- ggmap(get_map(location = 'austin', zoom = 12), extent = 'device')+
  geom_point(data=augment(clust, time.df),
             aes(x = Longitude, y = Latitude, color = .cluster), alpha =0.1, size = 4) +
  geom_point(aes(Longitude, Latitude), data = data.frame(clust$centers), size = 15, shape = 'x') + 
  theme(legend.position = 'none')

p3.2<- ggmap(get_map('domain, austin', zoom = 15), extent = 'device')+
  geom_point(data=augment(clust, time.df),
             aes(x = Longitude, y = Latitude, color = .cluster), alpha =0.1, size = 4) +
  geom_point(aes(Longitude, Latitude), data = data.frame(clust$centers), size = 15, shape = 'x') + 
  ggtitle('The Domain')+
  theme(legend.position = 'none', 
        plot.title = element_text(size = rel(2)),
        panel.border = element_rect(colour = "black", fill = NA, size=2))

plot_inset('3.png', p3, p3.2)

station.df <- read.csv('data/charging_stations (Feb 20 2016).csv', header = T)
station.austin = station.df%>%dplyr::filter(City=='Austin')

p4<- ggmap(get_map(location = 'austin', zoom = 12), extent = 'device')+
  geom_point(aes(Longitude, Latitude), data = data.frame(clust$centers), size = 15, shape = 'x') + 
  geom_point(aes(x = Longitude, y = Latitude), data = station.austin, 
             size = 14, shape = 'E', color = '#009E73') +
  theme(legend.position = 'none')

p4.2<- ggmap(get_map('domain, austin', zoom = 15), extent = 'device')+
  geom_point(aes(Longitude, Latitude), data = data.frame(clust$centers), size = 15, shape = 'x') + 
  geom_point(aes(x = Longitude, y = Latitude), data = station.austin, 
             size = 14, shape = 'E', color = '#009E73') +
  ggtitle('The Domain')+
  theme(legend.position = 'none', 
        plot.title = element_text(size = rel(2)),
        panel.border = element_rect(colour = "black", fill = NA, size=2))

plot_inset('4.png', p4, p4.2)
# png('4.png', width=1280, height=1280)
# grid.newpage()
# v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
# v2<-viewport(width = 0.2, height = 0.2, x = 0.18, y = 0.83) #plot area for the inset map
# print(p4,vp=v1) 
# print(p4.2,vp=v2)
# dev.off()   
  
station.dist <- mutate(data.frame(clust$centers), distToExist= 0)
for (i in 1:nrow(station.dist)){
  # In the area of Austin, one dgree in Latitude is 69.1 miles, 
  # while one degree in Longitude is 59.7 miles
  d <- sqrt(((station.austin$Latitude-station.dist$Latitude[i])*69.1)**2 
            +((station.austin$Longitude-station.dist$Longitude[i])*59.7)**2)
  station.dist$distToExist[i] <- min(d)
  
}

p5 <-ggmap(get_map(location = 'austin', zoom = 12), extent = 'device')+
  geom_point(aes(Longitude, Latitude, color = -sign(distToExist-0.5)), 
             data = station.dist, size = 15, shape = 'x') + 
  geom_point(aes(x = Longitude, y = Latitude), data = station.austin, 
             size = 14, shape = 'E', color = '#009E73') +
  theme(legend.position = 'none')

p5.2<- ggmap(get_map('domain, austin', zoom = 15), extent = 'device')+
  geom_point(aes(Longitude, Latitude, color = -sign(distToExist-0.5)), 
             data = station.dist, size = 15, shape = 'x') + 
  geom_point(aes(x = Longitude, y = Latitude), data = station.austin, 
             size = 14, shape = 'E', color = '#009E73') +
  ggtitle('The Domain')+
  theme(legend.position = 'none', 
        plot.title = element_text(size = rel(2)),
        panel.border = element_rect(colour = "black", fill = NA, size=2))

plot_inset('5.png', p5, p5.2)

a <- station.dist %>%
  filter(distToExist>0.5) %>%
  summarise(count=n())

# manhattan distance
clust <-time.df %>% select(Longitude, Latitude) %>% kmeans(50, iter.max=500, method="manhattan")
p3<- ggmap(get_map(location = 'austin', zoom = 12), extent = 'device')+
  geom_point(data=augment(clust, time.df),
             aes(x = Longitude, y = Latitude, color = .cluster), alpha =0.1, size = 4) +
  geom_point(aes(Longitude, Latitude), data = data.frame(clust$centers), size = 15, shape = 'x') + 
  theme(legend.position = 'none')

p3.2<- ggmap(get_map('domain, austin', zoom = 15), extent = 'device')+
  geom_point(data=augment(clust, time.df),
             aes(x = Longitude, y = Latitude, color = .cluster), alpha =0.1, size = 4) +
  geom_point(aes(Longitude, Latitude), data = data.frame(clust$centers), size = 15, shape = 'x') + 
  ggtitle('The Domain')+
  theme(legend.position = 'none', 
        plot.title = element_text(size = rel(2)),
        panel.border = element_rect(colour = "black", fill = NA, size=2))

plot_inset('6.png', p3, p3.2)