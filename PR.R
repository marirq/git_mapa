###################### PR ######################
pr <- read.csv2('PR.csv',header=T) 
str(pr)
pr.coord <- pr[which(pr$lat!=''),]
pr.coord <- pr.coord[complete.cases(pr.coord$lat),]
str(pr.coord) # 22216 prop com coords

require(stringr)
pr.coord$lat  <- str_trim(sub(',','.',pr.coord$lat));pr.coord$long  <- str_trim(sub(',','.',pr.coord$long))
str(pr.coord)

simb.lat <- pr.coord$lat;simb.long <- pr.coord$long
head(simb.lat);head(simb.long)

simb.lat2 <- simb.lat[1]
for (i in 2:length(simb.lat)) {
  simb.lat2 <- paste0(simb.lat2, simb.lat[i])
}
simb.long2 <- simb.long[1]
for (i in 2:length(simb.long)) {
  simb.long2 <- paste0(simb.long2, simb.long[i])
}

simb.lat3 <- NA
for (i in 1:nchar(simb.lat2)) {
  simb.lat3 <- c(simb.lat3, substr(simb.lat2, i, i))
}
simb.lat3 <- simb.lat3[-1]
simb.long3 <- NA 
for (i in 1:nchar(simb.long2)) { 
  simb.long3 <- c(simb.long3, substr(simb.long2, i, i))
}
simb.long3 <- simb.long3[-1]

simb.lat4 <- grep('[^[:alnum:]]', simb.lat3, value = T)
simb.long4 <- grep('[^[:alnum:]]', simb.long3, value = T)

u_lat <- unique(simb.lat4)
u_long <- unique(simb.long4)

pr.coord$LAT <- as.numeric(pr.coord$lat);pr.coord$LONG <- as.numeric(pr.coord$long)
str(pr.coord)

Nolat <- nchar(pr.coord$LAT)
unique(Nolat)
Nchar5 <- rownames(pr.coord[which(nchar(pr.coord$LAT)==5),])
coords5 <- pr.coord[Nchar5,'LAT']
pr.coord[Nchar5,'LAT'] <- coords5/1000

Nchar6 <- rownames(pr.coord[which(nchar(pr.coord$LAT)==6),])
coords6 <- pr.coord[Nchar6,'LAT']
q <- coords6[c(47,96)]
coords6 <- coords6/10000
coords6[c(47,96)] <- q
pr.coord[Nchar6,'LAT'] <- coords6

Nchar7 <- rownames(pr.coord[which(nchar(pr.coord$LAT)==7),])
coords7 <- pr.coord[Nchar7,'LAT']
pr.coord[Nchar7,'LAT'] <- coords7/100000


Nolong <- nchar(pr.coord$LONG)
unique(Nolong)
Nchar5.5 <- rownames(pr.coord[which(nchar(pr.coord$LONG)==5),])
coords5.5 <- pr.coord[Nchar5.5,'LONG']
coords5.5[3] <- coords5.5[3]/100
pr.coord[Nchar5.5,'LONG'] <- coords5.5/1000

Nchar6.6 <- rownames(pr.coord[which(nchar(pr.coord$LONG)==6),])
coords6.6 <- pr.coord[Nchar6.6,'LONG']
q <- coords6.6[c(31,143)]
coords6.6 <- coords6.6/10000
coords6.6[c(31,143)] <- q
pr.coord[Nchar6.6,'LONG'] <- coords6.6

Nchar7.7 <- rownames(pr.coord[which(nchar(pr.coord$LONG)==7),])
coords7.7 <- pr.coord[Nchar7.7,'LONG']
pr.coord[Nchar7.7,'LONG'] <- coords7.7/100000

Nchar7.coords7err <- row.names(pr.coord[which(pr.coord$LAT<1),])
coords7err <- as.numeric(pr.coord[Nchar7.coords7err,'lat'])
Nchar.lat.7err <- nchar(coords7err)
unique(Nchar.lat.7err)
pr.coord[Nchar7.coords7err,'LAT'] <- coords7err/10000

Nchar.coords.err.long <- row.names(pr.coord[which(pr.coord$LONG<1),])
coords.err.long <- as.numeric(pr.coord[Nchar.coords.err.long,'long'])
Nlong.err <- nchar(coords.err.long)
unique(Nlong.err)
pr.coord[Nchar.coords.err.long,'LONG'] <- coords.err.long/10000

str(pr.coord)
pr.coord$lat <- pr.coord$LAT*(-1);pr.coord$long <- pr.coord$LONG*(-1)
str(pr.coord)

library(ggmap)
qmap('State of Paraná',zoom=6)+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = pr.coord)

write.csv2(pr,file='PR_coords.csv')


################################ Sistemas de Coords ################################################
# O sistema de coordenada (DATUM)  em Grau, do Google Earth e da maioria dos mapas na internet é a WGS84

################################ Geotrans ################################################
# executavel no windowns pra transformar coordenadas