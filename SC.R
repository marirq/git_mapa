###################### SC ######################
sc0 <- read.csv('SC.csv',header=T,sep=';') 
sc <- sc0[which(sc0$LATITUDE_S!=''),]
names(sc)[c(9,10)] <- c('lat','long')
str(sc) # 9271 prop com coords
rows0 <- grep('^0.',sc$lat)
sc[rows0,c('lat','long')]
sc <- sc[-rows0,]
str(sc)

require(stringr)
sc$lat  <- str_trim(sc$lat);sc$long  <- str_trim(sc$long)
sc$lat <- sub(',','.',sc$lat);sc$long <- sub(',','.',sc$long)
str(sc)

q1 <- sc$lat;q11 <- sc$long
head(q1);head(q11)

q2 <- q1[1]
for (i in 2:length(q1)) {
  q2 <- paste0(q2, q1[i])
}
q22 <- q11[1]
for (i in 2:length(q11)) {
  q22 <- paste0(q22, q11[i])
}

q3 <- NA
for (i in 1:nchar(q2)) {
  q3 <- c(q3, substr(q2, i, i))
}
q3 <- q3[-1]
q33 <- NA
for (i in 1:nchar(q22)) {
  q33 <- c(q33, substr(q22, i, i))
}
q33 <- q33[-1]

q4 <- grep('[^[:alnum:]]', q3, value = T)
q44 <- grep('[^[:alnum:]]', q33, value = T)

u1 <- unique(q4)
u11 <- unique(q44)

str(sc)
Nchar.lat <- nchar(sc$lat)
unique(Nchar.lat)
sc[which(nchar(sc$lat)==8),c('lat','long')]
Nchar9 <- rownames(sc[which(nchar(sc$lat)==9),])
sc[which(nchar(sc$lat)==10),c('lat','long')]

coords9 <- as.numeric(sc[Nchar9,'lat'])
sc[Nchar9,'LAT'] <- coords9/1000000
str(sc)

Nchar.long <- nchar(sc$long)
unique(Nchar.long)
sc[which(nchar(sc$long)==8),c('lat','long')]
Nchar9.9 <- rownames(sc[which(nchar(sc$long)==9),])
sc[which(nchar(sc$long)==10),c('lat','long')]

coords9.9 <- as.numeric(sc[Nchar9.9,'long'])
sc[Nchar9.9,'LONG'] <- coords9.9/1000000

sc$LAT
Nrow8.pLAT <- row.names(sc[which(nchar(sc$lat)==8),])
sc[Nrow8.pLAT,'LAT'] <- as.numeric(sc[Nrow8.pLAT,'lat'])
Nrow10.pLAT <- row.names(sc[which(nchar(sc$lat)==10),])
sc[Nrow10.pLAT,'LAT'] <- as.numeric(sc[Nrow10.pLAT,'lat'])

sc$LONG
Nrow8.pLONG <- row.names(sc[which(nchar(sc$long)==8),])
sc[Nrow8.pLONG,'LONG'] <- as.numeric(sc[Nrow8.pLONG,'long'])
Nrow10.pLONG <- row.names(sc[which(nchar(sc$long)==10),])
sc[Nrow10.pLONG,'LONG'] <- as.numeric(sc[Nrow10.pLONG,'long'])

str(sc)
#sc$lat <- as.numeric(sc$LAT);sc$long <- as.numeric(sc$LONG)
#str(sc)

library(ggmap)
map_sc <- 'State of Santa Catarina'
sc_map <- qmap(map_sc,zoom=6)
w <- sc_map+geom_point(aes(x = LONG, y = LAT , color = 'red'), 
                       data = sc)
ww <- ggplot_build(w) # 23 missing rows
miss.rows <- as.numeric(row.names(ww$data[[4]][!complete.cases(ww$data[[4]][,c(2,3)]),c(2,3)]))
sc[miss.rows,c('lat','long','LAT','LONG')]
sc[miss.rows,c('LAT','LONG')]
mrLat <- miss.rows[c(1,3,4,6,7,9,10,11,12,13,15,16,17,18,19,20,21,22)] # subst lat p/ LAT
sc[mrLat,'LAT'] <- as.numeric(sc[mrLat,'lat'])
mrLong <- miss.rows[c(1,5,8,10,12,13,14,15,16,17,19,20,21,22,23)] # subst long p/LONG
sc[mrLong,'LONG'] <- as.numeric(sc[mrLong,'long'])

mrLatLong <- miss.rows[c(2)] # dividir antes e depoi passar p/LAT e LONG
sc[mrLatLong,c('LAT','LONG')] # as.numeric(sc$lat[mrLatLong])/1000000

str(sc)
sc$lat <- sc$LAT;sc$long <- sc$LONG
map_sc <- 'State of Santa Catarina'
sc_map <- qmap(map_sc,zoom=6)
sc_map+geom_point(aes(x = LONG, y = lat , color = 'red'), 
                       data = sc)

write.csv2(sc,file='SC_mrq_ArqGIS.csv',sep=';')
sc[865,c('lat','long')]