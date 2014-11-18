###################### PR ######################
pr0 <- read.csv('PR.csv',header=T,sep=';') 
str(pr0) # 22268 rows
pr <- pr0[which(pr0$Latitude!=''),]
str(pr) # 22216 prop com coords

names(pr)[c(14,15)] <- c('lat','long')
str(pr)

require(stringr)
pr$lat  <- str_trim(pr$lat);pr$long  <- str_trim(pr$long)
str(pr)

pr$lat <- sub(',','.',pr$lat);pr$long <- sub(',','.',pr$long)
str(pr)

q1 <- pr$lat;q11 <- pr$long
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

pr$LAT <- gsub(u1,'',pr$Latitude);pr$LONG <- gsub(u11,'',pr$Longitude)
pr$LAT <- as.numeric(pr$LAT);pr$LONG <- as.numeric(pr$LONG)
str(pr)

Nolat <- nchar(pr$LAT)
unique(Nolat)
Nchar5 <- rownames(pr[which(nchar(pr$LAT)==5),])
coords5 <- pr[Nchar5,'LAT']
pr[Nchar5,'LAT'] <- coords5/1000

Nchar6 <- rownames(pr[which(nchar(pr$LAT)==6),])
coords6 <- pr[Nchar6,'LAT']
q <- coords6[c(3,9)]
coords6 <- coords6/10000
coords6[c(3,9)] <- q
pr[Nchar6,'LAT'] <- coords6

Nchar7 <- rownames(pr[which(nchar(pr$LAT)==7),])
coords7 <- pr[Nchar7,'LAT']
pr[Nchar7,'LAT'] <- coords7/100000


Nolong <- nchar(pr$LONG)
unique(Nolong)
Nchar5.5 <- rownames(pr[which(nchar(pr$LONG)==5),])
coords5.5 <- pr[Nchar5.5,'LONG']
coords5.5[3] <- coords5.5[3]/100
pr[Nchar5.5,'LONG'] <- coords5.5/1000

Nchar6.6 <- rownames(pr[which(nchar(pr$LONG)==6),])
coords6.6 <- pr[Nchar6.6,'LONG']
q <- coords6.6[c(31,143)]
coords6.6 <- coords6.6/10000
coords6.6[c(31,143)] <- q
pr[Nchar6.6,'LONG'] <- coords6.6

Nchar7.7 <- rownames(pr[which(nchar(pr$LONG)==7),])
coords7.7 <- pr[Nchar7.7,'LONG']
pr[Nchar7,'LONG'] <- coords7.7/100000

Nchar7.coords7err <- row.names(pr[which(pr$LAT<1),])
coords7err <- as.numeric(pr[Nchar7.coords7err,'Latitude'])
#q <- as.numeric(gsub(u1,'',coords.7err))
#q <- Nchar7.coords7.7err/10000
pr[Nchar7.coords7err,'LAT'] <- coords7err

Nchar.coords.err0 <- row.names(pr[which(pr$LONG<1),])
fix <- pr[Nchar.coords.err0,'Longitude']
fix <- as.numeric(gsub(u11,'',fix))
nchar.fix <- nchar(fix)
unique(nchar.fix)
coords.err0 <- fix/10000
pr[Nchar.coords.err0,'LONG'] <- coords.err0

str(pr)
pr$lat <- pr$LAT*(-1);pr$long <- pr$LONG*(-1)

library(ggmap)
map_pr <- 'State of Paraná'
pr_map <- qmap(map_pr,zoom=6)
w <- pr_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = pr)
ww <- ggplot_build(w) # 1 missing row rj$lat[41]
ww$data[[4]][!complete.cases(ww$data[[4]][,c(2,3)]),c(2,3)]

write.csv2(pr,file='PR_mrq.csv',sep=';')


################################ Sistemas de Coords ################################################
# O sistema de coordenada (DATUM)  em Grau, do Google Earth e da maioria dos mapas na internet é a WGS84

################################ Geotrans ################################################
# executavel no windowns pra transformar coordenadas