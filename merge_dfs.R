names(ac)
names(am) 
names(ba)# arrumar COD_CADASTRO
names(ba)[1] <- 'COD_CADASTRO'
names(ce)
names(df)
names(es)
names(go)[c(1,2)] <- c('COD_CADASTRO','UF_ESTABEL') # arrumar COD_CADASTRO e UF_ESTABEL
names(ma)
names(mg)
names(ms)
names(mt)
names(pa)

a <- merge(ac[,c(2,1,11,12)],am[,c(2,1,11,12)],all=TRUE)
a <- merge(a,ba[,c(2,1,8,9)],all=TRUE)
a <- merge(a,ce[,c(2,1,13,16)],all=TRUE)
a <- merge(a,df[,c(2,1,11,12)],all=TRUE)
a <- merge(a,es[,c(2,1,8,9)],all=TRUE)
a <- merge(a,go[,c(2,1,9,10)],all=TRUE)
a <- merge(a,ma[,c(2,1,11,12)],all=TRUE)
a <- merge(a,mg[,c(2,1,8,9)],all=TRUE)
a <- merge(a,ms[,c(2,1,11,12)],all=TRUE)
a <- merge(a,mt[,c(2,1,8,10)],all=TRUE)
a <- merge(a,pa[,c(2,1,12,13)],all=TRUE)

head(a)
tail(a)
str(a) 

###### plotar mapa e pontos ######
library(ggmap)
map_br <- "Brasil"
br <- qmap(map_br,zoom=4)
br+geom_point(aes(x = long, y = lat, color = 'red'),
              data = a)
