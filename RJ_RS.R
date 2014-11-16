###################### RJ ######################
# quase ok, falta tirar um pto fora no ArqGIS 
rj0 <- read.csv('RJ.csv',header=T,sep=';') 
str(rj0)
rj <- rj0[which(rj0$LATITUDE_S..DECIMAL.!=''),] # eliminando rows sem coords
str(rj)

names(rj)[c(8,9)] <- c('lat','long')
str(rj)

require(stringr)
rj$lat  <- str_trim(rj$lat);rj$long  <- str_trim(rj$long) # retirando espacos em branco antes e depois
head(rj)

rj$lat <- sub(',','.',rj$lat);rj$long <- sub(',','.',rj$long)
str(rj)

####### Ver que simbolos tem 
q1 <- rj$lat;q11 <- rj$long
head(q1);head(q11)

q2 <- q1[1] # o primeiro linha da coluna  q escolhi
for (i in 2:length(q1)) { # do segundo da coluna (pq o primeiro ja pequei) ate o comprimento da coluna
  q2 <- paste0(q2, q1[i]) # juntando todas as linhas
}
q22 <- q11[1] # o primeiro linha da coluna  q escolhi
for (i in 2:length(q11)) { # do segundo da coluna (pq o primeiro ja pequei) ate o comprimento da coluna
  q22 <- paste0(q22, q11[i]) # juntando todas as linhas
}

q3 <- NA # crio um vetor vazio
for (i in 1:nchar(q2)) { # faco um for de 1 ate o numero de caracteres do meu vetor em conjunto
  q3 <- c(q3, substr(q2, i, i)) #  junto o q3 (que é NA no início)mais o q2 sendo que eu substituo o carcter 1 no primeiro lugar, depois o 2 no segundo e sucessivamente
}
q3 <- q3[-1] # tiro o primeiro elemento que era NA
q33 <- NA # crio um vetor vazio
for (i in 1:nchar(q22)) { # faco um for de 1 ate o numero de caracteres do meu vetor em conjunto
  q33 <- c(q33, substr(q22, i, i)) #  junto o q3 (que é NA no início)mais o q2 sendo que eu substituo o carcter 1 no primeiro lugar, depois o 2 no segundo e sucessivamente
}
q33 <- q33[-1] # tiro o primeiro elemento que era NA

q4 <- grep('[^[:alnum:]]', q3, value = T) # pega os simbolos
q44 <- grep('[^[:alnum:]]', q33, value = T) # pega os simbolos

u1 <- unique(q4) # tiro os repetidos, pra ver quais simbolos tenho de um jeito + limpo
u11 <- unique(q44) # tiro os repetidos, pra ver quais simbolos tenho de um jeito + limpo

u2 <- u1[-2] # tirando o simbolo de ponto
u22 <- u11[-2] # tirando o simbolo de ponto

# tenho que tirar um de cada vez
e <- gsub(u2[1],'T',rj$lat);ee <- gsub(u22[1],'T',rj$long)
e;ee

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+');ee <- strsplit(ee,'T+')

# antes pra ver se todas deram certo nomeia por 'LAT' ou 'LONG'
rj$LAT <- convert(e);rj$LONG <- convert(ee) 
is.numeric(rj$LAT);is.numeric(rj$LONG)
rj$LAT;rj$LONG

# mudar o sinal e colocar na coluna final
rj$lat <- rj$LAT*(-1);rj$long <- rj$LONG*(-1)
rj$lat;rj$long

###### plotar mapa e pontos pra ver se algum caiu fora
#install.packages('ggmap')
library(ggmap)
map_rj <- 'State of Rio de Janeiro'
rj_map <- qmap(map_rj,zoom=7)
w <- rj_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = rj)
ggplot_build(w) # 1 missing row rj$lat[41]

rj[41,c('lat','long')] # arrumar -229.1891 -42.99214
rj$lat[41] <- rj$lat[41]/10
# tirar um pto fora no ArqGIS


###################### RN ######################
# quase ok, falta tirar um pto fora no ArqGIS
rn0 <- read.csv('RN.csv',header=T,sep=';') 
str(rn0)
rn <- rn0[which(rn0$LATITUDE_S..DECIMAL.!=''),] # eliminando rows sem coords
str(rn)

names(rn)[c(8,9)] <- c('lat','long')
str(rn)

require(stringr)
rn$lat  <- str_trim(rn$lat);rn$long  <- str_trim(rn$long) # retirando espacos em branco antes e depois
head(rn)

rn$lat <- sub(',','.',rn$lat);rn$long <- sub(',','.',rn$long)
str(rn)

####### Ver que simbolos tem 
q1 <- rn$lat;q11 <- rn$long
head(q1);head(q11)

q2 <- q1[1] # o primeiro linha da coluna  q escolhi
for (i in 2:length(q1)) { # do segundo da coluna (pq o primeiro ja pequei) ate o comprimento da coluna
  q2 <- paste0(q2, q1[i]) # juntando todas as linhas
}
q22 <- q11[1] # o primeiro linha da coluna  q escolhi
for (i in 2:length(q11)) { # do segundo da coluna (pq o primeiro ja pequei) ate o comprimento da coluna
  q22 <- paste0(q22, q11[i]) # juntando todas as linhas
}

q3 <- NA # crio um vetor vazio
for (i in 1:nchar(q2)) { # faco um for de 1 ate o numero de caracteres do meu vetor em conjunto
  q3 <- c(q3, substr(q2, i, i)) #  junto o q3 (que é NA no início)mais o q2 sendo que eu substituo o carcter 1 no primeiro lugar, depois o 2 no segundo e sucessivamente
}
q3 <- q3[-1] # tiro o primeiro elemento que era NA
q33 <- NA # crio um vetor vazio
for (i in 1:nchar(q22)) { # faco um for de 1 ate o numero de caracteres do meu vetor em conjunto
  q33 <- c(q33, substr(q22, i, i)) #  junto o q3 (que é NA no início)mais o q2 sendo que eu substituo o carcter 1 no primeiro lugar, depois o 2 no segundo e sucessivamente
}
q33 <- q33[-1] # tiro o primeiro elemento que era NA

q4 <- grep('[^[:alnum:]]', q3, value = T) # pega os simbolos
q44 <- grep('[^[:alnum:]]', q33, value = T) # pega os simbolos

u1 <- unique(q4) # tiro os repetidos, pra ver quais simbolos tenho de um jeito + limpo
u11 <- unique(q44) # tiro os repetidos, pra ver quais simbolos tenho de um jeito + limpo

u2 <- u1[-4] # tirando o simbolo de ponto
u22 <- u11[-4] # tirando o simbolo de ponto

# tenho que tirar um de cada vez
e <- gsub(u2[1],'T',rn$lat);ee <- gsub(u22[1],'T',rn$long)
e <- gsub(u2[2],'T',e);ee <- gsub(u22[2],'T',ee)
e <- gsub(u2[3],'T',e);ee <- gsub(u22[3],'T',ee)
e <- gsub(u2[4],'T',e);ee <- gsub(u22[4],'T',ee)
e <- gsub(u2[5],'T',e);ee <- gsub(u22[5],'T',ee)
e <- gsub(u2[6],'T',e);ee <- gsub(u22[6],'T',ee)
e <- gsub('º','T',e);ee <- gsub('º','T',ee)
e;ee

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+');ee <- strsplit(ee,'T+')

# antes pra ver se todas deram certo nomeia por 'LAT' ou 'LONG'
rn$LAT <- convert(e);rn$LONG <- convert(ee) 
is.numeric(rn$LAT);is.numeric(rn$LONG)
rn$LAT;rn$LONG 

# mudar o sinal e colocar na coluna final
rn$lat <- rn$LAT*(-1);rn$long <- rn$LONG*(-1)
rn$lat;rn$long

###### plotar mapa e pontos pra ver se algum caiu fora
#install.packages('ggmap')
library(ggmap)
map_rn <- 'State of Rio Grande do Norte'
rn_map <- qmap(map_rn,zoom=7)
rn_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = rn)
# tirar um pto fora no ArqGIS


###################### RO ######################
# OK
ro0 <- read.csv('RO.csv',header=T,sep=';') 
str(ro0) # 139 rows
ro <- ro0[which(ro0$LATITUDE_S..DECIMAL.!=''),] # eliminando rows sem coords
str(ro) # 139 rows

names(ro)[c(10,11)] <- c('lat','long')
str(ro)

require(stringr)
ro$lat  <- str_trim(ro$lat);ro$long  <- str_trim(ro$long) # retirando espacos em branco antes e depois
head(ro)

ro$lat <- sub(',','.',ro$lat);ro$long <- sub(',','.',ro$long)
str(ro)

####### Ver que simbolos tem 
q1 <- ro$lat;q11 <- ro$long
head(q1);head(q11)

q2 <- q1[1] # o primeiro linha da coluna  q escolhi
for (i in 2:length(q1)) { # do segundo da coluna (pq o primeiro ja pequei) ate o comprimento da coluna
  q2 <- paste0(q2, q1[i]) # juntando todas as linhas
}
q22 <- q11[1] # o primeiro linha da coluna  q escolhi
for (i in 2:length(q11)) { # do segundo da coluna (pq o primeiro ja pequei) ate o comprimento da coluna
  q22 <- paste0(q22, q11[i]) # juntando todas as linhas
}

q3 <- NA # crio um vetor vazio
for (i in 1:nchar(q2)) { # faco um for de 1 ate o numero de caracteres do meu vetor em conjunto
  q3 <- c(q3, substr(q2, i, i)) #  junto o q3 (que é NA no início)mais o q2 sendo que eu substituo o carcter 1 no primeiro lugar, depois o 2 no segundo e sucessivamente
}
q3 <- q3[-1] # tiro o primeiro elemento que era NA
q33 <- NA # crio um vetor vazio
for (i in 1:nchar(q22)) { # faco um for de 1 ate o numero de caracteres do meu vetor em conjunto
  q33 <- c(q33, substr(q22, i, i)) #  junto o q3 (que é NA no início)mais o q2 sendo que eu substituo o carcter 1 no primeiro lugar, depois o 2 no segundo e sucessivamente
}
q33 <- q33[-1] # tiro o primeiro elemento que era NA

q4 <- grep('[^[:alnum:]]', q3, value = T) # pega os simbolos
q44 <- grep('[^[:alnum:]]', q33, value = T) # pega os simbolos

u1 <- unique(q4) # tiro os repetidos, pra ver quais simbolos tenho de um jeito + limpo
u11 <- unique(q44) # tiro os repetidos, pra ver quais simbolos tenho de um jeito + limpo

u2 <- u1[-3] # tirando o simbolo de ponto
u22 <- u11[-3] # tirando o simbolo de ponto

# tenho que tirar um de cada vez
e <- gsub(u2[1],'T',ro$lat);ee <- gsub(u22[1],'T',ro$long)
e <- gsub(u2[2],'T',e);ee <- gsub(u22[2],'T',ee)
e;ee

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+');ee <- strsplit(ee,'T+')

# antes pra ver se todas deram certo nomeia por 'LAT' ou 'LONG'
ro$LAT <- convert(e);ro$LONG <- convert(ee) 
is.numeric(ro$LAT);is.numeric(ro$LONG)
ro$LAT;ro$LONG 

# mudar o sinal e colocar na coluna final
ro$lat <- ro$LAT*(-1);ro$long <- ro$LONG*(-1)
ro$lat;ro$long

###### plotar mapa e pontos pra ver se algum caiu fora
#install.packages('ggmap')
library(ggmap)
map_ro <- 'State of Rondônia'
ro_map <- qmap(map_ro,zoom=6)
w <- ro_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                  data = ro)
ggplot_build(w) # ro$lat[90] ro$long[c(70,72)] ro[30]

# 4 missing rows
ro[c(30,70,72,90),c('lat','long')]
# ro[c(30,72)] - erro 
# ro[c(70,90)] - mar

# tirar ptos errados
ro <- ro[-c(30,70,72,90),]
str(ro) # 135 prop com coords


###################### RS ######################
# quase ok, tirar uns ptos fora com o ArqGIIS
rs0 <- read.csv('RS.csv',header=T,sep=';') 
str(rs0) # 7788 rows
rs1 <- rs0[complete.cases(rs0$LATITUDE_S..DECIMAL.),] # eliminando rows sem coords
str(rs1) # 5192 rows
rs <- rs1[complete.cases(rs0$LONGITUDE_W..DECIMAL.),] # eliminando rows sem coords
str(rs) # 5191 prop com coords

names(rs)[c(8,9)] <- c('lat','long')
str(rs)

require(stringr)
rs$lat  <- str_trim(rs$lat);rs$long  <- str_trim(rs$long) # retirando espacos em branco antes e depois
head(rs)

rs$lat <- sub(',','.',rs$lat);rs$long <- sub(',','.',rs$long)
str(rs)

####### Ver que simbolos tem 
q1 <- rs$lat;q11 <- rs$long
head(q1);head(q11)

q2 <- q1[1] # o primeiro linha da coluna  q escolhi
for (i in 2:length(q1)) { # do segundo da coluna (pq o primeiro ja pequei) ate o comprimento da coluna
  q2 <- paste0(q2, q1[i]) # juntando todas as linhas
}
q22 <- q11[1] # o primeiro linha da coluna  q escolhi
for (i in 2:length(q11)) { # do segundo da coluna (pq o primeiro ja pequei) ate o comprimento da coluna
  q22 <- paste0(q22, q11[i]) # juntando todas as linhas
}

q3 <- NA # crio um vetor vazio
for (i in 1:nchar(q2)) { # faco um for de 1 ate o numero de caracteres do meu vetor em conjunto
  q3 <- c(q3, substr(q2, i, i)) #  junto o q3 (que é NA no início)mais o q2 sendo que eu substituo o carcter 1 no primeiro lugar, depois o 2 no segundo e sucessivamente
}
q3 <- q3[-1] # tiro o primeiro elemento que era NA
q33 <- NA # crio um vetor vazio
for (i in 1:nchar(q22)) { # faco um for de 1 ate o numero de caracteres do meu vetor em conjunto
  q33 <- c(q33, substr(q22, i, i)) #  junto o q3 (que é NA no início)mais o q2 sendo que eu substituo o carcter 1 no primeiro lugar, depois o 2 no segundo e sucessivamente
}
q33 <- q33[-1] # tiro o primeiro elemento que era NA

q4 <- grep('[^[:alnum:]]', q3, value = T) # pega os simbolos
q44 <- grep('[^[:alnum:]]', q33, value = T) # pega os simbolos

u1 <- unique(q4) # tiro os repetidos, pra ver quais simbolos tenho de um jeito + limpo
u11 <- unique(q44) # tiro os repetidos, pra ver quais simbolos tenho de um jeito + limpo

# dados sem ponto, transformar pra decimal
a <- str_length(rs$lat) # vendo quantos digitos tem em cada
unique(a) # para isolar os repetidos e saber qtos caracteres tem cada, tem NAs

rsy <- rs[complete.cases(rs$lat),] # tirando NAs
rs <- rsy[complete.cases(rsy$long),] # tirando NAs
str(rs) # 3912 prop com coords

# novamente: dados sem ponto, transformar pra decimal
a <- str_length(rs$lat); # vendo quantos digitos tem em cada
unique(a) # para isolar os repetidos e saber qtos caracteres tem cada
str(rs)
rs$lat <- as.numeric(rs$lat);rs$long <- as.numeric(rs$long) # tranaformar p/numeric
is.numeric(rs$lat);is.numeric(rs$long)
rs$lat <- rs$lat/1000000

aa <- str_length(rs$long)
unique(aa)

dec.rs <- function(x){
  if(str_length(x)==8){
    x/100000
  }
  if(str_length(x)==9){
    x/1000000
  } 
}
bb <- dec.rs(rs$long)
head(bb);tail(bb)
head(rs$long);tail(rs$long)
rs$long <- bb
rs$lat;rs$long

###### plotar mapa e pontos pra ver se algum caiu fora
#install.packages('ggmap')
library(ggmap)
map_rs <- 'State of Rio Grande do Sul'
rs_map <- qmap(map_rs,zoom=6)
rs_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = rs)
# tirar uns ptos fora com o ArqGIIS




# funcao pra converter em coordenada decimal
convert<-function(coord){
  for (i in 1:length(coord)){
    dec=c(as.numeric(coord[[i]][1]),as.numeric(coord[[i]][2]),as.numeric(coord[[i]][3]))
    coord[i]<-abs(dec[1])+dec[2]/60+dec[3]/3600  
  }
  return((as.numeric(coord)))
}
