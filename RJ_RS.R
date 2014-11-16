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
# 
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
# tirar um pto fora de RN no ArqGIS




# funcao pra converter em coordenada decimal
convert<-function(coord){
  for (i in 1:length(coord)){
    dec=c(as.numeric(coord[[i]][1]),as.numeric(coord[[i]][2]),as.numeric(coord[[i]][3]))
    coord[i]<-abs(dec[1])+dec[2]/60+dec[3]/3600  
  }
  return((as.numeric(coord)))
}
