################### AC ################### 
ac <- read.csv('AC.csv', header=TRUE, sep=';') # 8282 propriedades
str(ac)

ac.coord <- ac[which(ac$lat!=''),] # todas as linhas tem coordenadas
ac.coord$lat <- as.numeric(sub(',','.',ac.coord$lat)); ac.coord$long <- as.numeric(sub(',','.',ac.coord$long))
str(ac.coord)

library(ggmap)
ac_map <- qmap('State of Acre',zoom=6)
ac_map+geom_point(aes(x = long, y = lat, color = 'red' ), 
                  data = ac.coord)
write.csv2(ac.coord,file='AC_mrq.csv')

###################### AM ######################
am <- read.csv('AM.csv',header=TRUE,sep=';') # 64 propriedades
names(am)[c(11,12)] <- c('lat','long')
str(am)

am.coord <- am[which(am$lat!=''),]
am.coord <- am.coord[complete.cases(am.coord$lat),]
str(am.coord)

require(stringr)
am.coord$lat  <- str_trim(sub(',','.',am.coord$lat));am.coord$long  <- str_trim(sub(',','.',am.coord$long))
str(am.coord)


simb_lat <- am.coord$lat;simb_long <- am.coord$long
head(simb_lat);head(simb_long)

simb_lat2 <- simb_lat[1]
for (i in 2:length(simb_lat)) {
  simb_lat2 <- paste0(simb_lat2, simb_lat[i])
}
simb_long2 <- simb_long[1]
for (i in 2:length(simb_long)) {
  simb_long2 <- paste0(simb_long2, simb_long[i])
}

simb_lat3 <- NA
for (i in 1:nchar(simb_lat2)) {
  simb_lat3 <- c(simb_lat3, substr(simb_lat2, i, i))
}
simb_lat3 <- simb_lat3[-1]
simb_long3 <- NA
for (i in 1:nchar(simb_long2)) {
  simb_long3 <- c(simb_long3, substr(simb_long2, i, i))
}
simb_long3 <- simb_long3[-1]

simb_lat4 <- grep('[^[:alnum:]]', simb_lat3, value = T)
simb_long4 <- grep('[^[:alnum:]]', simb_long3, value = T)

u_lat <- unique(simb_lat4)
u_long <- unique(simb_long4)

u_lat2 <- u_lat[-2]
u_long2 <- u_long[-2]

e <- gsub(u_lat2[1],'T',am.coord$lat);ee <- gsub(u_long2[1],'T',am.coord$long)
e <- gsub(u_lat2[2],'T',e);ee <- gsub(u_long2[2],'T',ee)
e <- gsub(u_lat2[3],'T',e);ee <- gsub(u_long2[3],'T',ee)
e <- gsub(u_lat2[4],'T',e);ee <- gsub(u_long2[4],'T',ee)
e <- gsub(u_lat2[5],'T',e);ee <- gsub(u_long2[5],'T',ee)
#e <- gsub(u_lat2[6],'T',e);
ee <- gsub(u_long2[6],'T',ee)
e;ee

e <- strsplit(e,'T+');ee <- strsplit(ee,'T+')

convert<-function(coord){
  for (i in 1:length(coord)){
    dec=c(as.numeric(coord[[i]][1]),as.numeric(coord[[i]][2]),as.numeric(coord[[i]][3]))
    coord[i]<-abs(dec[1])+dec[2]/60+dec[3]/3600  
  }
  return((as.numeric(coord)))
}

am.coord$LAT <- convert(e);am.coord$LONG <- convert(ee) 
is.numeric(am.coord$LAT);is.numeric(am.coord$LONG)
am.coord$LAT;am.coord$LONG

y <- gsub(u_lat2[1],'T',am.coord$lat[c(22,26)])
y <- gsub(u_lat2[2],'T',y)
y <- gsub(u_lat2[3],'T',y)
y <- gsub(u_lat2[4],'T',y)
y <- gsub(u_lat2[5],'T',y)
y <- strsplit(y,'T+')

dec1=c(as.numeric(y[[1]][1]),as.numeric(y[[1]][2]))
dec2=c(as.numeric(y[[2]][1]),as.numeric(y[[2]][2]))
am.coord$LAT[22] <- abs(dec1[1])+dec1[2]/60+0/3600
am.coord$LAT[26] <- abs(dec2[1])+dec2[2]/60+0/3600
am.coord$LAT;am.coord$LONG

am.coord$lat <- am.coord$LAT*(-1);am.coord$long <- am.coord$LONG*(-1)
am.coord$lat;am.coord$long

library(ggmap)
qmap('Amazonas',zoom=5)+geom_point(aes(x = long, y = lat , color = 'red'), 
                  data = am.coord)

write.csv2(am.coord,file='AM_mrq.csv')


###################### BA ######################
ba <- read.csv('BA.csv',header=TRUE,sep=';') # 509 propriedades
str(ba)
levels(ba$lat)
ba.coord <- ba[which(ba$lat!="Não informado"),]
ba.coord <- ba.coord[which(ba.coord$lat!=""),]
str(ba.coord) # 422 com coord

require(stringr)
ba.coord$lat  <- str_trim(sub(',','.',ba.coord$lat));ba.coord$long  <- str_trim(sub(',','.',ba.coord$long))
str(ba.coord)

simb_lat <- ba.coord$lat;simb_long <- ba.coord$long
head(simb_lat);head(simb_long)

simb_lat2 <- simb_lat[1]
for (i in 2:length(simb_lat)) {
  simb_lat2 <- paste0(simb_lat2, simb_lat[i])
}
simb_long2 <- simb_long[1]
for (i in 2:length(simb_long)) {
  simb_long2 <- paste0(simb_long2, simb_long[i])
}

simb_lat3 <- NA
for (i in 1:nchar(simb_lat2)) {
  simb_lat3 <- c(simb_lat3, substr(simb_lat2, i, i))
}
simb_lat3 <- simb_lat3[-1]
simb_long3 <- NA
for (i in 1:nchar(simb_long2)) {
  simb_long3 <- c(simb_long3, substr(simb_long2, i, i))
}
simb_long3 <- simb_long3[-1]

simb_lat4 <- grep('[^[:alnum:]]', simb_lat3, value = T)
simb_long4 <- grep('[^[:alnum:]]', simb_long3, value = T)

u_lat <- unique(simb_lat4)
u_long <- unique(simb_long4)

u_lat2 <- u_lat[-3]
u_long2 <- u_long[-3]

e <- gsub(u_lat2[1],'T',ba.coord$lat);ee <- gsub(u_long2[1],'T',ba.coord$long)
e <- gsub(u_lat2[2],'T',e);ee <- gsub(u_long2[2],'T',ee)
e <- gsub(u_lat2[3],'T',e);ee <- gsub(u_long2[3],'T',ee)
e <- gsub(u_lat2[4],'T',e);ee <- gsub(u_long2[4],'T',ee)
e <- gsub(u_lat2[5],'T',e);ee <- gsub(u_long2[5],'T',ee)
e <- gsub(u_lat2[6],'T',e);ee <- gsub(u_long2[6],'T',ee)

e <- strsplit(e,'T+');ee <- strsplit(ee,'T+')

ba.coord$LAT <- convert(e);ba.coord$LONG <- convert(ee) 
ba.coord$LAT;ba.coord$LONG
str(ba)

row.names(ba.coord[!complete.cases(ba.coord$LAT),]) # 86, 98, 100, 160, 300
row.names(ba.coord[!complete.cases(ba.coord$LONG),]) # 98, 291

y <- gsub(u_lat2[1],'T',ba.coord[c('86', '98', '100', '160', '300'),'lat'])
y <- gsub(u_lat2[2],'T',y)
y <- gsub(u_lat2[3],'T',y)
y <- gsub(u_lat2[4],'T',y)
y <- gsub(u_lat2[5],'T',y)
y <- gsub(u_lat2[6],'T',y)
y <- gsub('º','T',y)
y <- strsplit(y,'T+')
y <- convert(y)
ba.coord[c('86', '98', '100', '160', '300'),'LAT'] <- y

y <- gsub(u_long2[1],'T',ba.coord[c('98','291'),'long'])
y <- gsub(u_long2[2],'T',y)
y <- gsub(u_long2[3],'T',y)
y <- gsub('º','T',y)
y <- strsplit(y,'T+')
y <- convert(y)
ba.coord[c('98','291'),'LONG'] <- y

ba.coord$LAT;ba.coord$LONG
ba.coord$lat <- ba.coord$LAT*(-1);ba.coord$long <- ba.coord$LONG*(-1)
ba.coord$lat;ba.coord$long

library(ggmap)
qmap('State of Bahia',zoom=6)+geom_point(aes(x = long, y = lat , color = 'red'), 
                  data = ba.coord)

write.csv2(ba.coord,file='BA_mrq.csv')


###################### CE ######################
# OK
ce0 <- read.csv('CE2.csv',header=TRUE,sep=';') # 79 propriedades
str(ce0)
names(ce0)[c(13,16)] <- c('lat','long')

# removendo linhas sem coordenada
ce <- ce0[which(ce0$lat != ""),]
str(ce) # 66 prop c/coord
levels(ce$lat)

require(stringr)
ce$lat  <- str_trim(ce$lat);ce$long  <- str_trim(ce$long) # retirando espacos em branco antes e depois
str(ce)
ce$lat <- sub(',','.',ce$lat);ce$long <- sub(',','.',ce$long)
str(ce)

####### Ver que simbolos tem 
q1 <- ce$lat;q11 <- ce$long
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

q4 <- grep('[^[:digit:]]', q3, value = T) # pega os simbolos
q44 <- grep('[^[:digit:]]', q33, value = T) # pega os simbolos

u1 <- unique(q4) # tiro os repetidos, pra ver quais simbolos tenho de um jeito + limpo
u11 <- unique(q44) # tiro os repetidos, pra ver quais simbolos tenho de um jeito + limpo

u2 <- u1[-c(1,2)] # tiro o ponto e o negativo
u22 <- u11[-c(1,2)] # tiro o ponto e o negativo

d1 <- grep('^-',ce$lat) # vejo em que linhas estao os certos
d11 <- grep('^-',ce$long) # vejo em que linhas estao os certos

# tenho que tirar um de cada vez e dps converter
e <- ce$lat[-d1];ee <- ce$long[-d11] # tiro os que nao irei converter e comeco a mudar os simbolos
e <- gsub(u2[1],'T',e);ee <- gsub(u22[1],'T',ee)
e <- gsub(u2[2],'T',e);ee <- gsub(u22[2],'T',ee)
ee <- gsub(u22[3],'T',ee)
e;ee

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+')
ee <- strsplit(ee,'T+')

# antes pra ver se todas deram certo nomeia por 'f' e 'ff'
f <- convert(e);ff <- convert(ee) 
is.numeric(f);is.numeric(ff)
f;ff
# trocando o sinal
f <- f*(-1);ff <- ff*(-1)
f;ff

# arrumar os NAs formados e[c(4,10)]
e <- e[c(4,10)]
is.character(e)
is.character(e[1])
e[[1]] <- sub(',','.',e[[1]])
e[[1]][2] <- sub('21.','21',e[[1]][2])
e[[2]][3] <- paste0(e[[2]][3],e[[2]][4])
e[[2]][2] <- sub('07.','7',e[[2]][2])
e[[2]][3] <- sub(',','.',e[[2]][3])
b <- convert(e)
b <- b*(-1)
f[c(4,10)] <- b
f;ff

# colocando os convertidos no lugar certo: ce$lat e ce$long [c(3,21:50,52:64,66)] 
ce$lat[c(3,21:50,52:64,66)]  <- f
ce$long[c(3,21:50,52:64,66)] <- ff
ce$lat;ce$long
is.numeric(ce$lat);is.numeric(ce$long)
ce$lat <- as.numeric(ce$lat);ce$long <- as.numeric(ce$long)
str(ce)
ce$lat;ce$long

###### plotar mapa e pontos pra ver se algum caiu fora do AM
#install.packages('ggmap')
library(ggmap)
map_ce <- 'State of Ceará'
ce_map <- qmap(map_ce,zoom=7)
w <- ce_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = ce)
ggplot_build(w) # pra descobrir a linha removida: ponto ce$lat[15]
ce$lat[15]
ce$lat;ce$long
ce$lat[15] <- ce$lat[15]/100000 #faltava um ponto, resolvendo a divisao deu certo

### transformar p/.csv e depois .xls 
write.csv2(ce,file='CE_mrq.csv',sep=';')


###################### DF ######################
# quase ok
# retirar rows com ptos fora de bsb pelo ArqGIS
df0 <- read.csv('DF.csv',header=TRUE,sep=';') # 208 propriedades
str(df0)

df0$UF_ESTABEL <- as.character(df0$UF_ESTABEL)
df0[which(df0$UF_ESTABEL=='DISTRITO FEDERAL'),'UF_ESTABEL']='DF'

names(df0)[c(11,12)] <- c('lat','long')
names(df0)
df0$lat[1:10]
df <- df0
str(df)
df$lat <- as.character(df$lat);df$long <- as.character(df$long)
str(df)
df$lat  <- str_trim(df$lat);df$long  <- str_trim(df$long) # retirando espacos em branco antes e depois
df$lat <- sub(',','.',df$lat);df$long <- sub(',','.',df$long)
str(df)

q1 <- df$lat;q11 <- df$long # verificando as coordenadas
a1 <- grep('[^[:alnum:]]',q1) # vejo as linhas que tem numero e ponto
a11 <- grep('[^[:alnum:]]',q11) # vejo as linhas que tem numero e ponto
length(q1);length(a1);length(q11);length(a11) # comparo o tam do original com o que eu vi que tem ponto e numero

df$lat[-a1] # linha com valor: "155447"
df$long[-a11] # linhas com valores "481501"  "4752531" "48102"

df[which(df$lat=="155447"),] # df$lat[109]
df[which(df$long=="481501"),] # df$long[109]
df[which(df$long=="4752531"),] # df$long[115]
df[which(df$long=="48102"),] # df$long[185]
is.numeric(df$lat);is.numeric(df$long)
df$lat <- as.numeric(df$lat);df$long <- as.numeric(df$long)

# colocar ponto no lugar certo
df$lat[109] <- df$lat[109]/10000
df$long[109] <- df$long[109]/10000
df$long[115] <- df$long[115]/100000
df$long[185] <- df$long[185]/1000
df$lat;df$long
q1 <- df$lat;q11 <- df$long # verificando as coordenadas
a1 <- grep('[^[:alnum:]]',q1) # vejo as linhas que tem numero e ponto
a11 <- grep('[^[:alnum:]]',q11) # vejo as linhas que tem numero e ponto
length(q1);length(a1);length(q11);length(a11) # comparo o tam do original com o que eu vi que tem ponto e numero

is.numeric(df$lat);is.numeric(df$long)
df$lat <- df$lat*(-1);df$long <- df$long*(-1)
df$lat;df$long

###### plotar mapa e pontos pra ver se algum caiu fora do DF # verificar daqui pra cima
#install.packages('ggmap')
library(ggmap)
map_df <- 'Distrito Federal, Brazil'
df_map <- qmap(map_df,zoom=9)
w <- df_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = df)
ww <- ggplot_build(w) # pra descobrir linhas removidas
# df$lat[70] df$long[108] 
df$lat[70];df$long[108]
df[c(70,108),c(11,12)] 

# remover ambas linhas
df <- df[-c(70,108),]
str(df) # 206 prop com coord
# retirar rows com ptos fora de bsb pelo ArqGIS

### transformar p/.csv e depois .xls 
write.csv2(df,file='DF_mrq_ArqGIS.csv',sep=';')


###################### ES ######################
# quase ok
# retirar rows com ptos no mar pelo ArqGIS
es0 <- read.csv('ES.csv',header=T,sep=';') # 456 propriedades
es <- es0[which(es0$LATITUDE_S..DECIMAL.!=""),]
str(es0)
names(es)[c(8,9)] <- c('lat','long')
str(es) # 456 com coord

require(stringr)
es$lat  <- str_trim(es$lat);es$long  <- str_trim(es$long) # retirando espacos em branco antes e depois
str(es)
es$lat <- sub(',','.',es$lat);es$long <- sub(',','.',es$long)
str(es)
es$lat;es$long

####### Ver que simbolos tem 
q1 <- es$lat;q11 <- es$long
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
e <- gsub(u2[1],'T',es$lat);ee <- gsub(u22[1],'T',es$long)
e <- gsub(u2[2],'T',e);ee <- gsub(u22[2],'T',ee)
e <- gsub(u2[3],'T',e);ee <- gsub(u22[3],'T',ee)
e <- gsub(u2[4],'T',e);ee <- gsub(u22[4],'T',ee)
e <- gsub(u2[5],'T',e);ee <- gsub(u22[5],'T',ee)
e <- gsub(u2[6],'T',e)
e;ee
# separando pelo simbolo que coloquei
e <- strsplit(e,'T+')
ee <- strsplit(ee,'T+')

# antes pra ver se todas deram certo nomeia por 'LAT' ou 'LONG'
es$LAT <- convert(e);es$LONG <- convert(ee) 
is.numeric(es$LAT);is.numeric(es$LONG)
es$LAT;es$LONG

# mudar o sinal
es$lat <- es$LAT*(-1);es$long <- es$LONG*(-1)
es$lat;es$long

###### plotar mapa e pontos pra ver se algum caiu fora do DF
#install.packages('ggmap')
library(ggmap)
map_es <- 'Espírito Santo'
es_map <- qmap(map_es,zoom=7)
w <- es_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = es)
ggplot_build(w) # pra descobrir a linha removida: ponto es$lat[418] - parece erro de digitacao
es$lat[418];es$long[418]
# arrumando
es[418,c('lat','long')] # mar
dec=c(as.numeric(e[[418]][1]),as.numeric(e[[418]][2]),as.numeric(e[[418]][3]))
abs(dec[1])+dec[2]/60+dec[3]/3600

# retirando ponto
es <- es[-418,]
str(es) # 455 prop com coord
# retirar rows com ptos no mar pelo ArqGIS

### transformar p/.csv e depois .xls 
write.csv2(es,file='ES_mrq_ArqGIS.csv',sep=';')
