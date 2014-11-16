###################### GO ######################
# OK
go0 <- read.csv('GO.csv',header=TRUE,sep=';') # 873 propriedades
str(go0)
names(go0)[c(9,10)] <- c('lat','long')

go <- go0[complete.cases(go0[,9:10]),] # outro jeito de tirar rows com NA
str(go) # 739 com coord

go$LAT <- NA;go$LONG <- NA # criar coluna pra arrumar coordenadas
str(go)
q1 <- go$lat;q11 <- go$long
str(q1);str(q11)

a1 <- nchar(q1)
unique(a1) # 4:7

a2 <- which(nchar(q1)==4)
for (i in 1:length(a2)){
  go$LAT[a2[i]] <- go$lat[a2[i]]/100    
}

a2 <- which(nchar(q1)==5)
for (i in 1:length(a2)){
  go$LAT[a2[i]] <- go$lat[a2[i]]/1000    
}

a2 <- which(nchar(q1)==6)
for (i in 1:length(a2)){
  go$LAT[a2[i]] <- go$lat[a2[i]]/10000    
}

a2 <- which(nchar(q1)==7)
for (i in 1:length(a2)){
  go$LAT[a2[i]] <- go$lat[a2[i]]/100000
}
go$lat;go$LAT


a11 <- nchar(q11)  
unique(a11) # 5:7

a2 <- which(nchar(q11)==5)
for (i in 1:length(a2)){
  go$LONG[a2[i]] <- go$long[a2[i]]/1000    
}

a2 <- which(nchar(q11)==6)
for (i in 1:length(a2)){
  go$LONG[a2[i]] <- go$long[a2[i]]/10000    
}

a2 <- which(nchar(q11)==7)
for (i in 1:length(a2)){
  go$LONG[a2[i]] <- go$long[a2[i]]/100000
}
go$long;go$LONG

is.numeric(go$lat);is.numeric(go$LAT);is.numeric(go$long);is.numeric(go$LONG)
go$lat <- go$LAT*(-1);go$long <- go$LONG*(-1)
go$lat;go$long

###### plotar mapa e pontos pra ver se algum caiu fora do DF
#install.packages('ggmap')
library(ggmap)
map_go <- 'Goiás'
go_map <- qmap(map_go,zoom=7)
w <- go_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = go)
ggplot_build(w) # pra descobrir a linha removida: ponto go$long[67]
# go[67,c(9,10)] o ponto cai em MG

# retirar ponto
go <- go[-67,]
str(go) # 738 pop com coords


###################### MA ######################
# OK
ma0 <- read.csv('MA.csv',header=T,sep=';') # 70 propriedades
ma <- ma0[which(ma0$LATITUDE_S..DECIMAL.!=''),] # removendo linhas sem coordenadas - todas tem coordenadas
names(ma)[c(11,12)] <- c('lat','long')
str(ma) # 70 com coord

ma$lat <- as.character(ma$lat); ma$long <- as.character(ma$long)
str(ma)
ma$lat <- sub(',','.',ma$lat);ma$long <- sub(',','.',ma$long)

require(stringr)
ma$lat  <- str_trim(ma$lat);ma$long  <- str_trim(ma$long) # retirando espacos em branco antes e depois

####### Ver que simbolos tem 
q1 <- ma$lat;q11 <- ma$long
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
e <- gsub(u2[1],'T',ma$lat);ee <- gsub(u22[1],'T',ma$long)
e <- gsub(u2[2],'T',e);ee <- gsub(u22[2],'T',ee)
e <- gsub(u2[3],'T',e);ee <- gsub(u22[3],'T',ee)
e <- gsub(u2[4],'T',e);ee <- gsub(u22[4],'T',ee)
e <- gsub(u2[5],'T',e);ee <- gsub(u22[5],'T',ee)
e <- gsub('º','T',e);ee <- gsub('º','T',ee)

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+')
ee <- strsplit(ee,'T+')

# antes pra ver se todas deram certo nomeia por 'LAT' ou 'LONG'
ma$LAT <- convert(e);ma$LONG <- convert(ee) 
str(ma)
is.numeric(am$LAT);is.numeric(am$LONG)
# ver linhas que tem NA  
ma[!complete.cases(ma$LAT),] # ma$LAT[6]
ma[!complete.cases(ma$LONG),] # nenhuma

############### arrumar ma$LAT[6]
y <- e[[6]]
y <- sub('O','',y)
dec=c(as.numeric(y[1]),as.numeric(y[2]),as.numeric(y[3]))
ma$LAT[6] <- abs(dec[1])+dec[2]/60+0/3600
ma$LAT;ma$LONG

# colocar os valores na coluna certa e trocando o sinal
ma$lat <- ma$LAT*(-1);ma$long <- ma$LONG*(-1)
ma$lat;ma$long

###### plotar mapa e pontos pra ver se algum caiu fora do DF
#install.packages('ggmap')
library(ggmap)
map_ma <- 'Maranhão'
ma_map <- qmap(map_ma,zoom=6)
w <- ma_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = ma)

ggplot_build(w)

# td certo, finalisar igual


###################### MG ######################
# OK
mg0 <- read.csv('MG.csv',header=T,sep=';') # 2477 propriedades
mg <- mg0[which(mg0$LATITUDE_S..DECIMAL.!=''),] # todas as linhas tem coordenadas
str(mg0)
names(mg)[8:9] <- c('lat','long')
str(mg) # 2477 com coord
mg$lat <- as.character(mg$lat);mg$long <- as.character(mg$long)
mg$lat <- sub(',','.',mg$lat);mg$long <- sub(',','.',mg$long)
str(mg$lat);str(mg$long)

# transformar para numero
mg$lat <- as.numeric(mg$lat);mg$long <- as.numeric(mg$long)
str(mg[,8:9])
mg$lat;mg$long

###### plotar mapa e pontos pra ver se algum caiu fora do DF
#install.packages('ggmap')
library(ggmap)
map_mg <- 'Minas Gerais'
mg_map <- qmap(map_mg,zoom=6)
w <- mg_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = mg)

ww <- ggplot_build(w) # pra descobrir as 23 linhas removidas:
ww$data[[4]][!complete.cases(ww$data[[4]][,c(2,3)]),c(2,3)] # 22 pontos fora do mapa
mg[c(171,202,805,861,873,1237,1338,1409,1597,1617),8:9]
# mg[c(171,202,805,861,873,1237,1409,1597,1617)] ponto no mar
# mg[1338] erro de digitacao total hahahahaha
mg[c(549,857,879,1244,1313,1354,1380),8:9]
# mg$lat[549] ponto na Bahia
# mg$lat[857,879,1244,1313,1354,1380] ponto no mar
mg[c(531,1309,1358,1364,2075),8:9] #long
# mg$long[c(531,1364,2075)] ponto no mar
# mg$long[c(1309,1358)] lat e long iguais

mg <- mg[-c(171,202,805,861,873,1237,1338,1409,1597,1617),]
str(mg) # 2467 c/coords, ainda mising rows
ww$data[[4]][!complete.cases(ww$data[[4]][,c(2,3)]),c(2,3)] # 12 pontos fora do mapa
#  mg$lat[c(547,854,874,1238,1307,1347,1373)] mg$long[c(529,1303,1351,1357,2065)] 
mg[c(547,854,874,1238,1307,1347,1373,529,1303,1351,1357,2065),8:9]
# mg[549] - BA
# outros estaos errados

# tirar ptos fora
mg <- mg[-c(547,854,874,1238,1307,1347,1373,529,1303,1351,1357,2065),]
str(mg) # 2455 com coords


###################### MS ######################
# OK
ms0 <- read.csv('MS.csv',header=T,sep=';') # 658 propriedades

ms <- ms0[which(ms0$LATITUDE_S..DECIMAL.!=''),] # removendo linhas sem coordenadas
ms <- ms[which(ms$LATITUDE_S..DECIMAL.!=0),] # 604 com coord
str(ms0)
names(ms)[c(11,12)] <- c('lat','long')
str(ms)

ms$lat <- as.character(ms$lat); ms$long <- as.character(ms$long)
str(ms)
ms$lat <- sub(',','.',ms$lat);ms$long <- sub(',','.',ms$long)

ms$lat  <- str_trim(ms$lat);ms$long  <- str_trim(ms$long) # retirando espacos em branco antes e depois
str(ms)

####### Ver que simbolos tem 
q1 <- ms$lat;q11 <- ms$long
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
e <- gsub(u2[1],'T',ms$lat);ee <- gsub(u22[1],'T',ms$long)
e <- gsub(u2[2],'T',e);ee <- gsub(u22[2],'T',ee)
e <- gsub(u2[3],'T',e);ee <- gsub(u22[3],'T',ee)
e <- gsub(u2[4],'T',e);ee <- gsub(u22[4],'T',ee)
e <- gsub(u2[5],'T',e);ee <- gsub(u22[5],'T',ee)
ee <- gsub(u22[6],'T',ee)

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+')
ee <- strsplit(ee,'T+')

# antes pra ver se todas deram certo nomeia por 'LAT' ou 'LONG'
ms$LAT <- convert(e);ms$LONG <- convert(ee) 
is.numeric(ms$LAT);is.numeric(ms$LONG)
ms$LAT;ms$LONG

# ver linhas que tem NA  
ms[!complete.cases(ms$LAT),] # ms$LAT[c(169,363:487,593:597,599)]
ms[!complete.cases(ms$LONG),] # ms$LONG[c(169,363:487,584,593,594,596,597,599)] 

# arrumando lat
q1 <- ms$lat[c(169,363:487,593:597,599)] # ms$lat[169] ta errada
head(q1)

q2 <- q1[1] # o primeiro linha da coluna  q escolhi
for (i in 2:length(q1)) { # do segundo da coluna (pq o primeiro ja pequei) ate o comprimento da coluna
  q2 <- paste0(q2, q1[i]) # juntando todas as linhas
}

q3 <- NA # crio um vetor vazio
for (i in 1:nchar(q2)) { # faco um for de 1 ate o numero de caracteres do meu vetor em conjunto
  q3 <- c(q3, substr(q2, i, i)) #  junto o q3 (que é NA no início)mais o q2 sendo que eu substituo o carcter 1 no primeiro lugar, depois o 2 no segundo e sucessivamente
}
q3 <- q3[-1] # tiro o primeiro elemento que era NA
q4 <- grep('[^[:alnum:]]', q3, value = T) # pega os simbolos
u1 <- unique(q4) # tiro os repetidos, pra ver quais simbolos tenho de um jeito + limpo
u2 <- u1[-1] # tirando o simbolo de ponto

e <- gsub(u2[1],'T',ms$lat[c(169,363:487,593:597,599)])
e <- gsub(u2[2],'T',e)
e <- gsub(u2[3],'T',e)
e <- gsub(u2[4],'T',e)
e <- gsub('º','T',e)
e <- strsplit(e,'T+')

ms$LAT[c(169,363:487,593:597,599)] <- convert(e) 
ms$LAT # ms$LAT[c(169,584)] ainda NA, mas 169 esta errado

ms$lat[584] # arrumar
e <- gsub(u2[1],'T',ms$lat[584])
e <- gsub(u2[2],'T',e)
e <- gsub(u2[3],'T',e)
e <- gsub(u2[4],'T',e)
e <- gsub('º','T',e)
e <- strsplit(e,'T+')
ms$LAT[584] <- convert(e)
ms$LAT

# ms$LONG[c(169,363:487,584,593,594,596,597,599)] 
# arrumando long
q1 <- ms$long[c(169,363:487,584,593,594,596,597,599)]
head(q1)

q2 <- q1[1] # o primeiro linha da coluna  q escolhi
for (i in 2:length(q1)) { # do segundo da coluna (pq o primeiro ja pequei) ate o comprimento da coluna
  q2 <- paste0(q2, q1[i]) # juntando todas as linhas
}

q3 <- NA # crio um vetor vazio
for (i in 1:nchar(q2)) { # faco um for de 1 ate o numero de caracteres do meu vetor em conjunto
  q3 <- c(q3, substr(q2, i, i)) #  junto o q3 (que é NA no início)mais o q2 sendo que eu substituo o carcter 1 no primeiro lugar, depois o 2 no segundo e sucessivamente
}
q3 <- q3[-1] # tiro o primeiro elemento que era NA
q4 <- grep('[^[:alnum:]]', q3, value = T) # pega os simbolos
u1 <- unique(q4) # tiro os repetidos, pra ver quais simbolos tenho de um jeito + limpo
u2 <- u1[-1] # tirando o simbolo de ponto

e <- gsub(u2[1],'T',ms$long[c(169,363:487,584,593,594,596,597,599)])
e <- gsub(u2[2],'T',e)
e <- gsub(u2[3],'T',e)
e <- gsub(u2[4],'T',e)
e <- gsub('º','T',e)
e <- strsplit(e,'T+')

ms$LONG[c(169,363:487,584,593,594,596,597,599)] <- convert(e) 
ms$LONG # ms$LONG[c(169,583,437,453,483)] ainda NA, mas 169 esta errado

ms$long[c(169,583,437,453)] # arrumar, menos 169
e <- gsub(u2[1],'T',ms$long[c(169,583,437,453)])
e <- gsub(u2[2],'T',e)
e <- gsub(u2[3],'T',e)
e <- gsub(u2[4],'T',e)
e <- gsub('º','T',e)
e <- strsplit(e,'T+')
ms$LONG[c(169,583,437,453)] <- convert(e)
ms$LONG
e
# arrumando 583
e <- ms$long[583]
e <- gsub(u2[2],'T',e)
e <- gsub('°','T',e)
e <- strsplit(e,'T+')
dec=c(as.numeric(e[[1]][1]),as.numeric(e[[1]][2]),as.numeric(e[[1]][3]))
coord <-abs(dec[1])+dec[2]/60+dec[3]/3600 
ms$LONG[583] <- coord 

# arrumando 437
e <- ms$long[437]
e <- gsub('´','T',e)
e <- gsub('º','T',e)
e <- strsplit(e,'T+')
dec=c(as.numeric(e[[1]][2]),as.numeric(e[[1]][3]),as.numeric(e[[1]][4]))
coord <-abs(dec[1])+dec[2]/60+dec[3]/3600 
ms$LONG[437] <- coord 

# arrumando 453
e <- ms$long[453]
e <- gsub('´','T',e)
e <- gsub('º','T',e)
e <- strsplit(e,'T+')
dec=c(as.numeric(e[[1]][2]),as.numeric(e[[1]][3]),as.numeric(e[[1]][4]))
coord <-abs(dec[1])+dec[2]/60+dec[3]/3600 
ms$LONG[453] <- coord 

# arrumando 483
e <- ms$long[483]
e <- gsub('´','T',e)
e <- gsub('º','T',e)
e <- strsplit(e,'T+')
o <- strsplit(e[[1]][2],1)
o[[1]][1] <- 1
n <- c(e[[1]][1],o) 
dec=c(as.numeric(n[[1]][1]),as.numeric(n[[2]][1]),as.numeric(n[[2]][2]))
coord <-abs(dec[1])+dec[2]/60+dec[3]/3600 
ms$LONG[483] <- coord 
ms$LAT;ms$LONG

# trocando o sinal e colocando na coluna certa
ms$lat <- ms$LAT*(-1);ms$long <- ms$LONG*(-1)
ms$lat;ms$long

# tirar ms[169] que ta errado
ms <- ms[-169,]
str(ms) # 603 prop com coords


###### plotar mapa e pontos pra ver se algum caiu fora do DF
install.packages('ggmap')
library(ggmap)
map_ms <- 'Mato Grosso do Sul'
ms_map <- qmap(map_ms,zoom=6)
w <- ms_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = ms)

ww <- ggplot_build(w) # ms$LONG[214,452,482,532:535,537:541,604] [c(169)] 
ms[c(169,214,452,482,532:535,537:541,604),c('lat','long')]
# [214] Minas, [452] Paraguai, [482] Sao Paulo, [604] Goias - eliminar rows
# [532:535] MS, MS, MS, MS, [537:541] MS, MS, MS, MS, MS - preservar rows

# retirar essas linhas
ms <- ms[-c(214,452,482,537:541),]
str(ms) # 595 prop com coords


###################### MT ######################
# quase ok
# retirar rows com ptos no mar pelo ArqGIS
mt0 <- read.csv('MT.csv',header=T,sep=';') # 2643 propriedades
str(mt0)

mt1 <- mt0[which(mt0$lat_dec!=''),] # tirando rows sem coords
str(mt1) # 2342 prop
mt <- mt1[which(mt1$long_dec!=''),] # tirando rows sem coords
str(mt) # 2241 prop com coords
names(mt)[c(8,10)] <- c('lat','long')

mt$lat <- as.character(mt$lat); mt$long <- as.character(mt$long)
str(mt)
mt$lat <- sub(',','.',mt$lat);mt$long <- sub(',','.',mt$long)
str(mt)


#install.packages("stringr", dependencies=TRUE)
require(stringr)
mt$lat  <- str_trim(mt$lat);mt$long  <- str_trim(mt$long) # retirando espacos em branco antes e depois
head(mt)

####### Ver que simbolos tem 
q1 <- mt$lat;q11 <- mt$long
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
e <- gsub(u2[1],'T',mt$lat);ee <- gsub(u22[1],'T',mt$long)
e;ee

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+');ee <- strsplit(ee,'T+')

# antes pra ver se todas deram certo nomeia por 'LAT' ou 'LONG'
mt$LAT <- convert(e);mt$LONG <- convert(ee) 
is.numeric(mt$LAT);is.numeric(mt$LONG)
mt$LAT;mt$LONG

# trocando o sinal
mt$LAT <- mt$LAT*(-1);mt$LONG <- mt$LONG*(-1)
mt$LAT;mt$LONG


###### plotar mapa e pontos pra ver se algum caiu fora do DF
#install.packages('ggmap')
library(ggmap)
map_mt <- 'Mato Grosso'
mt_map <- qmap(map_mt,zoom=6)
w <- mt_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = mt)

ww <- ggplot_build(w) 
str(ww)
ww$data[[4]][!complete.cases(ww$data[[4]][,c(2,3)]),c(2,3)] # 20 missing rows
# NAs mt$long[c(3,7,262,538,539,588:591,650,701,963,1054,1055,1059,1832)] mt$LAT[c(979,1038)] [c(993,1574)]
mt[c(3,7,262,538,539,588:591,650,701,963,1054,1055,1059,1832,979,1038,993, 1574),c('lat','long')]
# mt[c(3,7)] - Peru; mg[c(650,701)] - MG
# mt[c(262,979,993)] - errado
# mt[c(538,539,588:591,963,1054,1055,1059,1832,1038)] - mar
# mt[1574,c('lat','long')] estão trocados
mt$lat <- mt$LAT;mt$long <- mt$LONG
mt$lat;mt$long
mt$lat[1574] <- mt$LONG[1574]
mt$long[1574] <- mt$LAT[1574]
names(mt)

# reitar essas 19 linhas mt[c(3,7,650,701,262,979,993,538,539,588:591,963,1054,1055,1059,1832,1038),]
mt <- mt[-c(3,7,650,701,262,979,993,538,539,588:591,963,1054,1055,1059,1832,1038),] # 2222 c/coords
str(mt)
# falta tirar ptos em torno do MT no ArqGIS



# funcao pra converter em coordenada decimal
convert<-function(coord){
  for (i in 1:length(coord)){
    dec=c(as.numeric(coord[[i]][1]),as.numeric(coord[[i]][2]),as.numeric(coord[[i]][3]))
    coord[i]<-abs(dec[1])+dec[2]/60+dec[3]/3600  
  }
  return((as.numeric(coord)))
}
