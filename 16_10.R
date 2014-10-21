################### AC ###################
ac <- read.csv('AC.csv', header=TRUE, sep=';')
head(ac)
str(ac)
names(ac)
names(ac)[c(11,12)] <- c('lat','long')
names(ac)
ac <- ac[which(ac$lat!=''),] # todas as linhas tem coordenadas

ac$lat <- as.character(ac$lat); ac$long <- as.character(ac$long)
str(ac)
ac$lat <- sub(',','.',ac$lat); ac$long <- sub(',','.',ac$long)
str(ac)
ac$lat <- as.numeric(ac$lat); ac$long <- as.numeric(ac$long)
str(ac)

grep('º',ac$lat,value=TRUE) # vendo se esta em decimal

# adicionar coluna para dar cor as propriedades pela atvdd
str(ac)
levels(ac$Atividade) # "1", "46", "Produtor Independente"
ac$Atividade <- as.character(ac$Atividade)
ac$Atividade  <- str_trim(ac$Atividade) # retirando espacos em branco antes e depois
ac$Atividade <- as.factor(ac$Atividade)

ac$cor <- NA
str(ac)
ac[which(ac$Atividade=="1"),'cor']=1
ac[which(ac$Atividade=="46"),'cor']=2
ac[which(ac$Atividade=="Produtor Independente"),'cor']=3

#names(am)[c(11,12)] <- c('lat_dec','long_dec')
###### plotar mapa e pontos pra ver se algum caiu fora do AM
#install.packages('ggmap')
library(ggmap)
#ac$cor <- as.integer(ac$cor)
str(ac)
# mapa
map_ac <- 'State of Acre'
ac_map <- qmap(map_ac,zoom=5)
ac_map+geom_point(aes(x = long, y = lat, color = cor ), 
                  data = ac)


###################### AM ######################
am <- read.csv('AM.csv',header=TRUE,sep=';')
str(am)
names(am)[c(11,12)] <- c('lat','long')
am <- am[which(am$lat!=''),] # removendo linhas sem coordenadas
am$lat <- as.character(am$lat); am$long <- as.character(am$long)
str(am)
am$lat <- sub(',','.',am$lat);am$long <- sub(',','.',am$long)

#install.packages("stringr", dependencies=TRUE)
require(stringr)
am$lat  <- str_trim(am$lat);am$long  <- str_trim(am$long) # retirando espacos em branco antes e depois

head(am)
str(am)

####### Ver que simbolos tem 
q1 <- am$lat;q11 <- am$long
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
e <- gsub(u2[1],'T',am$lat);ee <- gsub(u22[1],'T',am$long)
e <- gsub(u2[2],'T',e);ee <- gsub(u22[2],'T',ee)
e <- gsub(u2[3],'T',e);ee <- gsub(u22[3],'T',ee)
e <- gsub(u2[4],'T',e);ee <- gsub(u22[4],'T',ee)
e <- gsub(u2[5],'T',e);ee <- gsub(u22[5],'T',ee)
e <- gsub(u2[6],'T',e);ee <- gsub(u22[6],'T',ee)

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+')
ee <- strsplit(ee,'T+')

# funcao pra converter em coordenada decimal
convert<-function(coord){
  ab <- coord
  for (i in 1:length(coord)){
    dec=c(as.numeric(coord[[i]][1]),as.numeric(coord[[i]][2]),as.numeric(coord[[i]][3]))
    coord[i]<-abs(dec[1])+dec[2]/60+dec[3]/3600
    #ab[i]  
  }
  return((as.numeric(coord)))
}

# antes pra ver se todas deram certo nomeia por 'LAT' ou 'LONG'
am$LAT <- convert(e);am$LONG <- convert(ee) 
is.numeric(am$LAT);is.numeric(am$LONG)
am$LAT;am$LONG

# precisa arrumar o am$LAT[22] refazer a partir do 'e' ou 'ee' 
y <- gsub(u2[1],'T',am$lat[22])
y <- gsub(u2[2],'T',y)
y <- gsub(u2[3],'T',y)
y <- gsub(u2[4],'T',y)
y <- gsub(u2[5],'T',y)
y <- gsub(u2[6],'T',y)
y <- strsplit(y,'T+')

dec=c(as.numeric(y[[1]][1]),as.numeric(y[[1]][2]))#,as.numeric(e[[1]][3]))
am$LAT[22] <- abs(dec[1])+dec[2]/60+0/3600

# colocar os valores na coluna certa
am$lat <- am$LAT;am$long <- am$LONG
am$lat;am$long

# mudar o sinal
str(am)
am$lat <- (am$lat)*(-1);am$long <- (am$long)*(-1)
am$lat;am$long

# adicionar coluna para dar cor as propriedades
str(am)
levels(am$Classificação) # "Granja de Aves de Corte Comerciais" "Granja de Aves Poedeiras de Ovos Comerciais" 
# "Granja Matrizeira", "Granja Ovos Caipira"
am$Classificação <- as.character(am$Classificação)
am$Classificação  <- str_trim(am$Classificação) # retirando espacos em branco antes e depois
levels(am$Classificação)
am$Classificação <- as.factor(am$Classificação)
am$Classificação[14:35]
am$cor <- NA
str(am)
am[which(am$Classificação=="Granja de Aves de Corte Comerciais"),'cor']=1
am[which(am$Classificação=="Granja de Aves Poedeiras de Ovos Comerciais"),'cor']=2
am[which(am$Classificação=="Granja Matrizeira"),'cor']=3
am[which(am$Classificação=="Granja Ovos Caipira"),'cor']=4
am[which(am$Classificação=="Granja de Aves Poedeiras de Ovos Comerciais"),]


###### plotar mapa e pontos pra ver se algum caiu fora do AM
#install.packages('ggmap')
library(ggmap)
am$cor <- as.integer(am$cor)
str(am)
# mapa
map_am <- 'South America'
am_map <- qmap(map_am,zoom=4)
am_map+geom_point(aes(x = long_dec, y = lat_dec , color = cor), 
                  data = am)


###################### BA ######################
ba <- read.csv('BA.csv',header=TRUE,sep=';')
str(ba)
names(ba)[c(8,9)] <- c('lat','long')
names(ba)

# removendo linhas sem coordenada
str(ba)
levels(ba$lat)
ba <- ba[which(ba$lat!="Não informado"),]
ba <- ba[which(ba$lat!=""),]

ba$lat <- as.character(ba$lat); ba$long <- as.character(ba$long)
str(ba)
ba$lat <- sub(',','.',ba$lat);ba$long <- sub(',','.',ba$long)
str(ba)

#install.packages("stringr", dependencies=TRUE)
require(stringr)
ba$lat  <- str_trim(ba$lat);ba$long  <- str_trim(ba$long) # retirando espacos em branco antes e depois

str(ba)


####### Ver que simbolos tem 
q1 <- ba$lat;q11 <- ba$long
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
e <- gsub(u2[1],'T',ba$lat);ee <- gsub(u22[1],'T',ba$long)
e <- gsub(u2[2],'T',e);ee <- gsub(u22[2],'T',ee)
e <- gsub(u2[3],'T',e);ee <- gsub(u22[3],'T',ee)
e <- gsub(u2[4],'T',e);ee <- gsub(u22[4],'T',ee)
e <- gsub(u2[5],'T',e);ee <- gsub(u22[5],'T',ee)
e <- gsub(u2[6],'T',e);ee <- gsub(u22[6],'T',ee)

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+')
ee <- strsplit(ee,'T+')

# antes pra ver se todas deram certo nomeia por 'LAT' ou 'LONG'
ba$LAT <- convert(e);ba$LONG <- convert(ee) 
is.numeric(ba$LAT);is.numeric(ba$LONG)
ba$LAT;ba$LONG
str(ba)

ba[which(ba$LAT == NaN),]
class(ba$LAT[78])
# precisa arrumar os NAs em ba$LAT[78,90,92,151,291,348,360] e ba$LONG[90,282] refazer a partir do 'e' ou 'ee' 
y <- gsub(u2[1],'T',ba$lat[78])
y <- gsub(u2[2],'T',y)
y <- gsub(u2[3],'T',y)
y <- gsub(u2[4],'T',y)
y <- gsub(u2[5],'T',y)
y <- gsub(u2[6],'T',y)
y <- gsub('º','T',y)
y <- strsplit(y,'T+')
# ba$LAT[78]
dec=c(as.numeric(y[[1]][1]),as.numeric(y[[1]][2]),as.numeric(e[[1]][3]))
ba$LAT[78] <- abs(dec[1])+dec[2]/60+dec[3]/3600
# ba$LAT[90]
y <- gsub(u2[1],'T',ba$lat[90])
y <- gsub(u2[2],'T',y)
y <- gsub(u2[3],'T',y)
y <- gsub(u2[4],'T',y)
y <- gsub(u2[5],'T',y)
y <- gsub(u2[6],'T',y)
y <- gsub('º','T',y)
y <- strsplit(y,'T+')
# ba$LAT[90]
dec=c(as.numeric(y[[1]][1]),as.numeric(y[[1]][2]),as.numeric(e[[1]][3]))
ba$LAT[90] <- abs(dec[1])+dec[2]/60+dec[3]/3600
# ba$LAT [92]
y <- gsub(u2[1],'T',ba$lat[92])
y <- gsub(u2[2],'T',y)
y <- gsub(u2[3],'T',y)
y <- gsub(u2[4],'T',y)
y <- gsub(u2[5],'T',y)
y <- gsub(u2[6],'T',y)
y <- gsub('º','T',y)
y <- strsplit(y,'T+')
# ba$LAT[92]
dec=c(as.numeric(y[[1]][1]),as.numeric(y[[1]][2]),as.numeric(e[[1]][3]))
ba$LAT[92] <- abs(dec[1])+dec[2]/60+dec[3]/3600
# ba$LAT[151]
y <- gsub(u2[1],'T',ba$lat[151])
y <- gsub(u2[2],'T',y)
y <- gsub(u2[3],'T',y)
y <- gsub(u2[4],'T',y)
y <- gsub(u2[5],'T',y)
y <- gsub(u2[6],'T',y)
y <- gsub('º','T',y)
y <- strsplit(y,'T+')
# ba$LAT[151]
dec=c(as.numeric(y[[1]][1]),as.numeric(y[[1]][2]),as.numeric(e[[1]][3]))
ba$LAT[151] <- abs(dec[1])+dec[2]/60+dec[3]/3600
# ba$LAT[291]
y <- gsub(u2[1],'T',ba$lat[291])
y <- gsub(u2[2],'T',y)
y <- gsub(u2[3],'T',y)
y <- gsub(u2[4],'T',y)
y <- gsub(u2[5],'T',y)
y <- gsub(u2[6],'T',y)
y <- gsub('º','T',y)
y <- strsplit(y,'T+')
# ba$LAT[291]
dec=c(as.numeric(y[[1]][1]),as.numeric(y[[1]][2]),as.numeric(e[[1]][3]))
ba$LAT[291] <- abs(dec[1])+dec[2]/60+dec[3]/3600
# ba$LAT[348]
a <- c(substr(ba$lat[348],1,2),substr(ba$lat[348],4,5),substr(ba$lat[348],6,9))
# ba$LAT[348]
dec=c(as.numeric(a[[1]]),as.numeric(a[[2]]),as.numeric(a[[3]]))
ba$LAT[348] <- abs(dec[1])+dec[2]/60+dec[3]/3600
# ba$LAT[360]
y <- gsub(u2[1],'T',ba$lat[260])
y <- gsub(u2[2],'T',y)
y <- strsplit(y,'T+')
# ba$LAT[291]
dec=c(as.numeric(y[[1]][1]),as.numeric(y[[1]][2]),as.numeric(e[[1]][3]))
ba$LAT[360] <- abs(dec[1])+dec[2]/60+dec[3]/3600
ba$LAT # removi todos NAs de ba$LAT
# ba$LONG[90,282] refazer a partir do 'e' ou 'ee' 
y <- gsub(u2[1],'T',ba$long[90])
y <- gsub(u2[2],'T',y)
y <- gsub(u2[3],'T',y)
y <- gsub('º','T',y)
y <- strsplit(y,'T+')
# ba$LONG[90]
dec=c(as.numeric(y[[1]][1]),as.numeric(y[[1]][2]),as.numeric(e[[1]][3]))
ba$LONG[90] <- abs(dec[1])+dec[2]/60+dec[3]/3600
# ba$LONG[282]
y <- gsub(u2[1],'T',ba$long[282])
y <- gsub(u2[2],'T',y)
y <- gsub('º','T',y)
y <- strsplit(y,'T+')
# ba$LONG[282]
dec=c(as.numeric(y[[1]][1]),as.numeric(y[[1]][2]),as.numeric(e[[1]][3]))
ba$LONG[282] <- abs(dec[1])+dec[2]/60+dec[3]/3600
ba$LAT;ba$LONG # #conferindo se removi todos NAs

# colocar os valores na coluna certa
ba$lat <- ba$LAT;ba$long <- ba$LONG
ba$lat;ba$long

# mudar o sinal
str(ba)
ba$lat <- (ba$lat)*(-1);ba$long <- (ba$long)*(-1)
ba$lat;ba$long

# adicionar coluna para dar cor as propriedades
str(ba)
ba$CLASSIFICAÇÃO <- as.factor(ba$CLASSIFICAÇÃO)
str(ba)
levels(ba$CLASSIFICAÇÃO) # "733" "748" "749" "788"
ba$cor <- NA
str(ba)
ba[which(ba$CLASSIFICAÇÃO=="733"),'cor']=1
ba[which(ba$CLASSIFICAÇÃO=="748"),'cor']=2
ba[which(ba$CLASSIFICAÇÃO=="749"),'cor']=3
ba[which(ba$CLASSIFICAÇÃO=="788"),'cor']=4


###### plotar mapa e pontos pra ver se algum caiu fora do AM
#install.packages('ggmap')
library(ggmap)
#am$cor <- as.integer(am$cor)
#str(am)
# mapa
map_ba <- 'State of Bahia'
ba_map <- qmap(map_ba,zoom=7)
ba_map+geom_point(aes(x = long, y = lat , color = cor), 
                  data = ba)


###################### CE ######################
ce <- read.csv('CE2.csv',header=TRUE,sep=';')
str(ce)
levels(ce$LATITUDE_S..DECIMAL.)
names(ce)[c(13,16)] <- c('lat','long')
names(ce)

# removendo linhas sem coordenada
str(ce)
levels(ce$lat)
ce <- ce[which(ce$lat != ""),]

ce$lat <- as.character(ce$lat); ce$long <- as.character(ce$long)
str(ce)
ce$lat <- sub(',','.',ce$lat);ce$long <- sub(',','.',ce$long)
str(ce)

#install.packages("stringr", dependencies=TRUE)
require(stringr)
ce$lat  <- str_trim(ce$lat);ce$long  <- str_trim(ce$long) # retirando espacos em branco antes e depois
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

# colocando os convertidos no lugar certo: ce$lat e ce$long [c(3,21:50,52:64,66)] 
ce$lat[c(3,21:50,52:64,66)]  <- f
ce$long[c(3,21:50,52:64,66)] <- ff
ce$lat;ce$long
is.numeric(ce$lat);is.numeric(ce$long)
ce$lat <- as.numeric(ce$lat);ce$long <- as.numeric(ce$long)

# adicionar coluna para dar cor as propriedades
str(ce)
ce$X.Classificação <- as.factor(ce$X.Classificação)
levels(ce$X.Classificação) # "732" "733" "737" "742" "748" "749"
ce$cor <- NA
str(ce)
ce[which(ce$X.Classificação=="732"),'cor']=1
ce[which(ce$X.Classificação=="733"),'cor']=2
ce[which(ce$X.Classificação=="737"),'cor']=3
ce[which(ce$X.Classificação=="742"),'cor']=4
ce[which(ce$X.Classificação=="748"),'cor']=5
ce[which(ce$X.Classificação=="749"),'cor']=6
str(ce)

ce$lat;ce$long


###### plotar mapa e pontos pra ver se algum caiu fora do AM
#install.packages('ggmap')
library(ggmap)
map_ce <- 'State of Ceará'
ce_map <- qmap(map_ce,zoom=7)
w <- ce_map+geom_point(aes(x = long, y = lat , color = cor), 
                  data = ce)
ggplot_build(w) # pra descobrir a linha removida: ponto ce$lat[15]
ce$lat[15]
ce$lat;ce$long
ce$lat[15] <- ce$lat[15]/100000 #faltava um ponto, resolvendo a divisao deu certo


###################### DF ######################
df <- read.csv('DF.csv',header=TRUE,sep=';')
str(df)
df$UF_ESTABEL <- as.character(df$UF_ESTABEL)
df[which(df$UF_ESTABEL=='DISTRITO FEDERAL'),'UF_ESTABEL']='DF'

names(df)[c(11,12)] <- c('lat','long')
names(df)

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

q1 <- df$lat;q11 <- df$long # verificando as coordenadas
a1 <- grep('[^[:alnum:]]',q1) # vejo as linhas que tem numero e ponto
a11 <- grep('[^[:alnum:]]',q11) # vejo as linhas que tem numero e ponto
length(q1);length(a1);length(q11);length(a11) # comparo o tam do original com o que eu vi que tem ponto e numero

is.numeric(df$lat);is.numeric(df$long)
df$lat <- df$lat*(-1);df$long <- df$long*(-1)
df$lat;df$long














####### Ver que simbolos tem 
q1 <- df$lat;q11 <- df$long
head(q1);head(q11)
q1;q11
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
e <- gsub(u2[1],'T',ba$lat);ee <- gsub(u22[1],'T',ba$long)
e <- gsub(u2[2],'T',e);ee <- gsub(u22[2],'T',ee)
e <- gsub(u2[3],'T',e);ee <- gsub(u22[3],'T',ee)
e <- gsub(u2[4],'T',e);ee <- gsub(u22[4],'T',ee)
e <- gsub(u2[5],'T',e);ee <- gsub(u22[5],'T',ee)
e <- gsub(u2[6],'T',e);ee <- gsub(u22[6],'T',ee)

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+')
ee <- strsplit(ee,'T+')





###################### Para tranformar em excel depois ######################
library(xlsx)
write.xlsx(am, "/media/mariana/MRQ - HD externo/DOUTORADO/projeto_aves/banco_dout/am.xlsx")
getwd()
head(am)
am2 <- gsub(NA,'00',am)
head(am2)



################################ Sistemas de Coords ################################################
# O sistema de coordenada (DATUM)  em Grau, do Google Earth e da maioria dos mapas na internet é a WGS84

