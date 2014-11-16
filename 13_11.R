################### AC ###################  
# OK
ac0 <- read.csv('AC.csv', header=TRUE, sep=';') # 8282 propriedades
head(ac0)
names(ac0)[c(11,12)] <- c('lat','long')
ac <- ac0[which(ac0$lat!=''),] # todas as linhas tem coordenadas
str(ac) # 8282 com coord
names(ac)
ac$lat <- as.character(ac$lat); ac$long <- as.character(ac$long)
str(ac)
ac$lat <- sub(',','.',ac$lat); ac$long <- sub(',','.',ac$long)
str(ac)
ac$lat <- as.numeric(ac$lat); ac$long <- as.numeric(ac$long)
str(ac)

grep('º',ac$lat,value=TRUE) # vendo se esta em decimal


###### plotar mapa e pontos pra ver se algum caiu fora do AM
#install.packages('ggmap')
library(ggmap)
str(ac)
# mapa
map_ac <- 'State of Acre'
ac_map <- qmap(map_ac,zoom=6)
ac_map+geom_point(aes(x = long, y = lat, color = 'red' ), 
                  data = ac)

######tranformar em excel depois 
# preciso transformar
#install.packages('xlsx')
#install.packages('rJava')
#library(xlsx)
str(ac)
#write.xlsx(ac, "path")

### transformar p/.csv e depois .xls 
write.csv2(ac,file='AC_mrq.csv',sep=';')


###################### AM ######################
# OK
am0 <- read.csv('AM.csv',header=TRUE,sep=';') # 64 propriedades
names(am0)[c(11,12)] <- c('lat','long')
str(am0)

am <- am0[which(am0$lat!=''),] # removendo linhas sem coordenadas
str(am) # 57 com coord

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
    for (i in 1:length(coord)){
    dec=c(as.numeric(coord[[i]][1]),as.numeric(coord[[i]][2]),as.numeric(coord[[i]][3]))
    coord[i]<-abs(dec[1])+dec[2]/60+dec[3]/3600  
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
am$LAT;am$LONG

# colocar os valores na coluna certa
am$lat <- am$LAT;am$long <- am$LONG
am$lat;am$long

# mudar o sinal
str(am)
am$lat <- (am$lat)*(-1);am$long <- (am$long)*(-1)
am$lat;am$long

###### plotar mapa e pontos pra ver se algum caiu fora do AM
#install.packages('ggmap')
library(ggmap)
str(am)
# mapa
map_am <- 'Amazonas'
am_map <- qmap(map_am,zoom=5)
am_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                  data = am)


###################### BA ######################
# OK 
ba0 <- read.csv('BA.csv',header=TRUE,sep=';') # 509 propriedades
names(ba0)[c(8,9)] <- c('lat','long')

# removendo linhas sem coordenada
str(ba0)
levels(ba0$lat)
ba <- ba0[which(ba0$lat!="Não informado"),]
ba <- ba[which(ba$lat!=""),]
str(ba) # 422 com coord
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
# mapa
map_ba <- 'State of Bahia'
ba_map <- qmap(map_ba,zoom=6)
ba_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                  data = ba)


###################### CE ######################
# OK

ce0 <- read.csv('CE2.csv',header=TRUE,sep=';') # 79 propriedades
str(ce0)
names(ce0)[c(13,16)] <- c('lat','long')

# removendo linhas sem coordenada
ce <- ce0[which(ce0$lat != ""),]
str(ce) # 66 prop c/coord
levels(ce$lat)


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


###################### ES ######################
# quase ok
# retirar rows com ptos no mar pelo ArqGIS

es0 <- read.csv('ES.csv',header=T,sep=';') # 456 propriedades
es <- es0[which(es0$LATITUDE_S..DECIMAL.!=""),]
str(es0)
names(es)[c(8,9)] <- c('lat','long')
str(es) # 456 com coord

es$lat <- as.character(es$lat);es$long <- as.character(es$long)
str(es)
es$lat <- sub(',','.',es$lat);es$long <- sub(',','.',es$long)
str(es)
require(stringr)
es$lat  <- str_trim(es$lat);es$long  <- str_trim(es$long) # retirando espacos em branco antes e depois
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


###################### PA ######################
# OK
pa0 <- read.csv('PA.csv',header=T,sep=';') 
pa <- pa0[which(pa0$LATITUDE_S..DECIMAL.!=''),] # eliminando rows sem coords
str(pa)

names(pa)[c(12,13)] <- c('lat','long')
str(pa)

require(stringr)
pa$lat  <- str_trim(pa$lat);pa$long  <- str_trim(pa$long) # retirando espacos em branco antes e depois
head(pa)

pa$lat <- as.character(pa$lat);pa$long <- as.character(pa$long)
str(pa)

pa$lat <- sub(',','.',pa$lat);pa$long <- sub(',','.',pa$long)
str(pa)

####### Ver que simbolos tem 
q1 <- pa$lat;q11 <- pa$long
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
e <- gsub(u2[1],'T',pa$lat);ee <- gsub(u22[1],'T',pa$long)
e <- gsub(u2[2],'T',e);ee <- gsub(u22[2],'T',ee)
e <- gsub(u2[3],'T',e);ee <- gsub(u22[3],'T',ee)
e <- gsub(u2[4],'T',e);ee <- gsub(u22[4],'T',ee)
e <- gsub(u2[5],'T',e);ee <- gsub(u22[5],'T',ee)
e;ee

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+');ee <- strsplit(ee,'T+')

# antes pra ver se todas deram certo nomeia por 'LAT' ou 'LONG'
pa$LAT <- convert(e);pa$LONG <- convert(ee) 
is.numeric(pa$LAT);is.numeric(pa$LONG)
pa$LAT;pa$LONG

pa[!complete.cases(pa$LAT),] #  9 rows com NAs - pa[c(192,206:212,214),c('LAT','LONG')]
f <- pa[c(192,206:212,214),c('lat','long')]
####### Ver que simbolos tem 
q1 <- f$lat;q11 <- f$long
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

u2 <- u1[-3];u22 <- u11[-3]

# tenho que tirar um de cada vez
e <- gsub(u2[1],'T',f$lat);ee <- gsub(u22[1],'T',f$long)
e <- gsub(u2[2],'T',e);ee <- gsub(u22[2],'T',ee)
e <- gsub(u2[3],'T',e);ee <- gsub(u22[3],'T',ee)
ee <- gsub(u22[4],'T',ee)
e <- gsub('º','T',e);ee <- gsub('º','T',ee)
e;ee

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+');ee <- strsplit(ee,'T+')

f$lat <- convert(e);f$long <- convert(ee)
f
pa[c(192,206:212,214),'LAT'] <- f$lat;pa[c(192,206:212,214),'LONG'] <- f$long
pa[c(192,206:212,214),c('LAT','LONG')]
pa$LAT;pa$LONG
pa$lat <- pa$LAT*(-1);pa$long <- pa$LONG*(-1)
pa$lat;pa$long

###### plotar mapa e pontos pra ver se algum caiu fora 
#install.packages('ggmap')
library(ggmap)
map_pa <- 'State of Pará'
pa_map <- qmap(map_pa,zoom=5)
w <- pa_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = pa)



###################### PB ######################
# quase ok, precisa tirar uns ptos com ArqGIS
pb0 <- read.csv('PB.csv',header=T,sep=';') 
str(pb0) # 603 rows
pb <- pb0[which(pb0$LATITUDE.S!=''),] # eliminando rows sem coords
str(pb) # 570 prop com coords

names(pb)[c(8,9)] <- c('lat','long')
str(pb)

require(stringr)
pb$lat  <- str_trim(pb$lat);pb$long  <- str_trim(pb$long) # retirando espacos em branco antes e depois
head(pb)

pb$lat <- as.character(pb$lat);pb$long <- as.character(pb$long)
str(pb)

pb$lat <- sub(',','.',pb$lat);pb$long <- sub(',','.',pb$long)
str(pb)

####### Ver que simbolos tem 
q1 <- pb$lat;q11 <- pb$long
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
e <- gsub(u2[1],'T',pb$lat);ee <- gsub(u22[1],'T',pb$long)
e <- gsub(u2[2],'T',e);ee <- gsub(u22[2],'T',ee)
e <- gsub(u2[3],'T',e);ee <- gsub(u22[3],'T',ee)
e <- gsub(u2[4],'T',e);ee <- gsub(u22[4],'T',ee)
e <- gsub(u2[5],'T',e);ee <- gsub(u22[5],'T',ee)
e <- gsub(u2[6],'T',e);ee <- gsub(u22[6],'T',ee)
e <- gsub(u2[7],'T',e);ee <- gsub(u22[7],'T',ee)
e <- gsub('º','T',e);ee <- gsub('º','T',ee)
e;ee

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+');ee <- strsplit(ee,'T+')

# antes pra ver se todas deram certo nomeia por 'LAT' ou 'LONG'
pb$LAT <- convert(e);pb$LONG <- convert(ee) 
is.numeric(pb$LAT);is.numeric(pb$LONG)
pb$LAT;pb$LONG 

# trocar sinal e por no lugar certo
pb$lat <- pb$LAT*(-1);pb$long <- pb$LONG*(-1)
pb$lat;pb$long

###### plotar mapa e pontos pra ver se algum caiu fora
#install.packages('ggmap')
library(ggmap)
map_pb <- 'Paraiba'
pb_map <- qmap(map_pb,zoom=7)
w <- pb_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = pb)

ggplot_build(w) 
pb[483,c('lat','long')] # coord errada, tirar
str(pb)
pb <- pb[-483,]
str(pb) # 569 prop com coords
# precisa tirar uns ptos com ArqGIS


###################### PE ######################
# quase ok, falta eliminar ptos fora pelo ArqGIS
pe0 <- read.csv('PE.csv',header=T,sep=';') 
str(pe0) # 1360 rows
pe <- pe0[which(pe0$Latitude!=''),] # eliminando rows sem coords
str(pe) # 1254 prop com coords

names(pe)[c(11,12)] <- c('lat','long')
str(pe)

require(stringr)
pe$lat  <- str_trim(pe$lat);pe$long  <- str_trim(pe$long) # retirando espacos em branco antes e depois
head(pe)

pe$lat <- as.character(pe$lat);pe$long <- as.character(pe$long)
str(pe)

pe$lat <- sub(',','.',pe$lat);pe$long <- sub(',','.',pe$long)
str(pe)

####### Ver que simbolos tem 
q1 <- pe$lat;q11 <- pe$long
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
e <- gsub(u2[1],'T',pe$lat);ee <- gsub(u22[1],'T',pe$long)
e <- gsub(u2[2],'T',e)
e <- gsub(u2[3],'T',e)
e;ee

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+');ee <- strsplit(ee,'T+')

# antes pra ver se todas deram certo nomeia por 'LAT' ou 'LONG'
pe$LAT <- convert(e);pe$LONG <- convert(ee) 
is.numeric(pe$LAT);is.numeric(pe$LONG)
pe$LAT;pe$LONG 

pe[is.na(pe$LAT),c('LAT','LONG')] # pe[c(165:170,423:433,435,474,808,1047:1048.1051,1054,1059,1063:1076),c('LAT','LONG')]
pe[is.na(pe$LONG),] # [474]
f <- pe[c('165','166','167','168','169','170','423','424','425','426','427','428','429','430','431','433','435','474',
     '808','1047','1048','1051','1054','1059','1063','1064','1065','1066','1067','1068','1069','1070','1071','1072','1073','1074','1075','1076'),c('lat','long')] # rows com NAs
####### Ver que simbolos tem 
q1 <- f$lat;q11 <- f$long
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
e <- gsub(u2[1],'T',f$lat);ee <- gsub(u22[1],'T',f$long)
e <- gsub('º','T',e);ee <- gsub('º','T',ee)
e;ee

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+');ee <- strsplit(ee,'T+')

# antes pra ver se todas deram certo nomeia por 'LAT' ou 'LONG'
f$lat <- convert(e);f$long <- convert(ee)
f

# dps de arrumado, colocar com os outros transformados
pe[c('165','166','167','168','169','170','423','424','425','426','427','428','429','430','431','433','435','474',
     '808','1047','1048','1051','1054','1059','1063','1064','1065','1066','1067','1068','1069','1070','1071','1072','1073','1074','1075','1076'),'LAT'] <- f$lat
pe[c('165','166','167','168','169','170','423','424','425','426','427','428','429','430','431','433','435','474',
     '808','1047','1048','1051','1054','1059','1063','1064','1065','1066','1067','1068','1069','1070','1071','1072','1073','1074','1075','1076'),'LONG'] <- f$long
pe$LAT;pe$LONG

# trocar sinal e por no lugar certo
pe$lat <- pe$LAT*(-1);pe$long <- pe$LONG*(-1)
pe$lat;pe$long

###### plotar mapa e pontos pra ver se algum caiu fora
#install.packages('ggmap')
library(ggmap)
map_pe <- 'State of Pernambuco'
pe_map <- qmap(map_pe,zoom=6)
w <- pe_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = pe)

ww <- ggplot_build(w) # 18 missing rows
ww$data[[4]][!complete.cases(ww$data[[4]][,c(2,3)]),c(2,3)]
# pe[c(24,52,250,270,342,510,654,683,686,702,706,738,788,842,931,932,1156,1240),c('lat','long')]
m <- pe[c(24,52,250,270,342,510,654,683,686,702,706,738,788,842,931,932,1156,1240),c('lat','long')]
m[which(m$lat!=0),]
24,52,250,[270,342,]510,[654,]683,[686,702,706,]738,788,842,931,932,[1156,1240] # rows dps de eliminar vazias
32,61,276,[296,378,]566,[717,]746,[750,766,770,]802,855,909,998,999,[1245,1346] # rows originais
# coords erradas - pe[c(270,342,654,686,702,706,1156,1240,738,842)]
# pe[24] - Para; pe[52] - Antarctica
# pe[683] - mar

# pe[250,c('lat','long')] - arrumar a casa de long
pe[250,c('lat','long')]
pe$long[250] <- pe$long[250]/10

# pe[510,c('lat','long')] - arrumar a casa de long
pe[510,c('lat','long')]
pe$long[510] <- pe$long[510]/10

# pe[788,c('lat','long')] - arrumar a casa lat
pe[788,c('lat','long')]
pe$lat[788] <- pe$lat[788]/10

# pe[931,c('lat','long')] - arrumar casa long
pe[931,c('lat','long')]
pe$long[931] <- pe$long[931]/10

# pe[932,c('lat','long')] - arrumar casa long
pe[932,c('lat','long')]
pe$long[932] <- pe$long[932]/10

# eliminar coords erradas
pe <- pe[-c(270,342,654,686,702,706,1156,1240,738,842,24,52,683),]
str(pe) # 1241 prop com coords
# eliminar ptos fora pelo ArqGIS


###################### PI ######################
# quase ok, falta eliminar pontos fora no ArqGIS
pi0 <- read.csv('PI.csv',header=T,sep=';') 
str(pi0) # 114 rows
pi <- pi0[which(pi0$LATITUDE_S..DECIMAL.!=''),] # eliminando rows sem coords
str(pe) # 114 prop com coords

names(pi)[c(9,10)] <- c('lat','long')
str(pi)

require(stringr)
pi$lat  <- str_trim(pi$lat);pi$long  <- str_trim(pi$long) # retirando espacos em branco antes e depois
str(pi)

pi$lat <- sub(',','.',pi$lat);pi$long <- sub(',','.',pi$long)
str(pi)

####### Ver que simbolos tem 
q1 <- pi$lat;q11 <- pi$long
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
e <- gsub(u2[1],'T',pi$lat);ee <- gsub(u22[1],'T',pi$long)
e <- gsub(u2[2],'T',e);ee <- gsub(u22[2],'T',ee)
e <- gsub(u2[3],'T',e);ee <- gsub(u22[3],'T',ee)
e <- gsub(u2[4],'T',e);ee <- gsub(u22[4],'T',ee)
e <- gsub(u2[5],'T',e)
e <- gsub(u2[6],'T',e)
e <- gsub('º','T',e);ee <- gsub('º','T',ee)
e;ee

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+');ee <- strsplit(ee,'T+')

# antes pra ver se todas deram certo nomeia por 'LAT' ou 'LONG'
pi$LAT <- convert(e);pi$LONG <- convert(ee) 
is.numeric(pi$LAT);is.numeric(pi$LONG)
pi$LAT;pi$LONG 

pi[!complete.cases(pi[,c('LAT','LONG')]),] # NAs pi$LAT[95] pi$LONG[c(15,25.76)]
f <- pi[c(15,25,76,95),c('lat','long')]
####### Ver que simbolos tem 
q1 <- f$lat;q11 <- f$long
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
e <- gsub(u2[1],'T',f$lat);ee <- gsub(u22[1],'T',f$long)
e <- gsub(u2[2],'T',e);ee <- gsub(u22[2],'T',ee)
e <- gsub(u2[3],'T',e)
e <- gsub('º','T',e);ee <- gsub('º','T',ee)
e;ee

# separando pelo simbolo que coloquei
e <- strsplit(e,'T+');ee <- strsplit(ee,'T+')

# arrumar NAs pi$LAT[95] pi$LONG[c(15,25.76)]
e # lat
e[[4]][3] <-  sub(e[[4]][3],'20.15',e[[4]][3])
ee #long
ee[[1]][3] <- sub(ee[[1]][3],'10.9',ee[[1]][3])
ee[[2]][3] <- sub(ee[[2]][3],'2.7',ee[[2]][3])
ee[[3]][3] <- sub(ee[[3]][3],'25.57',ee[[3]][3])
e;ee
f$lat <- convert(e);f$long <- convert(ee)
f

# colocar no lugar certo
pi$LAT[c(15,25,76,95)] <- f$lat
pi$LONG[c(15,25,76,95)] <- f$long
pi$LAT;pi$LONG

# mudar o sinal e colocar na coluna final
pi$lat <- pi$LAT*(-1);pi$long <- pi$LONG*(-1)
pi$lat;pi$long

###### plotar mapa e pontos pra ver se algum caiu fora
#install.packages('ggmap')
library(ggmap)
map_pi <- 'Piauí'
pi_map <- qmap(map_pi,zoom=6)
w <- pi_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = pi)

ggplot_build(w) 

# 1 mising row pi$lat[74]
pi[74,c('lat','long')] # mar, tirar

pi <- pi[-74,]
str(pi) # 113 prop c/coords
# eliminar pontos fora no ArqGIS






################################ Sistemas de Coords ################################################
# O sistema de coordenada (DATUM)  em Grau, do Google Earth e da maioria dos mapas na internet é a WGS84

################################ Geotrans ################################################
# executavel no windowns pra transformar coordenadas