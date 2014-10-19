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

names(am)[c(11,12)] <- c('lat_dec','long_dec')
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
read.csv





###################### Para tranformar em excel depois ######################
library(xlsx)
write.xlsx(am, "/media/mariana/MRQ - HD externo/DOUTORADO/projeto_aves/banco_dout/am.xlsx")
getwd()
head(am)
am2 <- gsub(NA,'00',am)
head(am2)
