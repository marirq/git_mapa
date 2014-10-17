################### AC ###################
ac <- read.csv('AC cadastro.csv', header=TRUE, sep=';')
head(ac)
str(ac)
names(ac)[c(11,12)] <- c('lat','long')
names(ac)

ac$lat <- as.character(ac$lat); ac$long <- as.character(ac$long)
str(ac)
ac$lat <- sub(',','.',ac$lat); ac$long <- sub(',','.',ac$long)
str(ac)
ac$lat <- as.numeric(ac$lat); ac$long <- as.numeric(ac$long)
str(ac)

grep('º',ac$lat,value=TRUE) # esta em decimal

###################### AM ######################
am <- read.csv('AM cadastro.csv',header=TRUE,sep=';')
str(am)
names(am)[c(11,12)] <- c('lat','long')
am <- am[which(am$lat!=''),] # removendo linhas sem coordenadas
am$lat <- as.character(am$lat); am$long <- as.character(am$long)
str(am)
am$lat <- sub(',','.',am$lat);am$lat <- sub(',','.',am$long)

#install.packages("stringr", dependencies=TRUE)
require(stringr)
am$lat  <- str_trim(am$lat) # retirando espacos em branco antes e depois
str(am)


####### Ver que simbolos tem 
q1 <- am$lat

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
u2 <- u1[-2] # deixando apenas o simbolo de ponto

# tenho que tirar um de cada vez
am$lat <- gsub(u2[1],'T',am$lat)  
am$lat <- gsub(u2[2],'T',am$lat)
am$lat <- gsub(u2[3],'T',am$lat)
am$lat <- gsub(u2[4],'T',am$lat)
am$lat <- gsub(u2[5],'T',am$lat)
am$lat <- gsub(u2[6],'T',am$lat)
am$lat <- strsplit(am$lat,'T')
am$lat[11:16]

# funcao pra converter em coordenada decimal
convert<-function(coord){
  ab <- coord
  for (i in 1:length(coord)){
    dec=c(as.numeric(coord[[i]][1]),as.numeric(coord[[i]][2]),as.numeric(coord[[i]][3]))
    coord[i]<-abs(dec[1])+dec[2]/60+dec[3]/3600
    ab[i]  
  }
  return((as.numeric(coord)))
}


am$lat[31:36] <- convert(am$lat[31:36])
e <- unlist(am$lat[31:36]) # crio outra coluna no meu data frame para tirar da lista
e <- (e)*(-1) # pra passar pra sinal negativo

