###################### SC ######################
#
sc0 <- read.csv('SC.csv',header=T,sep=';') 
str(sc0) # 9271 rows
sc <- sc0[which(sc0$LATITUDE_S!=''),] # eliminando rows sem coords
str(sc) # 9271 prop com coords

names(sc)[c(9,10)] <- c('lat','long')
str(sc)

require(stringr)
sc$lat  <- str_trim(sc$lat);sc$long  <- str_trim(sc$long) # retirando espacos em branco antes e depois
head(sc)

sc$lat <- sub(',','.',sc$lat);sc$long <- sub(',','.',sc$long)
str(sc)

####### Ver que simbolos tem 
q1 <- sc$lat;q11 <- sc$long
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

str(sc)
b <- grep('^0.',sc$lat)
sc <- sc[-b,] # tirar as linhas com valor 0
str(sc) # 9250 prop com coords

####### Ver, novamente, que simbolos tem 
q1 <- sc$lat;q11 <- sc$long
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

# pra nao mexer nos que ja estao em decimal
a <- str_length(sc$lat)
unique(a)
row.names(sc[str_length(sc$lat)==c(8,10),c('lat','long')])
scy <- sc[-c(2642,2660,4329,4796,4820,4824,4838,4844,4852,4863,5236,7637,7645,7651,8556,8567,8573),] # tirando os certos
str(scy)
scy$lat

a <- str_length(scy$lat)
unique(a)
scy[str_length(scy$lat)==8,c('lat','long')]
row.names(scy[str_length(scy$lat)==8,c('lat','long')])
scy[str_length(scy$lat)==10,c('lat','long')]
row.names(scy[str_length(scy$lat)==10,c('lat','long')])
c(2533,2659,4329,4776,4783,4796,4797,4813,4820,4824,4825,4838,4844,4859,4863,4870,5236,6262,7634,7637,7638,7645,7646,76518461,8556,8566,8567,8573,8576)
scz <- scy["[^[c('2533','2659','4329','4776','4783','4796','4797','4813','4820','4824','4825','4838','4844','4859','4863','4870',
             '5236','6262','7634','7637','7638','7645','7646','7651','8461','8556','8566','8567','8573','8576']]"),]
nrow(scy[c('2533','2659','4329','4776','4783','4796','4797','4813','4820','4824','4825','4838','4844','4859','4863','4870',
      '5236','6262','7634','7637','7638','7645','7646','7651','8461','8556','8566','8567','8573','8576'),])

yx <- scy[which(row.names(scy)=='2533'&'2659'&'4329'),c('lat','long')]


# dados sem ponto, transformar pra decimal
a <- str_length(sc$lat) # vendo quantos digitos tem em cada
unique(a) # para isolar os repetidos e saber qtos caracteres tem cada, tem NAs
