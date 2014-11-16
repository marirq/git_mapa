###################### PR ######################
# 
pr0 <- read.csv('PR.csv',header=T,sep=';') 
str(pr0) # 22268 rows
pr <- pr0[which(pr0$Latitude!=''),] # eliminando rows sem coords
str(pr) # 22216 prop com coords

names(pr)[c(14,15)] <- c('lat','long')
str(pr)

require(stringr)
pr$lat  <- str_trim(pr$lat);pr$long  <- str_trim(pr$long) # retirando espacos em branco antes e depois
str(pr)

pr$lat <- sub(',','.',pr$lat);pr$long <- sub(',','.',pr$long) # caso tenha alguma virgula
str(pr)

####### Ver que simbolos tem 
q1 <- pr$lat;q11 <- pr$long
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
# testando tirar espacos FAZER NO LEB!!!
# tirar os espacos
e <- gsub(u1[1],'T',pr$lat[7485:7495]);ee <- gsub(u11[1],'T',pr$long[7485:7495])
e;ee
e <- 
  
# dados sem ponto, transformar pra decimal
a <- nchar(pr$lat) # vendo quantos digitos tem em cada
unique(a) # para isolar os repetidos e saber qtos caracteres tem cada

# retirar espacos e transformar em numeric
require(stringr)
pr$lat  <- str_trim(pr$lat);pr$long  <- str_trim(pr$long) # retirando espacos em branco antes e depois

str(pr)

grep('[^[:digit:]]',pr$lat,value=T)

pr$lat[7485:7495]

sub(pr$lat[1])


b <- dec.pr(as.numeric(pr$lat))
head(pr$lat)
class(pr$lat)
as.numeric(pr$lat[nchar(pr$lat)==5])/1000
as.numeric(pr$lat[5415])/100


dec.pr <- function(x){  
  if (nchar(x)==5){
  x/1000  
  }
  if (nchar(x)==6){
  x/10000
  }
  if (nchar(x)==7){
    x/100000
  }
  if (nchar(x)==8){
    x/1000000
  }
}



# funcao pra converter em coordenada decimal
convert<-function(coord){
  for (i in 1:length(coord)){
    dec=c(as.numeric(coord[[i]][1]),as.numeric(coord[[i]][2]),as.numeric(coord[[i]][3]))
    coord[i]<-abs(dec[1])+dec[2]/60+dec[3]/3600  
  }
  return((as.numeric(coord)))
}


################################ Sistemas de Coords ################################################
# O sistema de coordenada (DATUM)  em Grau, do Google Earth e da maioria dos mapas na internet é a WGS84

################################ Geotrans ################################################
# executavel no windowns pra transformar coordenadas