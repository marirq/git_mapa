###################### SE ######################
# quase ok, tirar um pto fora no ArqGIS
se0 <- read.csv2('SE cadastro.csv',header=T,sep=';')
str(se0) # 972 rows
se <- se0[which(se0$Latitude!=''),] # eliminando rows sem coords
names(se)[c(17,18)] <- c('lat','long')
str(se) # 427 prop com coords

require(stringr)
se$lat  <- str_trim(se$lat);se$long  <- str_trim(se$long) # retirando espacos em branco antes e depois
head(se)

se$lat <- sub(',','.',se$lat);se$long <- sub(',','.',se$long)
str(se)

####### Ver que simbolos tem 
q1 <- se$lat;q11 <- se$long
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

# ver se ta certo em outra col
se$LAT <- se$lat;se$LONG <- se$long
se$lat;se$LONG
str(se)

# trocar sinal e colocar na col certa
se$lat <- as.numeric(se$LAT)*(-1);se$long <- as.numeric(se$LONG)*(-1)
se$lat;se$long

###### plotar mapa e pontos pra ver se algum caiu fora
#install.packages('ggmap')
library(ggmap)
map_se <- 'State of Sergipe'
se_map <- qmap(map_se,zoom=7)
w <- se_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = se)
ggplot_build(w) # 4 missing rows se[c(329,291,264,265),c('lat','long')]
se[c(329,291,264,265),c('lat','long')]
# ptos com NAs [329,264,265] e coords repetidas [678] - tirar
se <- se[-c(329,291,264,265),] # 423 prop com coords
# tirar um pto fora no ArqGIS

### transformar p/.csv e depois .xls 
write.csv2(se,file='SE_mrq_ArqGIS.csv',sep=';')


###################### SP ######################
# quase ok, falta tirar ptos com ArqGIS
sp0 <- read.csv2('SP.csv',header=T,sep=';')
str(sp0) # 4507 rows
sp <- sp0[which(sp0$LATITUDE_S..DECIMAL.!=''),] # eliminando rows sem coords
names(sp)[c(8,9)] <- c('lat','long')
str(sp) # 4197 prop com coords

require(stringr)
sp$lat  <- str_trim(sp$lat);sp$long  <- str_trim(sp$long) # retirando espacos em branco antes e depois
head(sp)

sp$lat <- sub(',','.',sp$lat);sp$long <- sub(',','.',sp$long)
str(sp)

####### Ver que simbolos tem 
q1 <- sp$lat;q11 <- sp$long
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

# ver se ta certo em outra col
sp$LAT <- sp$lat;sp$LONG <- sp$long
sp$lat;sp$LONG
str(sp)

# transformar em numeric e colocar na col certa
sp$lat <- as.numeric(sp$lat);sp$long <- as.numeric(sp$long)
str(sp)

###### plotar mapa e pontos pra ver se algum caiu fora
#install.packages('ggmap')
library(ggmap)
map_sp <- 'State of São Paulo'
sp_map <- qmap(map_sp,zoom=6)
w <- sp_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = sp)
ww <- ggplot_build(w) # 23 missing rows 
ww$data[[4]][!complete.cases(ww$data[[4]][,c(2,3)]),c(2,3)]
rownames(ww$data[[4]][!complete.cases(ww$data[[4]][,c(2,3)]),c(2,3)])
# 138,356,659,671,921,987,1047,1325,1679,1801,1819,2207,2368,2843,2937,2951,3387,3665,3719,3842,3907,4036,4194 - como numero
# "138","356","659","671","921","987","1047","1325","1679","1801","1819","2207","2368","2843","2937","2951","3387","3665","3719","3842","3907","4036","4194" em character
sp[c(138,356,659,671,921,987,1047,1325,1679,1801,1819,2207,2368,2843,2937,2951,3387,3665,3719,3842,3907,4036,4194),c('lat','long')] 
# 138,356,659,671,921,987,1047,1325,1679,1801,1819,2207,2368,2843,2937,2951,3387,3665,3719,3842,3907,4036,4194
# [c(138,659,671,921,1047,1679,2207,2843,3387,3842)] - MS
# [c(356,987,1325,1801,2368,2951,3665,4036)] - mar
# [1819] - Paraguai; [c(2937,3907,4194)] - erro

sp$long[3719] # arrumar o ponto, pq eh em sp
sp$long[3719] <- sp$long[3719]*10

# tirar ptos errados
sp <- sp[-c(138,356,659,671,921,987,1047,1325,1679,1801,1819,2207,2368,2843,2937,2951,3387,3665,3842,3907,4036,4194),]
str(sp) # 4175 prop com coords
# tirar ptos fora com ArqGIS

### transformar p/.csv e depois .xls 
write.csv2(sp,file='SP_mrq_ArqGIS.csv',sep=';')


###################### TO ######################
#
to0 <- read.csv2('TO.csv',header=T,sep=';')
str(to0) # 104 rows
to <- to0[which(to0$LATITUDE_S..DECIMAL.!=''),] # eliminando rows sem coords
str(to)

# unindo as caselas pra coords e spliting
to$lat <- paste(to$LATITUDE_S..DECIMAL.,to$X,to$X.1)
to$long <- paste(to$LONGITUDE_W..DECIMAL.,to$X.2,to$X.3)
to$lat;to$long
to$LAT <- strsplit(to$lat,' ');to$LONG <- strsplit(to$long,' ')
to$LAT;to$LONG

# funcao pra converter em coordenada decimal 
convert<-function(coord){
  for (i in 1:length(coord)){
    dec=c(as.numeric(coord[[i]][1]),as.numeric(coord[[i]][2]),as.numeric(coord[[i]][3]))
    coord[i]<-abs(dec[1])+dec[2]/60+dec[3]/3600  
  }
  return((as.numeric(coord)))
}

# converter coords
e <- convert(to$LAT);ee <- convert(to$LONG)
e;ee

# trocar sinal e colocar na col certa
to$lat <- e*(-1);to$long <- ee*(-1)
to$lat;to$long

str(to) # 104 prop com coords
to5 <- to[,-c(17,18)] # tirando as listas, fica melhor pra ver str
str(to5)

###### plotar mapa e pontos pra ver se algum caiu fora
#install.packages('ggmap')
library(ggmap)
map_to <- 'State of Tocantins'
to_map <- qmap(map_to,zoom=6)
w <- to_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = to)

ggplot_build(w) # 4 missing rows - [c(16,47,48,88)]
to[c(16,47,48,88),c('lat','long')]
# [16,48] - SP; [47] - mar; [88] - AM
# FALTA FAZER ISSO
### transformar p/.csv e depois .xls 
write.csv2(pi,file='PI_mrq_ArqGIS.csv',sep=';')
