###################### PA ######################
# OK
pa0 <- read.csv('PA.csv',header=T,sep=';') 
str(pa0) # 306 rows
pa <- pa0[which(pa0$LATITUDE_S..DECIMAL.!=''),] # eliminando rows sem coords
str(pa) # 303 prop com coords

names(pa)[c(12,13)] <- c('lat','long')
str(pa)

require(stringr)
pa$lat  <- str_trim(pa$lat);pa$long  <- str_trim(pa$long) # retirando espacos em branco antes e depois
head(pa)

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
pa_map+geom_point(aes(x = long, y = lat , color = 'red'), 
                       data = pa)

### transformar p/.csv e depois .xls 
write.csv2(pa,file='PA_mrq.csv',sep=';')


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

### transformar p/.csv e depois .xls 
write.csv2(pb,file='PB_mrq_ArqGIS.csv',sep=';')


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

### transformar p/.csv e depois .xls 
write.csv2(pe,file='PE_mrq_ArqGIS.csv',sep=';')


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

### transformar p/.csv e depois .xls 
write.csv2(pi,file='PI_mrq_ArqGIS.csv',sep=';')


# funcao pra converter em coordenada decimal
convert<-function(coord){
  for (i in 1:length(coord)){
    dec=c(as.numeric(coord[[i]][1]),as.numeric(coord[[i]][2]),as.numeric(coord[[i]][3]))
    coord[i]<-abs(dec[1])+dec[2]/60+dec[3]/3600  
  }
  return((as.numeric(coord)))
}
