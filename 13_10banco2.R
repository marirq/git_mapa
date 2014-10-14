br_cad <- read.csv('cad_ufs_zuado.csv',header=TRUE,sep=';')

br_cad2 <- read.csv('cad_ufs.csv',header=TRUE,sep=';')

names(br_cad)
levels(br_cad$UF_ESTABEL)
br_cad[which(br_cad$UF_ESTABEL == ''),] 


br_cad$UF_ESTABEL <- as.character(br_cad$UF_ESTABEL)
str(br_cad$UF_ESTABEL)
br_cad[,'UF_ESTABEL'] <- sub('DISTRITO FEDERAL','DF',br_cad[,'UF_ESTABEL'])
br_cad[,'UF_ESTABEL'] <- sub('Pará','PA',br_cad[,'UF_ESTABEL'])
br_cad$UF_ESTABEL <- as.factor(br_cad$UF_ESTABEL)
str(br_cad$UF_ESTABEL)
levels(br_cad$UF_ESTABEL)

names(br_cad)[c(11,12)] <- c('lat','long') # renomendo as cols de coords
str(br_cad$UF_ESTABEL)
# gsub('[[:punct:]]','º','M@RI 2nA') # pra tirar um metacharacter
br_cad$lat <- as.character(br_cad$lat); br_cad$long <- as.character(br_cad$long)
str(br_cad)
# br_cad[which(br_cad$lat=='.º'),]
linhas <- grep('.º',br_cad$lat)
length(linhas)
mud <- br_cad[linhas,]
head(mud)
tail(mud)
str(mud)
mud$UF_ESTABEL <- as.factor(mud$UF_ESTABEL)
levels(mud$UF_ESTABEL)
