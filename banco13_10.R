br_cad <- read.csv('cad_ufs.csv',header=TRUE,sep=';')

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

teste <- grep('^A',br_cad$UF_ESTABEL,value=TRUE)
a <- br_cad[which(grep(br_cad$UF_ESTABEL=='A^')),]
