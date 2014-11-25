ac <- read.csv('AC_mrq.csv',header=T,sep=';')
str(ac)
ac$lat <- str_trim(ac$lat); ac$long <- str_trim(ac$long)
ac$lat <- sub(',','.',ac$lat); ac$long <- sub(',','.',ac$long)
ac$lat <- as.numeric(ac$lat); ac$long <- as.numeric(ac$long)
str(ac)
am <- read.csv('AM_mrq.csv',header=T,sep=';')
str(am)
am$lat <- str_trim(am$lat);am$long <- str_trim(am$long)
am$lat <- sub(',','.',am$lat);am$long <- sub(',','.',am$long)
am$lat <- as.numeric(am$lat);am$long <- as.numeric(am$long)
str(am)
names(ac)
names(am)
merge_dfs <- merge(ac[,c(3,2,12,13)],am[,c(3,2,12,13)],all=TRUE)

ba <- read.csv('BA_mrq.csv',head=T,sep=';')
str(ba)
ba$lat <- str_trim(ba$lat);ba$long <- str_trim(ba$long)
ba$lat <- sub(',','.',ba$lat);ba$long <- sub(',','.',ba$long)
ba$lat <- as.numeric(ba$lat);ba$long <- as.numeric(ba$long)
str(ba)
names(ba)# arrumar COD_CADASTRO
names(ba)[2] <- 'COD_CADASTRO'
merge_dfs <- merge(merge_dfs,ba[,c(3,2,9,10)],all=TRUE)

ce <- read.csv('CE_mrq.csv',header=T,sep=';')
str(ce)
ce$lat <- str_trim(ce$lat);ce$long <- str_trim(ce$long)
ce$lat <- sub(',','.',ce$lat);ce$long <- sub(',','.',ce$long)
ce$lat <- as.numeric(ce$lat);ce$long <- as.numeric(ce$long)
str(ce)
names(ce)
merge_dfs <- merge(merge_dfs,ce[,c(3,2,14,17)],all=TRUE)

df <- read.csv('DF_sem_out.csv',header=T,sep=';')
str(df)
df$lat <- str_trim(df$lat);df$long <- str_trim(df$long)
df$lat <- sub(',','.',df$lat);df$long <- sub(',','.',df$long)
df$lat <- as.numeric(df$lat);df$long <- as.numeric(df$long)
str(df)
names(df)
merge_dfs <- merge(merge_dfs,df[,c(4,3,13,14)],all=TRUE)

es <- read.csv('ES_sem_out.csv',header=T,sep=';')
str(es)
es$lat <- as.numeric(sub(',','.',es$lat));es$long <- as.numeric(sub(',','.',es$long))
str(es)
names(es)
merge_dfs <- merge(merge_dfs,es[,c(3,2,9,10)],all=TRUE)

go <- read.csv('GO_mrq.csv',header=T,sep=';')
str(go)
go$lat <- str_trim(go$lat);go$long <- str_trim(go$long)
go$lat <- sub(',','.',go$lat);go$long <- sub(',','.',go$long)
go$lat <- as.numeric(go$lat);go$long <- as.numeric(go$long)
str(go)
names(go)
names(go)[c(2,3)] <- c('COD_CADASTRO','UF_ESTABEL') # arrumar COD_CADASTRO e UF_ESTABEL
merge_dfs <- merge(merge_dfs,go[,c(3,2,10,11)],all=TRUE)

ma <- read.csv('MA_mrq.csv',header=T,sep=';')
str(ma)
ma$lat <- str_trim(ma$lat);ma$long <- str_trim(ma$long)
ma$lat <- sub(',','.',ma$lat);ma$long <- sub(',','.',ma$long)
ma$lat <- as.numeric(ma$lat);ma$long <- as.numeric(ma$long)
str(ma)
names(ma)
merge_dfs <- merge(merge_dfs,ma[,c(3,2,12,13)],all=TRUE)

mg <- read.csv('MG_sem_out.csv',header=T,sep=';')
str(mg)
mg$lat <- str_trim(mg$lat);mg$long <- str_trim(mg$long)
mg$lat <- sub(',','.',mg$lat);mg$long <- sub(',','.',mg$long)
mg$lat <- as.numeric(mg$lat);mg$long <- as.numeric(mg$long)
str(mg)
names(mg)
merge_dfs <- merge(merge_dfs,mg[,c(4,3,10,11)],all=TRUE)

ms <- read.csv("MS_sem_out.csv",header=T,sep=';')
str(ms)
ms$lat <- as.numeric(sub(',','.',ms$lat));ms$long <- as.numeric(sub(',','.',ms$long))
str(ms)
names(ms)
merge_dfs <- merge(merge_dfs,ms[,c(3,2,12,13)],all=TRUE)

mt <- read.csv('MT_sem_out.csv',header=T,sep=';')
str(mt)
mt$lat <- as.numeric(sub(',','.',mt$lat));mt$long <- as.numeric(sub(',','.',mt$long))
str(mt)
names(mt)
merge_dfs <- merge(merge_dfs,mt[,c(3,2,9,11)],all=TRUE)

pa <- read.csv('PA_mrq.csv',header=T,sep=';')
str(pa)
pa$lat <- as.numeric(sub(',','.',pa$lat));pa$long <- as.numeric(sub(',','.',pa$long))
str(pa)
names(pa)
merge_dfs <- merge(merge_dfs,pa[,c(3,2,13,14)],all=TRUE)

pb <- read.csv('PB_sem_out.csv',header=T,sep=';')
str(pb)
pb$lat <- as.numeric(sub(',','.',pb$lat));pb$long <- as.numeric(sub(',','.',pb$long))
str(pb)
names(pb)
names(pb)[c(3,4)] <- c('COD_CADASTRO','UF_ESTABEL')
names(pb)
merge_dfs <- merge(merge_dfs,pb[,c(4,3,10,11)],all=T)

pe <- read.csv('PE_sem_out.csv',header=T,sep=';')
str(pe)
pe$lat <- str_trim(pe$lat);pe$long <- str_trim(pe$long)
pe$lat <- as.numeric(sub(',','.',pe$lat));pe$long <- as.numeric(sub(',','.',pe$long))
str(pe)
names(pe)
merge_dfs <- merge(merge_dfs,pe[,c(4,3,13,14)],all=T)

pi <- read.csv('PI_sem_out.csv',header=T,sep=';')
str(pi)
pi$lat <- as.numeric(sub(',','.',pi$lat));pi$long <- as.numeric(sub(',','.',pi$long))
str(pi)
names(pi)
merge_dfs <- merge(merge_dfs,pi[,c(4,3,11,12)],all=T)

pr <- read.csv('PR_mrq.csv',header=T,sep=';')
str(pr)
pr$lat <- as.numeric(sub(',','.',pr$lat));pr$long <- as.numeric(sub(',','.',pr$long))
str(pr)
names(pr)
merge_dfs <- merge(merge_dfs,pr[,c(2,7,22,23)],all=T)

rj <- read.csv('RJ_sem_out.csv',header=T,sep=';')
str(rj)
rj$lat <- as.numeric(sub(',','.',rj$lat));rj$long <- as.numeric(sub(',','.',rj$long))
str(rj)
names(rj)
merge_dfs <- merge(merge_dfs,rj[,c(4,3,10,11)],all=T)

rn <- read.csv('RN_sem_out.csv',header=T,sep=';')
str(rn)
rn$lat <- as.numeric(sub(',','.',rn$lat));rn$long <- as.numeric(sub(',','.',rn$long))
str(rn)
names(rn)
merge_dfs <- merge(merge_dfs,rn[,c(4,3,10,11)],all=T)

ro <- read.csv('RO_mrq.csv',header=T,sep=';')
str(ro)
ro$lat <- as.numeric(sub(',','.',ro$lat));ro$long <- as.numeric(sub(',','.',ro$long))
str(ro)
names(ro)[c(2,3)] <- c('COD_CADASTRO','UF_ESTABEL')
names(ro)
merge_dfs <- merge(merge_dfs,ro[,c(3,2,11,12)],all=T)

rs <- read.csv('RS_sem_out.csv',header=T,sep=';')
str(rs)
rs$lat <- as.numeric(sub(',','.',rs$lat));rs$long <- as.numeric(sub(',','.',rs$long))
str(rs)
names(rs)
merge_dfs <- merge(merge_dfs,rs[,c(4,3,10,11)],all=T)

sc <- read.csv('SC_sem_out.csv',header=T,sep=';')
str(sc)
sc$lat <- as.numeric(sub(',','.',sc$lat));sc$long <- as.numeric(sub(',','.',sc$long))
str(sc)
names(sc)
merge_dfs <- merge(merge_dfs,sc[,c(5,3,11,12)],all=T)

se <- read.csv('SE_sem_out.csv',header=T,sep=';')
str(se)
se$lat <- as.numeric(sub(',','.',se$lat));se$long <- as.numeric(sub(',','.',se$long))
str(se)
names(se)[c(3,10,11)] <- c('=P','COD_CADASTRO','UF_ESTABEL')
names(se)
merge_dfs <- merge(merge_dfs,se[,c(11,10,19,20)],all=T)

sp <- read.csv('SP_sem_out.csv',header=T,sep=';')
str(sp)
sp$lat <- as.numeric(sub(',','.',sp$lat));sp$long <- as.numeric(sub(',','.',sp$long))
str(sp)
names(sp)
merge_dfs <- merge(merge_dfs,sp[,c(4,3,10,11)],all=T)

to <- read.csv('TO_sem_out.csv',header=T,sep=';')
str(to)
to$lat <- as.numeric(sub(',','.',to$lat));to$long <- as.numeric(sub(',','.',to$long))
str(to)
names(to)
merge_dfs <- merge(merge_dfs,to[,c(4,3,19,20)],all=T)

str(merge_dfs)
a <- merge_dfs

###### plotar mapa e pontos ######
library(ggmap)
map_br <- "Brasil"
br <- qmap(map_br,zoom=4)
w <- br+geom_point(aes(x = long, y = lat, color = 'red'),
              data = merge_dfs)
ww <- ggplot_build(w)
row.names(ww$data[[4]][!complete.cases(ww$data[[4]][,c(2,3)]),c(2,3)]) 
a[16279,]
lat.pb <- a[16279,3]/10
a[16279,3]  <- lat.pb
a[49537,]
lat.sc <- a[49537,4]
long.sc <- a[49537,3]
a[49537,3] <- lat.sc
a[49537,4] <- long.sc

map_br <- "Brasil"
br <- qmap(map_br,zoom=4)
br+geom_point(aes(x = long, y = lat, color = 'red'),
                   data = a)
write.csv2(a,'UF_coords.csv',sep=';')
