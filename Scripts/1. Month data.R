# Versão  R.4.1.2		27/07/2023
# Desenvolvido por: Dr. Joaquim Queirozn (UFPA)
# Adaptação: Sindy Almeida (PGMET/INPE)       

rm(list=ls())	# limpar a area de trabalho
# a alternativa pro rgal é usar o sf com st_read
install.packages("rgdal")
install.packages("geoR")
install.packages("gstat")
install.packages("lattice")
install.packages("units")
install.packages("https://cran.r-project.org/src/contrib/Archive/rgdal/rgdal_1.5-27.tar.gz", repos = NULL, type = "source")
install.packages("ggplot2")
install.packages("maptools")
install.packages("tmap")
install.packages("plyr")



library(rgdal) 	# for geographic transformations
library(units) 
library(geoR)
library(gstat)
library(lattice)
library(prettymapr)
library(maps)
library(GISTools) 
library(maptools)
library(sf)
library(ggplot2)
library(tmap)




setwd("D:/PIBIC/Pibic2022/ANALISES")
dir()
#################################################################
##########			ANALISES				###########
## 1. Precipitações na RMB de 2003 a 2019 (dados mensais)	   ##	
##    Dimensões: Lat, Long e tempo (ano, mes)
## 2. Altitudes (m)   			   ##
#################################################################

##===== Carregar arquivos de dados =====##

belem <- read.table("D:/sindy_pibic/PIBIC/scripts/Testes/ANALISES/belem.txt", header=T)
dim(belem)		# [1] 217740      6
head(belem)		# inicio: 08 de 2003
tail(belem)		# final:  06 de 2019
		
##===== Carregar o Shapes do municipio de Belem =====##

#belch <-readOGR('.','D:/sindy_pibic/PIBIC/scripts/Testes/ANALISES/Belem_CHIRPS_SSEBop')
belch <-readOGR('D:/sindy_pibic/PIBIC/scripts/Testes/ANALISES/Belem_CHIRPS_SSEBop.shp')
names(belch)
belch[1:3,]

win.graph(width=6, height=6)
plot(belch,axes=T, lwd=1,pch=19,cex=0.3, 
     xlim=c(-48.65,-48.15),
     ylim=c(-1.55,-1.0))

## 
##===== Municipios da RMB =====##
#rmb<-readOGR('.','Regiao_Metropolitana_de_Belem') #graus
rmb = readOGR('D:/sindy_pibic/PIBIC/scripts/ANALISES/Regiao_Metropolitana_de_Belem.shp')

# Criando um objeto "Spatial Polygon"
spols<-polygons(rmb)

# Criando um objeto "Dataframe"
rmbt<-data.frame(rmb)
plot(spols,add=T,axes = TRUE,lwd=2)
#title("Belem")

##===== Bairros da RMB =====##
#rmb2<-readOGR('.','RegiaoMetropolitana3') #graus
rmb2 = readOGR ('D:/sindy_pibic/PIBIC/scripts/ANALISES/RegiaoMetropolitana3.shp')

# Criando um objeto "Spatial Polygon"
spols<-polygons(rmb2)

# Criando um objeto "Dataframe"
rmbt2<-data.frame(rmb2)
plot(spols,add=T,axes = TRUE,lwd=1)
title("Região metropolitana de Belem")


###

# Incluindo label no mapa
segments(-48.45,-1.35,-48.58,-1.40, col= 'red') 
text(-48.61,-1.418,"Belém",lwd=5,
	adj = c(0, 0),col="red", srt = 0,cex=0.75)

segments(-48.28,-1.2,-48.15,-1.12, col= 'black') 
text(-48.15,-1.12,"Santa \n Bárbara",adj = c(0, 0),col="black", srt = 0,cex=0.75)

segments(-48.28,-1.3,-48.15,-1.32, col= 'black') 
text(-48.18,-1.335,"Benevides",adj = c(0, 0),col="black", srt = 0,cex=0.75)

segments(-48.32,-1.4,-48.3,-1.50, col= 'black') 
text(-48.32,-1.51,"Marituba",adj = c(0, 0),col="black", srt = 0,cex=0.75)

segments(-48.38,-1.35,-48.38,-1.53, lwd=1.2,col= 'black') 
text(-48.45,-1.535,"Ananindeua",adj = c(0, 0),col="black", srt = 0,cex=0.75)

north.arrow(xb=-48.67, yb=-1.05, len=.006, lab="N") 
maps::map.scale(cex=.7, x=-48.7, ratio=F, relwidth=0.15)  

#### Uso de tmap e sf
# Carregar os shapefiles
rmb_sf <- st_read("D:/sindy_pibic/PIBIC/scripts/ANALISES/Regiao_Metropolitana_de_Belem.shp")
rmb2_sf <- st_read("D:/sindy_pibic/PIBIC/scripts/ANALISES/RegiaoMetropolitana3.shp")


# Filtrar geometrias não vazias no shapefile rmb2_sf
rmb2_sf <- rmb2_sf[!st_is_empty(rmb2_sf), ]

# Filtrar geometrias não vazias no shapefile rmb_sf
rmb_sf <- rmb_sf[!st_is_empty(rmb_sf), ]

# Criar o mapa


tm_shape(rmb2_sf) +
  tm_polygons(border.col = "black", lwd = 1) +
  tm_shape(rmb_sf) +
  tm_polygons(border.col = "black", lwd = 2, alpha = 0) +
  tm_layout(
    title = "Região Metropolitana de Belém",
    title.position = c("center", "top"),
    legend.outside = FALSE,
    frame = TRUE,
    outer.margins = c(0.02, 0.02, 0.02, 0.02),  # Ajuste de margem
    panel.show = TRUE                            # Mostra os eixos
  ) +
  tm_graticules(lines = FALSE, labels.size = 0.8) +  # Adiciona rótulos nos eixos
  tm_compass(type = "8star", position = c("right", "top"))

#========== 1. Organização das variaveis ========== #
# Definição dos periodos: cada periodo tem 191 linhas
#número 191 refere-se ao número de meses no período agosto de 2003 a junho de 2019.
belem[1:191,] # Periodo: anos de 2003 a 2019
belem$ano <- as.factor(belem$ano)
belem$mes <- as.factor(belem$mes)

# Criando um indice (ID) para identificar os periodos acima (2003 a 2019)

nrow(belem)/191 # 1140
ID <- rep(c(1:1140),each=191)
belem$ID <- ID
belem[1:192,]

1140*191 # 217740 (nrow(belem)) número de linhas do arquivo

belem[1:2,]
xybel <- aggregate(list(ID=belem[belem$ID!=0,7]), 
          list(x=belem[belem$ID!=0,1],y=belem[belem$ID!=0,2]), 
          function(ID) unique(ID))  # ordem crescente do x
# correspondentes aos períodos agregados da tabela xybel.
xybel[1:5,]
class(xybel)
dim(xybel)  # 1140    3
win.graph(width=6, height=6)
plot(xybel$y,xybel$x, pch=19, cex=0.5) 	# 1140 pontos amostrais

# Ordenando por ano (e mes)
library(plyr)
xybel[1:4,]
xybelo <- arrange(xybel,xybel$ID)
xybelo[1:18,]

# Criar um arquivo tipo list
datalist = list()
for(i in 1:1140) {
belxy <- matrix(rep(xybelo[i,],each=17),17,2)
  datalist[[i]] = belxy
print(belxy)
}
dim(belxy) # 17 (17 anos: 2003 a 2019) 2
belxy[1:17,]

belxy = do.call(rbind, datalist)
class(belxy)
dim(belxy)	# 19380	2
belxy <- as.data.frame(belxy)
colnames(belxy) <- c("Lat","Long")
belxy[1:18,]

# Separando os pontos amostrais (1140)
belem[1:3,]
# Médias mensais
prec_m <- tapply(belem$prec,belem$mes,mean)  # prec
prec_m
#        1         2         3         4         5         6         7 
#344.56269 392.69775 443.64155 424.94556 333.81836 177.75483 169.37300 
#        8         9        10        11        12 
#137.14492  97.09709  88.63944  87.14420 232.07777 

sid <- split(belem, belem$ID) # por pontos amostrais
names(sid)	# 1140 pontos amostrais
sid[1]	# amostra 1 (ponto amostral 1): 2003 a 2019 (mensal)
sid[2]	# amostra 2 (ponto amostral 2): 2003 a 2019 (mensal)
#......
sid[1140]	# amostra 1140 (ponto amostral 1140): 2003 a 2019 (mensal)

# Gráfico da série mensal no ponto 1 (-1.093473 -48.23782)
ponto_1 <- as.data.frame(sid[1])
ponto_1[1:13,]
colnames(ponto_1) <- c("Lat","Long","ano","mes","prec","ET","ID")
head(ponto_1)
tail(ponto_1)
prec_1 <- tapply(ponto_1$prec,ponto_1$mes,mean)  # prec
prec_1

# serie temporal no ponto Ponto_1:-1.093 -48.237
prec_01 <- ts(ponto_1$prec,start=c(2003,8),end=c(2019,6),freq=12);prec_01
plot.ts(prec_01,main='Precipitacoes anuais RMB (2003 a 2019) \n (Ponto_1:-1.093 -48.237)',
  ylab='Precipitações anuais  (mm)',xlab= " ")
summary(prec_01)

# regime mensal no ponto Ponto_1:-1.093 -48.237
prec_1m <- ts(prec_1,start=c(1),end=c(12),freq=1);prec_1m
plot.ts(prec_1m,main='Regime mensal RMB (2003 a 2019) \n (Ponto_1:-1.093 -48.237)',
  ylab='Precipitações mensais (mm)',xlab= " ")

##===== Precipitações para cada ano (usar o indice ID) =====##
for(i in 1:1140) {
data_id <- sid[i]
}

for(i in 1:1140) {
data_id[i] <- sid[i]
}
data_id[i]
data_id[10]

##===== Soma das precipitações anuais em cada ponto =====##
## 1. para os anos:  2003 a 2019 ( 17 anos )
datalistp = list()
for(i in 1:1140) {
data_id[[i]]$ano <- as.factor(data_id[[i]]$ano)
pplat <- 0
idprec <- tapply(data_id[[i]]$prec,data_id[[i]]$ano,sum)  # prec
  datalistp[[i]] = idprec
}

precid_ano = do.call(cbind, datalistp)
class(precid_ano)
dim(precid_ano)	# 17 anos  1140
precid_ano[1:3,]

##===== MÉDIAS das precipitações MENSAIS em cada ponto =====##
## 1. para os meses: 12 meses

datalistpm = list()
for(i in 1:1140) {
data_id[[i]]$mes <- as.factor(data_id[[i]]$mes)
pplat <- 0
idprec <- tapply(data_id[[i]]$prec,data_id[[i]]$mes,mean)  # prec
  datalistpm[[i]] = idprec
}

precid_mes = do.call(cbind, datalistpm)
class(precid_mes)
dim(precid_mes)	# 12 meses  1140
precid_mes[1:12]
precdf <- as.data.frame(precid_mes)
precdf <- t(precdf)
dim(precdf)
precdf[1:3,]

sid[1]
sid[2]
sid[3]

#ordenando
# Ordenando por ano (e mes)
library(plyr)
xybel_o <- arrange(xybel,desc(xybel$x))
xybel_o[1:3,]
dim(xybel_o)

# Incluindo as coordenadas
prec_mensal <- cbind(xybel_o[,1:2],precdf)
prec_mensal[1:5,]
names(prec_mensal)
colnames(prec_mensal) <- c("x","y","jan","fev","mar",
                           "abr","mai","jun","jul",
                           "ago","set","out","nov","dez")
prec_mensal[1:5,]
# Tabela 1. Estatisticas de precipitações mensais
Tab1 <- rbind(summary(prec_mensal$jan),summary(prec_mensal$fev),summary(prec_mensal$mar),
              summary(prec_mensal$abr),summary(prec_mensal$mai),summary(prec_mensal$jun),
              summary(prec_mensal$jul),summary(prec_mensal$ago),summary(prec_mensal$set),
              summary(prec_mensal$out),summary(prec_mensal$nov),summary(prec_mensal$dez))
rownames(Tab1) <- c("jan","fev","mar","abr","mai","jun",
                    "jul","ago","set","out","nov","dez")
print(Tab1,dig=5)

#Min. 1st Qu.  Median    Mean 3rd Qu.   Max.
#jan 265.064 334.833 343.234 344.563 355.854 373.13
#fev 316.949 383.747 393.198 392.698 401.606 430.85
#mar 403.317 435.911 443.535 443.642 450.251 498.06
#abr 388.509 404.089 416.426 424.946 444.828 491.51
#mai 290.777 325.806 332.397 333.818 345.623 359.66
#jun 166.688 172.140 175.597 177.755 184.054 251.89
#jul 150.202 165.424 168.277 169.373 173.623 182.62
#ago 115.349 135.251 137.338 137.145 139.768 147.92
#set  56.294  87.989  95.898  97.097 104.020 128.19
#out  43.278  81.208  89.460  88.639  95.181 105.51
#nov  56.072  78.016  86.383  87.144  96.010 110.23
#dez 181.078 227.270 231.769 232.078 238.272 250.23


# regime no ponto 1
prec_pt1 <- prec_mensal[1,3:14]
class(prec_pt1)

plot(seq(1,12),prec_pt1,type="l",main='Regime mensal RMB (2003 a 2019) \n (Ponto_1:-1.093 -48.237)',
  ylab='Precipitações mensais (mm)')

dim(prec_mensal) #1140   14
# salvar para interpolação

### PRECIPITAÇÕES MENSAIS
write.table(prec_mensal, "prec_mensal.txt")

### 2. Altitudes
### dados reamostrados (80000 amostras)
bel_alt8 <- read.table("D:/sindy_pibic/PIBIC/scripts/ANALISES/rmb_alt.txt", head=T)
bel_alt8[1:3,]
altm8   <- as.data.frame(bel_alt8$x)
altm8$y <- bel_alt8$y
altm8$alt <- bel_alt8$altitude 
colnames(altm8) <- c("x","y","alt")
altm8[1:3,]
dim(altm8)	# 80000	3
head(altm8)
tail(altm8)
class(altm8)	# data.frame
altm8$alt <- as.numeric(altm8$alt)
str(altm8)
summary(altm8$alt)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   1.00   14.00   18.00   19.81   25.00   71.00 

# Mudar para 5000 amostras
salt5 <- altm8[sample(nrow(altm8), 5000), ] 
## Obs. tentar selecionar uma amostra aleatoria até conseguir essas estatiticas
## minimo =1    máximo = 71 (valores da amostra de 80000)

summary(salt5$alt)
##   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
##   1.00   14.00   18.00   19.85   25.00   62.00  

# salvar arquivo (5000 amostras)
write.table(salt5, "salt5.txt")

# Arquivo de altitudes com 5000 amostras
bel_alt <- read.table("salt5.txt", head=T)
bel_alt[1:3,]
altm   <- as.data.frame(bel_alt$x)
altm$y <- bel_alt$y
altm$alt <- bel_alt$alt 
colnames(altm) <- c("x","y","alt")
altm[1:3,]
dim(altm)	# 5000	3
head(altm)
tail(altm)
class(altm)	# data.frame
altm$alt <- as.numeric(altm$alt)
str(altm)
summary(altm$alt)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## 1.00   14.00   18.00   19.85   25.00   62.00 

### É necessário que as dados de precipitações mensais e altitudes estejam
### em pontos (lat/long) comuns (iguais), para se poder as algebras de mapas

#### Colocar os dados nas mesmas coordenadas usando krigagem
altm[1:3,]
dim(altm)	# 5000	3
library(geoR)
alt <-as.geodata(altm,coords.col = 1:2, data.col = 3)
names(alt)

var_alt <- variog(alt, uvec = seq(0, 0.22, length = 25), 
           option = "bin")
var_alt
plot(var_alt, xlab = "Distância", ylab = "Semivariância",type="l")
title("Semivariograma Experimental: Alt")

###=== Figura 3. Semivariograma experimental -> Mede a dependência espacial com base nos dados observados.
###=== Modelo  teórico ajustado -> Representa uma aproximação matemática para a estrutura de dependência espacial
#nos dados, essencial para técnicas como krigagem.

#Semivariograma unidirecional e modelo ajustado
plot(var_alt,pch=19, cex=0.8, xlab = "Distância", ylab = "Semivariância")
lines.variomodel(cov.model = "spherical" , 
       cov.pars = rbind(c(42,0.03),c(28,0.2)), 
       nugget = 10, max.dist = 0.222, lwd = 2, col = "red")
text(0.1,0.1, "MODELO: 10 + 42Sph(h/0,03) + 28Sph (h/0.2)",cex=0.75)
title("Semivariograma Experimental e \n modelo ajustado")

#Semivariograma unidirecional e modelo ajustado
plot(var_alt,pch=19, cex=0.8, xlab = "Distância", ylab = "Semivariância")
lines.variomodel(cov.model = "spherical" , 
       cov.pars = rbind(c(52,0.03),c(35,0.22)), 
       nugget = 0, max.dist = 0.222, lwd = 2, col = "red")
text(0.1,0.1, "MODELO: 20 + 33Sph(h/0,06) + 15 Sph (h/0.2)",cex=0.75)
title("Semivariograma Experimental e \n modelo ajustado")

# Coordenadas dos pontos de prec mensal
#prec_m <- read.table("prec_mensal.txt",header=T)
prec_mensal[1:5,]  # coordenadas x e y invertidas
mcoord <- cbind(prec_mensal$y,prec_mensal$x)
colnames(mcoord) <- c("x","y")
class(mcoord)
mcoord <-as.data.frame(mcoord)
mcoord[1:3,]
ko_alt <- ksline(alt, locations=mcoord, cov.model = "spherical" , 
       cov.pars = rbind(c(52,0.03),c(35,0.22)), nugget = 0)

$start 17:32  end: 17:36
names(ko_alt)
class(ko_alt)
summary(ko_alt$predict)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  4.607  14.009  17.725  19.262  23.327  49.372 

ko_alt$locations

alt_ko <- as.data.frame(cbind(ko_alt$locations,ko_alt$predict))
alt_ko[1:5,]
dim(alt_ko)	# 1140	3
# salvar resultados da krigagem
#write.table(alt_ko,"alt_ko.txt")

### Gráficos de altitudes amostrais e interpoladas por krigagem

# 1. Altitudes Amostrais
### 1.1. Arquivo de 80000 amostras
F31_alt8 <- as.data.frame(altm8)
F31_alt8[1:3,]
dim(F31_alt8)	# 5000	3

df_alt8 <- SpatialPoints(F31_alt8[,1:2],proj4string=CRS("+proj=longlat +datum=NAD83"))
#df_alt
df_alt8 <- SpatialPointsDataFrame(df_alt8, F31_alt8)
names(df_alt8)
spplot(df_alt8,axes=T,c("alt"),main="Altitudes (m) \n (80000 Amostras)",
       key.space=list(x=0.75,y=0.22,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.5)

### 2. arquivo de 5000 amostras
F31_alt5 <- as.data.frame(altm)
F31_alt5[1:3,]
dim(F31_alt5)	# 5000	3

df_alt5 <- SpatialPoints(F31_alt5[,1:2],proj4string=CRS("+proj=longlat +datum=NAD83"))
#df_alt
df_alt5 <- SpatialPointsDataFrame(df_alt5, F31_alt5)
names(df_alt5)
spplot(df_alt5,axes=T,c("alt"),main="Altitudes (m) \n Amostras",
       key.space=list(x=0.75,y=0.22,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.5)

# 2. Altitudes interpoladas por krigagem Krigagem
dir()
F32_altm <-read.table("alt_ko.txt",head=T)
class(F32_altm)	# data.frame
colnames(F32_altm) <- c("x","y","alt")
F32_altm[1:3,]
coordinates(F32_altm)<- ~x + y
class(F32_altm)	# data.frame
#[1] "SpatialPointsDataFrame"

spplot(F32_altm,axes=T,c("alt"),main="Altitudes (m) \n Krigagem",
       key.space=list(x=0.72,y=0.19,corner=c(0,1),cex=0.7), 
       scales=list(draw=T),cex = 0.75)

# Tabela 2. Estatísticas amostrais e interpoladas (krigagem)
names(F31_alt8)
names(F32_altm)

Tab2 <- as.data.frame(rbind(summary(F31_alt5$alt),summary(F32_altm$alt)))
rownames(Tab2) <- c("Amostras", "Krigagem")
print(Tab2, digits=4)

Tab2 <- as.data.frame(rbind(summary(F31_alt8$alt),summary(F32_altm$alt)))
rownames(Tab2) <- c("Amostras", "Krigagem")
print(Tab2, digits=4)


