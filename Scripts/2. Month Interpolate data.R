### R.3.6.3		data: 		27/07/2023

# Desenvolvido por: Dr. Joaquim Queirozn (UFPA)
# Adaptação: Sindy Almeida (PGMET/INPE)
#contato e dúvidas: sialmeteoro@gmail.com


## ---------- Livrarias ---------- ##

library(rgdal)
library(raster)
library(gstat)
library(fields)
#install.packages("tmap")
#library(tmap)  			# não roda no R.3.6.3.

## ---------- Livraria rspatial  ---------- ##

#if (!require("rspatial")) remotes::install_github('rspatial/rspatial')
#1

## ---------- Arquivos R ----------  ##

library(rspatial)

### ========== Abrir arquivos ========== ###
## 1. Altitudes (arquivos da krigagem)
bel_alt <- read.table("alt_ko.txt", head=T)
colnames(bel_alt) <- c("x","y","alt")
bel_alt[1:3,]
dim(bel_alt)	# 1140	3
str(bel_alt)
summary(bel_alt$alt)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  2.872  13.598  17.702  19.120  23.466  52.769

## 2. Precipitações
precm <- read.table("prec_mensal.txt",header=T)
precm[1:3,]
dim(precm)	#1140	14
head(precm)
tail(precm)
class(precm)	# data.frame
summary(precm$jan)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  265.1   334.8   343.2   344.6   355.9   373.1 

## ---------- Construção dos mapas amostrais ----------  ##
 
## 1. Precipitações mensais (precm)
precm[1:3,] # obs. x e y trocados
dfbel <- SpatialPoints(precm[,2:1],proj4string=CRS("+proj=longlat +datum=NAD83"))
dfbel
dfbel <- SpatialPointsDataFrame(dfbel, precm)
dfbel
class(dfbel)
# área de localização dos pontos
area <-readOGR('.','Regiao_Metropolitana_de_Belem')
area
plot(area,axes = TRUE)

coordinates(precm)<- ~x + y
TB <- CRS("+proj=longlat +datum=NAD83")
cata <-spTransform(area, TB)
plot(cata,axes = TRUE)
class(cata)
#[1] "SpatialPolygonsDataFrame"
#attr(,"package")
#[1] "sp"

class(dfbel)
#[1] "SpatialPointsDataFrame"
summary(dfbel)

## Mapas de precipitações
###====== FIGURAS 5.1 a 5.12 ===== ###
#  1. JANEIRO
summary(dfbel$jan)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  265.1   334.8   343.2   344.6   355.9   373.1 
# define groups for mapping
#cuts <- c(250, 275, 300, 325, 375)
cuts <- c(265, 334, 343, 355, 375)
# set up a palette of interpolated colors
blues <- colorRampPalette(c('yellow', 'orange', 'blue', 'dark blue'))
pols <- list("sp.polygons", cata, fill = "lightgray")
spplot(dfbel,axes=T, c("jan"),cuts=cuts,sp.layout=pols,
       key.space=list(x=0.75,y=0.20,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.75)
title(" JANEIRO \n ")
length(dfbel$jan)	# 1140

##  USAR ESTE 
#  1. JANEIRO
summary(dfbel$jan)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  265.1   334.8   343.2   344.6   355.9   373.1 
# define groups for mapping
#cuts <- c(250, 275, 300, 325, 375)
cuts <- c(265, 334, 343, 355, 375)
# set up a palette of interpolated colors
blues <- colorRampPalette(c('blue','yellow', 'orange','red'))
pols <- list("sp.polygons", cata, fill = "lightgray")
spplot(dfbel,axes=T,col.regions=blues(100), c("jan"),cuts=cuts,sp.layout=pols,
       key.space=list(x=0.75,y=0.20,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.75)
title(" JANEIRO \n ")
length(dfbel$jan)	# 1140

#  2. FEVEREIRO
summary(dfbel$fev)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  316.9   383.7   393.2   392.7   401.6   430.8 
# define groups for mapping
cuts <- c(316, 383, 393, 401, 435)
# set up a palette of interpolated colors
blues <- colorRampPalette(c('blue','yellow', 'orange','red'))
pols <- list("sp.polygons", cata, fill = "lightgray")
spplot(dfbel,axes=T,col.regions=blues(100), c("fev"),cuts=cuts,sp.layout=pols,
       key.space=list(x=0.75,y=0.20,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.75)
title(" FEVEREIRO \n ")

#  3. MARÇO
summary(dfbel$mar)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  403.3   435.9   443.5   443.6   450.3   498.1
# define groups for mapping
cuts <- c(403, 435, 443, 450, 498);cuts
# set up a palette of interpolated colors
pols <- list("sp.polygons", cata, fill = "lightgray")
spplot(dfbel,axes=T,col.regions=blues(100), c("mar"),cuts=cuts,sp.layout=pols,
       key.space=list(x=0.75,y=0.20,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.75)
title(" MARÇO \n ")

#  4. ABRIL
summary(dfbel$abr)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  388.5   404.1   416.4   424.9   444.8   491.5 
# define groups for mapping
cuts <- c(388, 404, 416, 444, 492);cuts
# set up a palette of interpolated colors
pols <- list("sp.polygons", cata, fill = "lightgray")
spplot(dfbel,axes=T,col.regions=blues(100), c("abr"),cuts=cuts,sp.layout=pols,
       key.space=list(x=0.75,y=0.20,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.75)
title(" ABRIL \n ")

#  5. MAIO
summary(dfbel$mai)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  290.8   325.8   332.4   333.8   345.6   359.7 
# define groups for mapping
cuts <- c(290, 325, 332, 345, 360);cuts
# set up a palette of interpolated colors
pols <- list("sp.polygons", cata, fill = "lightgray")
spplot(dfbel,axes=T,col.regions=blues(100), c("mai"),cuts=cuts,sp.layout=pols,
       key.space=list(x=0.75,y=0.20,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.75)
title(" MAIO \n ")

#  6. JUNHO
summary(dfbel$jun)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  166.7   172.1   175.6   177.8   184.1   251.9 
# define groups for mapping
cuts <- c(166, 172, 175, 184, 252);cuts
# set up a palette of interpolated colors
pols <- list("sp.polygons", cata, fill = "lightgray")
spplot(dfbel,axes=T,col.regions=blues(100), c("jun"),cuts=cuts,sp.layout=pols,
       key.space=list(x=0.75,y=0.20,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.75)
title(" JUNHO \n ")

#  7. JULHO
summary(dfbel$jul)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  150.2   165.4   168.3   169.4   173.6   182.6
# define groups for mapping
cuts <- c(150, 165, 168, 173, 183);cuts
# set up a palette of interpolated colors
pols <- list("sp.polygons", cata, fill = "lightgray")
spplot(dfbel,axes=T,col.regions=blues(100), c("jul"),cuts=cuts,sp.layout=pols,
       key.space=list(x=0.75,y=0.20,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.75)
title(" JULHO \n ")

#  8. AGOSTO
summary(dfbel$ago)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  115.3   135.3   137.3   137.1   139.8   147.9 
# define groups for mapping
cuts <- c(115, 135, 137, 139, 148);cuts
# set up a palette of interpolated colors
pols <- list("sp.polygons", cata, fill = "lightgray")
spplot(dfbel,axes=T,col.regions=blues(100), c("ago"),cuts=cuts,sp.layout=pols,
       key.space=list(x=0.75,y=0.20,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.75)
title(" AGOSTO \n ")

#  9. SETEMBRO
summary(dfbel$set)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  56.29   87.99   95.90   97.10  104.02  128.19 
# define groups for mapping
cuts <- c(55, 88, 95, 104, 130);cuts
# set up a palette of interpolated colors
pols <- list("sp.polygons", cata, fill = "lightgray")
spplot(dfbel,axes=T,col.regions=blues(100), c("set"),cuts=cuts,sp.layout=pols,
       key.space=list(x=0.75,y=0.20,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.75)
title(" SETEMBRO \n ")

#  10. OUTUBRO
summary(dfbel$out)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  43.28   81.21   89.46   88.64   95.18  105.51 
# define groups for mapping
cuts <- c(43, 81, 90, 95, 106);cuts
# set up a palette of interpolated colors
pols <- list("sp.polygons", cata, fill = "lightgray")
spplot(dfbel,axes=T,col.regions=blues(100), c("out"),cuts=cuts,sp.layout=pols,
       key.space=list(x=0.75,y=0.20,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.75)
title(" OUTUBRO \n ")

#  11. NOVEMBRO
summary(dfbel$nov)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  56.07   78.02   86.38   87.14   96.01  110.23
# define groups for mapping
cuts <- c(56, 78, 86, 96, 111);cuts
# set up a palette of interpolated colors
pols <- list("sp.polygons", cata, fill = "lightgray")
spplot(dfbel,axes=T,col.regions=blues(100), c("nov"),cuts=cuts,sp.layout=pols,
       key.space=list(x=0.75,y=0.20,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.75)
title(" NOVEMBRO \n ")

#  12. DEZEMBRO
summary(dfbel$dez)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   181.1   227.3   231.8   232.1   238.3   250.2
# define groups for mapping
cuts <- c(180, 227, 231, 238, 251);cuts
# set up a palette of interpolated colors
pols <- list("sp.polygons", cata, fill = "lightgray")
spplot(dfbel,axes=T,col.regions=blues(100), c("dez"),cuts=cuts,sp.layout=pols,
       key.space=list(x=0.75,y=0.20,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.75)
title(" DEZEMBRO \n ")

## 2. Altitudes
bel_alt[1:3,]
dim(bel_alt)	# 1140	3
dfalt <- SpatialPoints(bel_alt[,1:2],proj4string=CRS("+proj=longlat +datum=NAD83"))
dfalt <- SpatialPointsDataFrame(dfalt, bel_alt)
dfalt
class(dfalt)
#[1] "SpatialPointsDataFrame"
length(dfalt)	# 1140	

coordinates(bel_alt)<- ~x + y

### FIGURA 7.1
summary(dfalt$alt)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   2.872  13.598  17.702  19.120  23.466  52.769
cuts <- c(0, 14, 18, 25, 53)
# set up a palette of interpolated colors
blues <- colorRampPalette(c('blue','green','yellow', 'orange','red'))
blues <- colorRampPalette(c('blue','yellow', 'orange','red'))
pols <- list("sp.polygons", cata, fill = "lightgray")
spplot(dfalt,axes=T, c("alt"), col.regions=blues(100),main=" ",
       cuts=cuts,sp.layout=pols,key.space=list(x=0.75,y=0.20,corner=c(0,1),cex=0.8), 
       scales=list(draw=T),cex = 0.75)
title(" ALTITUDES (m) \n (Krigagem)")

## ---------- Interpolações ----------- ##
#install.packages("dismo")
library(dismo)

### 1. altitudes
dta  <- spTransform(dfalt, TB)
class(dta)
#[1] "SpatialPointsDataFrame"

### 2. precipitações
dtp  <- spTransform(dfbel, TB)
class(dtp)
#[1] "SpatialPointsDataFrame"

va <- voronoi(dta) # altitude
class(va)

vp <- voronoi(dtp) # precipitação
class(vp)

##===== Definir as projeções do arquivo de bordas =====##
ca <- aggregate(cata) # altitude
class(ca)

## 1. Altitudes
vca <- intersect(va, ca)
spplot(vca, 'alt', col.regions=rev(get_col_regions()))

#cp <- aggregate(cata) # precipitação
## 2. Precipitações
vcp <- intersect(vp, ca)

spplot(vcp, 'jan', col.regions=rev(get_col_regions()))
spplot(vcp, 'fev', col.regions=rev(get_col_regions()))
spplot(vcp, 'mar', col.regions=rev(get_col_regions()))
#....................................................#
spplot(vcp, 'dez', col.regions=rev(get_col_regions()))

## ---------- Rasterização (equivalente à Interpolação NN) ---------- ##
# Altitudes
ra <- raster(cata, res=0.005);ra
vra <- rasterize(vca, ra, 'alt')
dim(vra)	#101	97	1
names(vra)
summary(vra)
plot(vra)
plot(area,add=T,axes = TRUE)

### Figura 7.2 (Apenas para visualização. Não entra no relatorio)
blues <- colorRampPalette(c('blue','green','yellow', 'orange','red'))

blues <- colorRampPalette(c('blue','yellow', 'orange','red'))
plot(vra, legend=FALSE, axes=T, main="Altitudes (m)",col=blues(25))
plot(area,add=T)
plot(vra, legend.only=TRUE,smallplot=c(0.68,0.7, 0.2,0.4),
     legend.width=1, legend.shrink=0.75,col=blues(25),
     axis.args=list(cex.axis=0.7))

# Precipitações
rp <- raster(cata, res=0.005);rp
# 1. JANEIRO
vrpjan <- rasterize(vcp, rp, 'jan')
dim(vrpjan)	#101	97	1
summary(vrpjan)
plot(vrpjan)
plot(area,add=T,axes = TRUE)
vrpjan

## Figura 6.1. a 6.12 (Apenas para visualização. Não entra no relatorio)
plot(vrpjan, legend=FALSE, axes=T, main="Janeiro (mm)",col=blues(25))
plot(area,add=T)
plot(vrpjan, legend.only=TRUE,smallplot=c(0.68,0.7, 0.2,0.4),
     legend.width=1, legend.shrink=0.75,col=blues(25),
     axis.args=list(cex.axis=0.7))

# 2. FEVEREIRO
vrpfev <- rasterize(vcp, rp, 'fev')
summary(vrpfev)
plot(vrpfev)
plot(area,add=T,axes = TRUE)

plot(vrpfev, legend=FALSE, axes=T, main="Fevereiro (mm)",col=blues(25))
plot(area,add=T)
plot(vrpfev, legend.only=TRUE,smallplot=c(0.68,0.7, 0.2,0.4),
     legend.width=1, legend.shrink=0.75,col=blues(25),
     axis.args=list(cex.axis=0.7))

# 3. MARÇO
vrpmar <- rasterize(vcp, rp, 'mar')
summary(vrpmar)
plot(vrpmar)
plot(area,add=T,axes = TRUE)

plot(vrpmar, legend=FALSE, axes=T, main="Março (mm)",col=blues(25))
plot(area,add=T)
plot(vrpmar, legend.only=TRUE,smallplot=c(0.68,0.7, 0.2,0.4),
     legend.width=1, legend.shrink=0.75,col=blues(25),
     axis.args=list(cex.axis=0.7))

# 4. ABRIL
vrpabr <- rasterize(vcp, rp, 'abr')
summary(vrpabr)
plot(vrpabr)
plot(area,add=T,axes = TRUE)

plot(vrpabr, legend=FALSE, axes=T, main="Abril (mm)",col=blues(25))
plot(area,add=T)
plot(vrpabr, legend.only=TRUE,smallplot=c(0.68,0.7, 0.2,0.4),
     legend.width=1, legend.shrink=0.75,col=blues(25),
     axis.args=list(cex.axis=0.7))

# 5. MAIO
vrpmai <- rasterize(vcp, rp, 'mai')
summary(vrpmai)
plot(vrpmai)
plot(area,add=T,axes = TRUE)

plot(vrpmai, legend=FALSE, axes=T, main="Maio (mm)",col=blues(25))
plot(area,add=T)
plot(vrpmai, legend.only=TRUE,smallplot=c(0.68,0.7, 0.2,0.4),
     legend.width=1, legend.shrink=0.75,col=blues(25),
     axis.args=list(cex.axis=0.7))

# 6. JUNHO
vrpjun <- rasterize(vcp, rp, 'jun')
summary(vrpjun)
plot(vrpjun)
plot(area,add=T,axes = TRUE)

plot(vrpjun, legend=FALSE, axes=T, main="Junho (mm)",col=blues(25))
plot(area,add=T)
plot(vrpjun, legend.only=TRUE,smallplot=c(0.68,0.7, 0.2,0.4),
     legend.width=1, legend.shrink=0.75,col=blues(25),
     axis.args=list(cex.axis=0.7))

# 7. JULHO
vrpjul <- rasterize(vcp, rp, 'jul')
summary(vrpjul)
plot(vrpjul)
plot(area,add=T,axes = TRUE)

plot(vrpjul, legend=FALSE, axes=T, main="Julho (mm)",col=blues(25))
plot(area,add=T)
plot(vrpjul, legend.only=TRUE,smallplot=c(0.68,0.7, 0.2,0.4),
     legend.width=1, legend.shrink=0.75,col=blues(25),
     axis.args=list(cex.axis=0.7))

# 8. AGOSTO
vrpago <- rasterize(vcp, rp, 'ago')
summary(vrpago)
plot(vrpago)
plot(area,add=T,axes = TRUE)

plot(vrpago, legend=FALSE, axes=T, main="Agosto (mm)",col=blues(25))
plot(area,add=T)
plot(vrpago, legend.only=TRUE,smallplot=c(0.68,0.7, 0.2,0.4),
     legend.width=1, legend.shrink=0.75,col=blues(25),
     axis.args=list(cex.axis=0.7))

# 9. SETEMBRO
vrpset <- rasterize(vcp, rp, 'set')
summary(vrpset)
plot(vrpset)
plot(area,add=T,axes = TRUE)

plot(vrpset, legend=FALSE, axes=T, main="Setembro (mm)",col=blues(25))
plot(area,add=T)
plot(vrpset, legend.only=TRUE,smallplot=c(0.68,0.7, 0.2,0.4),
     legend.width=1, legend.shrink=0.75,col=blues(25),
     axis.args=list(cex.axis=0.7))

# 10. OUTUBRO
vrpout <- rasterize(vcp, rp, 'out')
summary(vrpout)
plot(vrpout)
plot(area,add=T,axes = TRUE)

plot(vrpout, legend=FALSE, axes=T, main="Outubro (mm)",col=blues(25))
plot(area,add=T)
plot(vrpout, legend.only=TRUE,smallplot=c(0.68,0.7, 0.2,0.4),
     legend.width=1, legend.shrink=0.75,col=blues(25),
     axis.args=list(cex.axis=0.7))

# 11. NOVEMBRO
vrpnov <- rasterize(vcp, rp, 'nov')
summary(vrpnov)
plot(vrpnov)
plot(area,add=T,axes = TRUE)

plot(vrpnov, legend=FALSE, axes=T, main="Novembro (mm)",col=blues(25))
plot(area,add=T)
plot(vrpnov, legend.only=TRUE,smallplot=c(0.68,0.7, 0.2,0.4),
     legend.width=1, legend.shrink=0.75,col=blues(25),
     axis.args=list(cex.axis=0.7))

# 12. DEZEMBRO
vrpdez <- rasterize(vcp, rp, 'dez')
summary(vrpdez)
plot(vrpdez)
plot(area,add=T,axes = TRUE)

plot(vrpdez, legend=FALSE, axes=T, main="Dezembro (mm)",col=blues(25))
plot(area,add=T)
plot(vrpdez, legend.only=TRUE,smallplot=c(0.68,0.7, 0.2,0.4),
     legend.width=1, legend.shrink=0.75,col=blues(25),
     axis.args=list(cex.axis=0.7))

###===== Tranformação para shape file =====###
library(raster)
library(sp)

# 1. altitudes
names(vra)
summary(vra)
#              layer
#Min.       2.871811
#1st Qu.   11.486996
#Median    16.609987
#3rd Qu.   21.379435
#Max.      52.769357
#NA's    3983.000000

plot(vra)
plot(area,add=T,axes = TRUE)
length(vra)
# 9797  101*97 

### 1. Altitude
polys_a = rasterToPolygons(vra)
names(polys_a)
#[1] "layer"

cols = rev(terrain.colors(255))

spplot(polys_a, "layer", col.regions=blues(25), lwd=0)
plot(area,add=T,axes = TRUE)

raster::shapefile(polys_a, "alt_shp.shp")

### 2. Precipitação
# 1. JANEIRO
summary(vrpjan)
plot(vrpjan)
plot(area,add=T,axes = TRUE)

polys_jan = rasterToPolygons(vrpjan)
names(polys_jan)
#[1] "layer"

raster::shapefile(polys_jan, "jan_shp.shp")

# 2. FEVEREIRO
summary(vrpfev)
plot(vrpfev)
plot(area,add=T,axes = TRUE)

polys_fev = rasterToPolygons(vrpfev)
names(polys_fev)
#[1] "layer"
summary(polys_fev)

raster::shapefile(polys_fev, "fev_shp.shp")

# 3. MARÇO
summary(vrpmar)
plot(vrpmar)
plot(area,add=T,axes = TRUE)

polys_mar = rasterToPolygons(vrpmar)

raster::shapefile(polys_mar, "mar_shp.shp")

# 4. ABRIL
summary(vrpabr)
plot(vrpabr)
plot(area,add=T,axes = TRUE)

polys_abr = rasterToPolygons(vrpabr)

raster::shapefile(polys_abr, "abr_shp.shp")

# 5. MAIO
summary(vrpmai)
plot(vrpmai)
plot(area,add=T,axes = TRUE)

polys_mai = rasterToPolygons(vrpmai)

raster::shapefile(polys_mai, "mai_shp.shp")

# 6. JUNHO
summary(vrpjun)
plot(vrpjun)
plot(area,add=T,axes = TRUE)

polys_jun = rasterToPolygons(vrpjun)

raster::shapefile(polys_jun, "jun_shp.shp")

# 7. JULHO
summary(vrpjul)
plot(vrpjul)
plot(area,add=T,axes = TRUE)

polys_jul = rasterToPolygons(vrpjul)

raster::shapefile(polys_jul, "jul_shp.shp")

# 8. AGOSTO
summary(vrpago)
plot(vrpago)
plot(area,add=T,axes = TRUE)

polys_ago = rasterToPolygons(vrpago)

raster::shapefile(polys_ago, "ago_shp.shp")

# 9. SETEMBRO
summary(vrpset)
plot(vrpset)
plot(area,add=T,axes = TRUE)

polys_set = rasterToPolygons(vrpset)

raster::shapefile(polys_set, "set_shp.shp")

# 10. OUTUBRO
summary(vrpout)
plot(vrpout)
plot(area,add=T,axes = TRUE)

polys_out = rasterToPolygons(vrpout)

raster::shapefile(polys_out, "out_shp.shp")

# 11. NOVEMBRO
summary(vrpnov)
plot(vrpnov)
plot(area,add=T,axes = TRUE)

polys_nov = rasterToPolygons(vrpnov)

raster::shapefile(polys_nov, "nov_shp.shp")

# 12. DEZEMBRO
summary(vrpdez)
plot(vrpdez)
plot(area,add=T,axes = TRUE)

polys_dez = rasterToPolygons(vrpdez)

raster::shapefile(polys_dez, "dez_shp.shp")

