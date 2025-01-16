### R.4.2.1.	data: 		27/07/2023
# Desenvolvido por: Dr. Joaquim Queirozn (UFPA)
# Adaptação: Sindy Almeida (PGMET/INPE)   
#contato e dúvidas: sialmeteoro@gmail.com

install.packages("rgdal")

## Risk maps
library(maps)
library(GISTools) 
library(maptools)
#install.packages("rlist")
library(rlist)
library(sf)
#install.packages("tmap")
library(tmap)

setwd("D:/PIBIC/Pibic2022/ANALISES")

dir()

#map_mun <- st_read("Regiao_Metropolitana_de_Belem.shp") #reading polygon layer map 1
map_mun <- vect("D:/sindy_pibic/PIBIC/MDE/resultados/RegiaoMetropolitana3.shp") 
#shapefile <- vect("D:/sindy_pibic/PIBIC/scripts/Testes/ANALISES/SHP_Modificiado/shp_jq_modificado2023.shp") 
names(map_mun)
#[1] "NM_MUNICIP" "CD_GEOCMU"  "geometry"  

map_bairro <- st_read("shp_jq_modificado2023.shp") #reading polygon layer map 1
names(map_bairro)

map_alt <- st_read("alt_shp.shp") #reading polygon layer map 1
names(map_alt)

# Criar paleta de cores
#Mypal <- c("blue","green","yellow","orange","red")

####=====  FIGURA 7.2 ===== ####
Mypal <- c("blue","yellow","orange","red")

legend_title = expression("Altitudes (m)")
win.graph(width=5.5, height=5)
Map_alt = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "layer", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.85,0.15)) # change background colour
Map_alt + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Altitudes RMB ",   #/n Inundação
		main.title.position = "center")

####=====   FIGURAS 6.1 a 6.12 =====####
# Mes de JANEIRO
map1_jan <- st_read("jan_shp.shp") #reading polygon layer map 1
names(map1_jan)
summary(map1_jan$layer)

# Inseriri dados de janeiro em map_alt
map_alt$jan <- map1_jan$layer
names(map_alt)

# Criar paleta de cores

Mypal <- c("blue","green","yellow","orange","red")

legend_title = expression("Precipitação (mm)")
win.graph(width=5.5, height=5)
map_jan = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "jan", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.82,0.15)) # change background colour
map_jan + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Precipitação JANEIRO ",   #/n Inundação
		main.title.position = "center")

# 2. Mes de FEVEREIRO
map_fev <- st_read("fev_shp.shp") #reading polygon layer map 1
names(map_fev)
summary(map_fev)

map_alt$fev <- map_fev$layer
names(map_alt)
summary(map_alt$jan)
summary(map_alt$fev)

legend_title = expression("Precipitação (mm)")
win.graph(width=5.5, height=5)
map_fev = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "fev", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.82,0.15)) # change background colour
map_fev + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Precipitação FEVEREIRO ",   #/n Inundação
		main.title.position = "center")

# 3. Mes de MARÇO
map_mar <- st_read("mar_shp.shp") #reading polygon layer map 1
names(map_mar)

map_alt$mar <- map_mar$layer
names(map_alt)

legend_title = expression("Precipitação (mm)")
win.graph(width=5.5, height=5)
map_mar = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "mar", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.82,0.15)) # change background colour
map_mar + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Precipitação MARÇO ",   #/n Inundação
		main.title.position = "center")

# 4. Mes de ABRIL
map_abr <- st_read("abr_shp.shp") #reading polygon layer map 1
names(map_abr)

map_alt$abr <- map_abr$layer
names(map_alt)

legend_title = expression("Precipitação (mm)")
win.graph(width=5.5, height=5)
map_abr = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "abr", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.82,0.15)) # change background colour
map_abr + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Precipitação ABRIL ",   #/n Inundação
		main.title.position = "center")

# 5. Mes de MAIO
map_mai <- st_read("mai_shp.shp") #reading polygon layer map 1
names(map_abr)

map_alt$mai <- map_mai$layer
names(map_alt)

legend_title = expression("Precipitação (mm)")
win.graph(width=5.5, height=5)
map_mai = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "mai", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.82,0.15)) # change background colour
map_mai + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Precipitação MAIO ",   #/n Inundação
		main.title.position = "center")

# 6. Mes de JUNHO
map_jun <- st_read("jun_shp.shp") #reading polygon layer map 1
names(map_jun)

map_alt$jun <- map_jun$layer
names(map_alt)

legend_title = expression("Precipitação (mm)")
win.graph(width=5.5, height=5)
map_jun = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "jun", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.82,0.15)) # change background colour
map_jun + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Precipitação JUNHO ",   #/n Inundação
		main.title.position = "center")

# 7. Mes de JULHO
map_jul <- st_read("jul_shp.shp") #reading polygon layer map 1
names(map_jul)

map_alt$jul <- map_jul$layer
names(map_alt)

legend_title = expression("Precipitação(mm)")
win.graph(width=5.5, height=5)
map_jul = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "jul", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.82,0.15)) # change background colour
map_jul + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Precipitação JULHO ",   #/n Inundação
		main.title.position = "center")

# 8. Mes de AGOSTO
map_ago <- st_read("ago_shp.shp") #reading polygon layer map 1
names(map_ago)

map_alt$ago <- map_ago$layer
names(map_ago)

legend_title = expression("Precipitação(mm)")
win.graph(width=5.5, height=5)
map_ago = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "ago", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.82,0.15)) # change background colour
map_ago + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Precipitação AGOSTO ",   #/n Inundação
		main.title.position = "center")

# 9. Mes de SETEMBRO
map_set <- st_read("set_shp.shp") #reading polygon layer map 1
names(map_set)

map_alt$set <- map_set$layer
names(map_set)

legend_title = expression("Precipitação(mm)")
win.graph(width=5.5, height=5)
map_set = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "set", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.82,0.15)) # change background colour
map_set + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Precipitação SETEMBRO ",   #/n Inundação
		main.title.position = "center")

# 10. Mes de OUTUBRO
map_out <- st_read("out_shp.shp") #reading polygon layer map 1
names(map_out)

map_alt$out <- map_out$layer
names(map_out)

legend_title = expression("Precipitação(mm)")
win.graph(width=5.5, height=5)
map_out = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "out", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.82,0.15)) # change background colour
map_out + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Precipitação OUTUBRO ",   #/n Inundação
		main.title.position = "center")

# 11. Mes de NOVEMBRO
map_nov <- st_read("nov_shp.shp") #reading polygon layer map 1
names(map_nov)

map_alt$nov <- map_nov$layer
names(map_nov)

legend_title = expression("Precipitação(mm)")
win.graph(width=5.5, height=5)
map_nov = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "nov", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.82,0.15)) # change background colour
map_nov + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Precipitação NOVEMBRO ",   #/n Inundação
		main.title.position = "center")

# 12. Mes de DEZEMBRO
map_dez <- st_read("dez_shp.shp") #reading polygon layer map 1
names(map_dez)

map_alt$dez <- map_dez$layer
names(map_dez)

legend_title = expression("Precipitação(mm)")
win.graph(width=5.5, height=5)
map_dez = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "dez", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.82,0.15)) # change background colour
map_dez + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Precipitação DEZEMBRO ",   #/n Inundação
		main.title.position = "center")

names(map_alt)

# Definir as faixas de valores de alt e precipitaçãi para os mapas de risco 
meses <- cbind(map_alt$jan,map_alt$fev,map_alt$mar,map_alt$abr,
               map_alt$mai,map_alt$jun,map_alt$jul,map_alt$ago,
               map_alt$set,map_alt$out,map_alt$nov,map_alt$dez)
class(meses)
dim(meses)	# 5814*12 = 69768
summary(meses)
meses[1:3,]
mmes <- matrix(meses,ncol=1);mmes
length(mmes)	# 69768

# Todos os meses
summary(mmes)
# Min.   : 43.28  
# 1st Qu.:125.97  
# Median :197.47  
# Mean   :242.59  
# 3rd Qu.:365.36  
# Max.   :498.06

# Altitude
summary(map_alt$layer)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  2.872  11.487  16.610  17.697  21.379  52.769

## faixas de risco (ensaio usando as descritivas principais)
## alt >= 21.00 e (&) prec < 125  						   1.Baixo
## alt <  21.00 e (&) alt >= 16.00 ou (|) prec >= 125  e prec < 242, 2.Medio
## alt <  16.00 e (&) alt >= 11.00 ou (|) prec >= 242  e prec < 365, 3.Alto
## alt <  11.00 e (&) prec > 365, 						   4.Muito Alto


#### =====  FIGURAS 8.1 a 8.12 ====#####
### 1. Map risk Janeiro
map_alt$rjan <- ifelse(map_alt$layer >= 30.00 | map_alt$jan < 125, "1.Baixo",
                ifelse(map_alt$layer <  30.00 & map_alt$layer  >= 16.00 | 
                       map_alt$jan >= 125     & map_alt$jan < 242, "2.Medio",
                ifelse(map_alt$layer < 16.00  & map_alt$layer  >= 11.00 |
                       map_alt$jan >= 242     & map_alt$jan < 365, "3.Alto", "4.Muito Alto")))

tjan <- as.data.frame(table(map_alt$rjan))
tjan
tjan$Perc <- round(tjan$Freq/sum(tjan$Freq)*100, dig=2)
tjan
colnames(tjan) <- c("Risco","Jan","Perct")

names(map_alt)

legend_title = expression("Risco")
win.graph(width=5.5, height=5)
map_Map = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "rjan", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.85,0.15)) # change background colour
map_Map + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Risco de Inundação (RMB) /n JANEIRO ",
		main.title.position = "center",main.title.size=0.85)

### 2. Map risk Fevereiro
map_alt$rfev <- ifelse(map_alt$layer >= 30.00 | map_alt$fev < 125, "1.Baixo",
                ifelse(map_alt$layer <  30.00 & map_alt$layer  >= 16.00 | 
                   map_alt$fev >= 125  & map_alt$fev < 242, "2.Medio",
                ifelse(map_alt$layer < 16.00  & map_alt$layer  >= 11.00 |
                   map_alt$fev >= 242  & map_alt$fev < 365, "3.Alto", "4.Muito Alto")))

tfev <- as.data.frame(table(map_alt$rfev))
tfev$Perc <- round(tfev$Freq/sum(tfev$Freq)*100, dig=2)
colnames(tfev) <- c("Risco","Fev","Perct")
tfev

names(map_alt)

legend_title = expression("Risco")
win.graph(width=5.5, height=5)
map_Map = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "rfev", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.85,0.15)) # change background colour
map_Map + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Risco de Inundação (RMB) /n FEVEREIRO ",
		main.title.position = "center",main.title.size=0.85)

### 3. Map risk Março
map_alt$rmar <- ifelse(map_alt$layer >= 30.00 | map_alt$mar < 125, "1.Baixo",
                ifelse(map_alt$layer <  30.00 & map_alt$layer  >= 16.00 | 
                   map_alt$mar >= 125  & map_alt$mar < 242, "2.Medio",
                ifelse(map_alt$layer < 16.00  & map_alt$layer  >= 11.00 |
                   map_alt$mar >= 242  & map_alt$mar < 365, "3.Alto", "4.Muito Alto")))

tmar <- as.data.frame(table(map_alt$rmar))
tmar$Perc <- round(tmar$Freq/sum(tmar$Freq)*100, dig=2)
colnames(tmar) <- c("Risco","Mar","Perct")
tmar

table(map_alt$rmar)
#     1.Baixo      2.Medio       3.Alto 	4.Muito Alto 
#     505         2563         1563         1183
#	505         2563         1613         1133  fev
#       505       2563         2738            8  jan 

names(map_alt)

legend_title = expression("Risco")
win.graph(width=5.5, height=5)
map_Map = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "rmar", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.85,0.15)) # change background colour
map_Map + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Risco de Inundação (RMB) /n MARÇO ",
		main.title.position = "center",main.title.size=0.85)

### 4. Map risk Abril
map_alt$rabr <- ifelse(map_alt$layer >= 30.00 | map_alt$abr < 125, "1.Baixo",
                ifelse(map_alt$layer <  30.00 & map_alt$layer  >= 16.00 | 
                   map_alt$abr >= 125  & map_alt$abr < 242, "2.Medio",
                ifelse(map_alt$layer < 16.00  & map_alt$layer  >= 11.00 |
                   map_alt$abr >= 242  & map_alt$abr < 365, "3.Alto", "4.Muito Alto")))

tabr <- as.data.frame(table(map_alt$rabr))
tabr$Perc <- round(tabr$Freq/sum(tabr$Freq)*100, dig=2)
colnames(tabr) <- c("Risco","Abr","Perct")
tabr

names(map_alt)

legend_title = expression("Risco")
win.graph(width=5.5, height=5)
map_Map = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "rabr", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.85,0.15)) # change background colour
map_Map + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Risco de Inundação (RMB) /n ABRIL ",
		main.title.position = "center",main.title.size=0.85)

### 5. Map risk Maio
map_alt$rmai <- ifelse(map_alt$layer >= 30.00 | map_alt$mai < 125, "1.Baixo",
                ifelse(map_alt$layer <  30.00 & map_alt$layer  >= 16.00 | 
                   map_alt$mai >= 125  & map_alt$mai < 242, "2.Medio",
                ifelse(map_alt$layer < 16.00  & map_alt$layer  >= 11.00 |
                   map_alt$mai >= 242  & map_alt$mai < 365, "3.Alto", "4.Muito Alto")))

tmai <- as.data.frame(table(map_alt$rmai))
tmai$Perc <- round(tmai$Freq/sum(tmai$Freq)*100, dig=2)
colnames(tmai) <- c("Risco","Mai","Perct")
tmai

names(map_alt)

legend_title = expression("Risco")
win.graph(width=5.5, height=5)
map_Map = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "rmai", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.85,0.15)) # change background colour
map_Map + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Risco de Inundação (RMB) /n MAIO ",
		main.title.position = "center",main.title.size=0.85)

### 5. Map risk Junho
map_alt$rjun <- ifelse(map_alt$layer >= 30.00 | map_alt$jun < 125, "1.Baixo",
                ifelse(map_alt$layer <  30.00 & map_alt$layer  >= 16.00 | 
                   map_alt$jun >= 125  & map_alt$jun < 242, "2.Medio",
                ifelse(map_alt$layer < 16.00  & map_alt$layer  >= 11.00 |
                   map_alt$jun >= 242  & map_alt$jun < 365, "3.Alto", "4.Muito Alto")))

tjun <- as.data.frame(table(map_alt$rjun))
tjun$Perc <- round(tjun$Freq/sum(tjun$Freq)*100, dig=2)
colnames(tjun) <- c("Risco","Jun","Perct")
tjun

names(map_alt)

legend_title = expression("Risco")
win.graph(width=5.5, height=5)
map_Map = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "rjun", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.85,0.15)) # change background colour
map_Map + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Risco de Inundação (RMB) /n JUNHO ",
		main.title.position = "center",main.title.size=0.85)

### 5. Map risk Julho
map_alt$rjul <- ifelse(map_alt$layer >= 30.00 | map_alt$jul < 125, "1.Baixo",
                ifelse(map_alt$layer <  30.00 & map_alt$layer  >= 16.00 | 
                   map_alt$jul >= 125  & map_alt$jul < 242, "2.Medio",
                ifelse(map_alt$layer < 16.00  & map_alt$layer  >= 11.00 |
                   map_alt$jul >= 242  & map_alt$jul < 365, "3.Alto", "4.Muito Alto")))

tjul <- as.data.frame(table(map_alt$rjul))
tjul$Perc <- round(tjul$Freq/sum(tjul$Freq)*100, dig=2)
colnames(tjul) <- c("Risco","Jul","Perct")
tjul

names(map_alt)

legend_title = expression("Risco")
win.graph(width=5.5, height=5)
map_Map = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "rjul", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.85,0.15)) # change background colour
map_Map + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Risco de Inundação (RMB) /n JULHO ",
		main.title.position = "center",main.title.size=0.85)

### 8. Map risk Agosto
map_alt$rago <- ifelse(map_alt$layer >= 30.00 | map_alt$ago < 125, "1.Baixo",
                ifelse(map_alt$layer <  30.00 & map_alt$layer  >= 16.00 | 
                   map_alt$ago >= 125  & map_alt$ago < 242, "2.Medio",
                ifelse(map_alt$layer < 16.00  & map_alt$layer  >= 11.00 |
                   map_alt$ago >= 242  & map_alt$ago < 365, "3.Alto", "4.Muito Alto")))

tago <- as.data.frame(table(map_alt$rago))
tago$Perc <- round(tago$Freq/sum(tago$Freq)*100, dig=2)
colnames(tago) <- c("Risco","Ago","Perct")
tago

names(map_alt)

legend_title = expression("Risco")
win.graph(width=5.5, height=5)
map_Map = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "rago", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.85,0.15)) # change background colour
map_Map + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Risco de Inundação (RMB) /n AGOSTO ",
		main.title.position = "center",main.title.size=0.85)

### 9. Map risk Setembro
map_alt$rset <- ifelse(map_alt$layer >= 30.00 | map_alt$set < 125, "1.Baixo",
                ifelse(map_alt$layer <  30.00 & map_alt$layer  >= 16.00 | 
                   map_alt$set >= 125  & map_alt$set < 242, "2.Medio",
                ifelse(map_alt$layer < 16.00  & map_alt$layer  >= 11.00 |
                   map_alt$set >= 242  & map_alt$set < 365, "3.Alto", "4.Muito Alto")))

tset <- as.data.frame(table(map_alt$rset))
tset$Perc <- round(tset$Freq/sum(tset$Freq)*100, dig=2)
colnames(tset) <- c("Risco","Set","Perct")
tset

names(map_alt)

legend_title = expression("Risco")
win.graph(width=5.5, height=5)
map_Map = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "rset", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.85,0.15)) # change background colour
map_Map + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Risco de Inundação (RMB) /n SETEMBRO ",
		main.title.position = "center",main.title.size=0.85)

### 10. Map risk Outubro
map_alt$rout <- ifelse(map_alt$layer >= 30.00 | map_alt$out < 125, "1.Baixo",
                ifelse(map_alt$layer <  30.00 & map_alt$layer  >= 16.00 | 
                   map_alt$out >= 125  & map_alt$out < 242, "2.Medio",
                ifelse(map_alt$layer < 16.00  & map_alt$layer  >= 11.00 |
                   map_alt$out >= 242  & map_alt$out < 365, "3.Alto", "4.Muito Alto")))

tout <- as.data.frame(table(map_alt$rout))
tout$Perc <- round(tout$Freq/sum(tout$Freq)*100, dig=2)
colnames(tout) <- c("Risco","Out","Perct")
tout

names(map_alt)

legend_title = expression("Risco")
win.graph(width=5.5, height=5)
map_Map = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "rout", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.85,0.15)) # change background colour
map_Map + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Risco de Inundação (RMB) /n OUTUBRO ",
		main.title.position = "center",main.title.size=0.85)

### 11. Map risk Novembro
map_alt$rnov <- ifelse(map_alt$layer >= 30.00 | map_alt$nov < 125, "1.Baixo",
                ifelse(map_alt$layer <  30.00 & map_alt$layer  >= 16.00 | 
                   map_alt$nov >= 125  & map_alt$nov < 242, "2.Medio",
                ifelse(map_alt$layer < 16.00  & map_alt$layer  >= 11.00 |
                   map_alt$nov >= 242  & map_alt$nov < 365, "3.Alto", "4.Muito Alto")))

tnov <- as.data.frame(table(map_alt$rnov))
tnov$Perc <- round(tnov$Freq/sum(tnov$Freq)*100, dig=2)
colnames(tnov) <- c("Risco","Nov","Perct")
tnov

names(map_alt)

legend_title = expression("Risco")
win.graph(width=5.5, height=5)
map_Map = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "rnov", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.85,0.15)) # change background colour
map_Map + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Risco de Inundação (RMB) /n NOVEMBRO ",
		main.title.position = "center",main.title.size=0.85)

### 11. Map risk Dezembro
map_alt$rdez <- ifelse(map_alt$layer >= 30.00 | map_alt$dez < 125, "1.Baixo",
                ifelse(map_alt$layer <  30.00 & map_alt$layer  >= 16.00 | 
                   map_alt$dez >= 125  & map_alt$dez < 242, "2.Medio",
                ifelse(map_alt$layer < 16.00  & map_alt$layer  >= 11.00 |
                   map_alt$dez >= 242  & map_alt$dez < 365, "3.Alto", "4.Muito Alto")))

tdez <- as.data.frame(table(map_alt$rdez))
tdez$Perc <- round(tdez$Freq/sum(tdez$Freq)*100, dig=2)
colnames(tdez) <- c("Risco","Dez","Perct")
tdez

names(map_alt)

legend_title = expression("Risco")
win.graph(width=5.5, height=5)
map_Map = tm_shape(map_alt) + tm_borders(alpha=.001) +
          tm_graticules(alpha=.10)+ tm_grid()+
  tm_fill(col = "rdez", title = legend_title, palette = Mypal, 
          style = "cont",legend.position = 20) + # add fill
  tm_borders(col = "white", lwd = .1)  + # add borders
  tm_compass(type = "arrow", position = c("left", "top") , size = 3) + # add compass
  tm_scale_bar(breaks = c(0,5,10), text.size = 0.75, position =  c("right", "bottom")) + # add scale bar
  tm_layout(bg.color = "white", legend.position=c (0.85,0.15)) # change background colour
map_Map + tm_shape(map_mun) + # add region boundaries
  tm_borders(col = "black", lwd = 2)+ # add borders
  tm_text("NM_MUNICIP",col = "black", size=0.7)+
  tm_shape(map_bairro) + # add region boundaries
  tm_borders(col = "black", lwd = 1)+ # add borders
  tm_layout(legend.text.size = 0.80, legend.title.size = 0.98, 
      frame = FALSE,main.title = "Risco de Inundação (RMB) /n DEZEMBRO ",
		main.title.position = "center",main.title.size=0.85)

### Tabela 3. percentual de áreas inundadas na RMB
# Precisa completar as linhas das tabelas: 
# Todas devem ter o mesmo umero de linhas e colunas
tmai[4,] <-  c("4.Muito Alto", 0, 0)
tjun[4,] <-  c("4.Muito Alto", 0, 0)
tjul[3,] <-  c("3.Alto", 0, 0)
tjul[4,] <-  c("4.Muito Alto", 0, 0)
tago[3,] <-  c("3.Alto", 0, 0)
tago[4,] <-  c("4.Muito Alto", 0, 0)
tset[3,] <-  c("3.Alto", 0, 0)
tset[4,] <-  c("4.Muito Alto", 0, 0)
tout[2,] <-  c("2.Medio", 0, 0)
tout[3,] <-  c("3.Alto", 0, 0)
tout[4,] <-  c("4.Muito Alto", 0, 0)
tnov[2,] <-  c("2.Medio", 0, 0)
tnov[3,] <-  c("3.Alto", 0, 0)
tnov[4,] <-  c("4.Muito Alto", 0, 0)
tdez[4,] <-  c("4.Muito Alto", 0, 0)

tab3 <- cbind(tjan,tfev[,2:3],tmar[,2:3],tabr[,2:3],tmai[,2:3],
              tjun[,2:3],tjul[,2:3],
              tago[,2:3],tset[,2:3],tout[,2:3],
              tnov[,2:3],tdez[,2:3])
tab3

# Reorganizando
tab33 <- as.data.frame(t(tab3))
names(tab33)
### TABELA 3.
tab33

