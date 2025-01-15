# Mapeamento-de-Risco-de-inundação - RMB

Este repositório contém scripts e datasets desenvolvidos no âmbito do projeto "Desenvolvimento de modelos quantitativos e computacionais para mapeamento de vulnerabilidade e riscos de inundação" do Programa Institucional de Bolsas de Iniciação Científica (PIBIC) realizado entre 2022 e 2023, sendo projetados para análise de dados geoespaciais e modelagem de risco na Região Metropolitana de Belém (RMB).

# Requisitos
Ambiente de Desenvolvimento
R: Versão 3.6.3 a 4.1.3.
Rtools: Necessário para compilar pacotes a partir do código-fonte. Adicione o Rtools ao PATH do sistema para evitar erros de instalação.

Pacotes principais:
rgdal, geoR, gstat, plyr, sf, raster, tmap, fields, dismo.
Substituições sugeridas:
Pacotes legados: rgdal, maptools, e GISTools.
Pacotes modernos: sf, terra, e stars.

# Descrição dos scripts
Este repositório inclui:

# 1. Month Data.R

# Finalidade
Este script é o ponto de partida para a análise. Ele processa os dados brutos de precipitação para gerar um dataset base com valores mensais e anuais, estruturados para análises espaciais e temporais.
Foco em organizar e sumarizar dados, criando tabelas que alimentam os próximos scripts.

# Peculiaridades
Entrada: Um arquivo contendo dados de precipitação histórica por coordenada (latitude e longitude) e mês (belem.txt; Belem_CHIRPS_SSEBop.shp; Regiao_Metropolitana_de_Belem.shp).
Realiza:
Leitura de dados espaciais e climáticos.
Organização dos dados em séries temporais mensais.
Geração de estatísticas descritivas para análise preliminar.

# A saída do script (dados processados e organizados em formato tabular) é usada diretamente pelo script 2. Month Interpolate Data.R para interpolação espacial.

# Compatível com R 3.6.3.
Pacotes utilizados:
rgdal: Manipulação de shapefiles, mas pode ser subsituído pelo sf
geoR: Análise geoestatística.
gstat: Interpolação.
plyr: Manipulação e agregação de dados.

# 2. Month Interpolate Data.R

# Finalidade
Este script realiza a interpolação espacial dos dados mensais de precipitação para gerar superfícies contínuas (rasters) que representam a distribuição espacial da chuva.
É uma etapa intermediária que transforma os dados pontuais do primeiro script em mapas interpolados.

# Peculiaridades
Entrada: O dataset gerado pelo script anterior e um shapefile representando os limites da RMB (Regiao_Metropolitana_de_Belem.shp).
Realiza:
Krigagem para interpolação de altitude.
Interpolação de precipitação utilizando o método NN (Nearest Neighbor).
Criação de rasters e visualizações mensais interpoladas.

# A saída do script (superfícies interpoladas e rasters mensais) é utilizada pelo script 3. Month Risk Map.R para classificar áreas de risco.

# Compatível com R 3.6.3.
Pacotes utilizados:
rgdal: Leitura de shapefiles.
raster: Manipulação de dados raster.
gstat: Krigagem para interpolação.
fields: Manipulação espacial.
dismo: Criação de Voronoi e rasterização.

# 3. Month Risk Map.R

# Finalidade
Este script combina as superfícies interpoladas de precipitação e altitude para criar mapas mensais de risco de inundação, categorizados em diferentes níveis de gravidade.
É a etapa final da análise, que gera os mapas visuais e estatísticas de risco.

# Peculiaridades
Entrada: Mapas interpolados de precipitação e altitude gerados pelo script anterior.
Realiza:
Combinação dos dados interpolados.
Definição de faixas de risco com base em critérios predefinidos e considerando as estatísticas descritivas das duas variáveis (ex.: precipitação > 365 mm e altitude < 11 m).
Classificação de risco (Baixo, Médio, Alto, Muito Alto).
Geração de mapas visuais para cada mês, incluindo legendas, escalas e limites administrativos.

# Depende diretamente dos resultados gerados por 2. Month Interpolate Data.R.
Gera produtos finais (mapas e estatísticas) que podem ser usados para análises, relatórios e apresentações.

# Compatível com R 4.2.1.
Pacotes utilizados:
sf: Manipulação de dados espaciais em formato moderno.
tmap: Criação de mapas temáticos.
GISTools: Visualização de dados geoespaciais.
rlist: Manipulação de listas complexas.


##############
Compatível com R 3.6 a R 4.1: rgdal, geoR, GISTools, maptools, prettymapr.
Compatível com versões mais recentes (R 4.2 ou superiores): gstat, lattice, maps.
Pacotes legados: rgdal, maptools, e GISTools estão sendo substituídos gradualmente por sf e outros pacotes modernos.

# Créditos
Dr. Joaquim Queiroz: Desenvolvimento conceitual, orientação e supervisão técnica do projeto.
Bch. Sindy Almeida: Adaptação dos scripts, processamento de dados e análises espaciais.

Para dúvidas ou colaborações, entre em contato: sialmeteoro@gmail.com


