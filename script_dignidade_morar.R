library(tidyverse)
library(dplyr)

#caminho da base de dados
setwd( "F:/GitHub/dignidade_morar")

BASE<-read.csv("base_dignidade_v1.csv",header = TRUE, stringsAsFactors = FALSE)
#names(BASE)
#count (BASE)

#Subset Locacao
LOCACAO <- subset(BASE, tipo_transacao == "LOCACAO")

#Preço, condomínio e IPTU por m2
LOCACAOm2 <- mutate(LOCACAO,preco_m2 = preco_imovel_mediana/area_util, 
                    condominio_m2= preco_condominio/area_util,
                    iptu_m2= valor_iptu/area_util )
colnames(LOCACAOm2)

#Corrigir valores infinitos
LOCACAOm2$preco_m2[is.infinite(LOCACAOm2$preco_m2)] <- NA

#Verificar observações duplicadas
duplicated_rows <- duplicated(LOCACAOm2[, c('latitude', 'longitude','preco_m2','area_util', 'dormitorios','Mes','Ano')])
duplicate_counts <- table(duplicated_rows) 

#contando o número de entradas unicas (FALSE) e entradas duplicadas (TRUE)
print(duplicate_counts)

#gerando uma base de locação sem repetiçoes na base para as colnas duplicadas
LOCACAOm2_sem_duplicadas <- LOCACAOm2[!duplicated_rows,]
count(LOCACAOm2_sem_duplicadas)

#limpando preco_m2=NA e valores numéricos
LOCm2_clean <- LOCACAOm2_sem_duplicadas[!is.na(LOCACAOm2_sem_duplicadas$preco_m2), ]
LOCm2_clean$preco_m2_num<- as.numeric (LOCm2_clean$preco_m2)
count(LOCm2_clean)

#histograma percentil de preco_m2 
precom2_decile <- quantile(LOCm2_clean$preco_m2, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
print(precom2_decile)
hist(precom2_decile, breaks = precom2_decile,main ="Histograma de preço/m2", xlab = "Preço/m2")

#histograma extremos de preco_m2 
LOCm2_clean_num <-as.numeric(LOCm2_clean$preco_m2)
custom_breaks <- c(0,5,10,20,40,80,Inf)
breaks <-cut(LOCm2_clean_num, breaks=custom_breaks, right= TRUE, include.lowest= TRUE, stringsAsFactors = FALSE, na.rm = TRUE)
freq_table<-table(breaks)
barplot(freq_table, main ="Histograma de preço/m2", xlab = "Preço/m2", ylab = "Frequência", col = "blue", border = "black")

LOC_preco <- LOCm2_clean %>% group_by(preco_m2_num<5,preco_m2_num>80, na.rm = TRUE)%>%
  summarise(total_count=n(),
            .groups = 'drop')

#gerando base 5<preço_m2<80
LOCp_valid<- LOCm2_clean %>% filter(preco_m2_num>=5 & preco_m2_num<=80)

#limpando condominio_m2=na e valores numéricos
LOCp_valid$condominio_m2_num<- as.numeric (LOCp_valid$condominio_m2)
LOCp_valid$condominio_m2[is.infinite(LOCp_valid$condominio_m2)] <- NA
count(LOCp_valid)

#histograma percentil de condomínio_m2
condominiom2_decile <- quantile(LOCp_valid$condominio_m2_num, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
print(condominiom2_decile)
hist(condominiom2_decile, breaks = condominiom2_decile,main ="Histograma de condomínio/m2", xlab = "Condomínio/m2")

#histograma extremos de condominio_m2 
LOCm2_clean_condominio_num<-as.numeric(LOCp_valid$condominio_m2_num)
custom_breaks <- c(0,5,10,20,Inf)
breaks <-cut(LOCm2_clean_condominio_num, breaks=custom_breaks, right= TRUE, include.lowest= TRUE, stringsAsFactors = FALSE, na.rm = TRUE)
freq_table<-table(breaks)
barplot(freq_table, main ="Histograma de condomínio/m2", xlab = "Condomínio/m2", ylab = "Frequência", col = "blue", border = "black")

LOC_condominio <- LOCp_valid%>% group_by(condominio_m2<5,condominio_m2>20, na.rm = TRUE)%>%
  summarise(total_count=n(),
            .groups = 'drop')

#gerando base 5<preço_m2<80 e condomínio<20
LOCpc_valid<- LOCp_valid %>% filter(condominio_m2<=20 | is.na (condominio_m2))

#limpando iptu_m2=na e valores numéricos
LOCpc_valid$iptu_m2_num<- as.numeric (LOCpc_valid$iptu_m2)
count(LOCpc_valid)

#histograma percentil de iptu_m2
iptum2_decile <- quantile(LOCpc_valid$iptu_m2, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
print(iptum2_decile)
hist(iptum2_decile, breaks = iptum2_decile,main ="Histograma de IPTU/m2", xlab = "IPTU/m2",ylim = c(0, 1))

#histograma extremos de iptu_m2 
LOCm2_clean_iptu_num<-as.numeric(LOCp_valid$iptu_m2)
custom_breaks <- c(0,1,2,4,8,16,Inf)
breaks <-cut(LOCm2_clean_condominio_num, breaks=custom_breaks, right= TRUE, include.lowest= TRUE, stringsAsFactors = FALSE)
freq_table<-table(breaks)
barplot(freq_table, main ="Histograma de IPTU/m2", xlab = "IPTU/m2", ylab = "Frequência", col = "blue", border = "black")

LOC_iptu <- LOCpc_valid %>% group_by(iptu_m2<1,iptu_m2>16, na.rm = TRUE)%>%
  summarise(total_count=n(),
            .groups = 'drop')

#gerando base 5<preço_m2<80 e condomínio<20 e iptu<16
LOCpci_valid<- LOCpc_valid %>% filter(iptu_m2<=16 |is.na (iptu_m2 ))

#limpando area_util=na e valores numéricos
LOCpci_valid$area_util_num<- as.numeric (LOCpci_valid$area_util)
count(LOCpci_valid)

#histograma percentil de area_util
area_decile <- quantile(LOCpci_valid$area_util, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
print(area_decile)
hist(area_decile, breaks = area_decile,main ="Histograma de Área útil", xlab = "Área útil",ylim = c(0, 0.05))

#histograma extremos de area_util 
LOCm2_clean_area_num<-as.numeric(LOCpci_valid$area_util)
custom_breaks <- c(0,25,50,100,200,400,800, Inf)
breaks <-cut(LOCm2_clean_area_num, breaks=custom_breaks, right= TRUE, include.lowest= TRUE, stringsAsFactors = FALSE,na.rm = TRUE)
freq_table<-table(breaks)
barplot(freq_table, main ="Histograma de Área útil", xlab = "Área útil", ylab = "Frequência", col = "blue", border = "black")

LOC_area <- LOCpci_valid %>% group_by(area_util<25,area_util_num>800, na.rm = TRUE)%>%
  summarise(total_count=n(),
            .groups = 'drop')

#gerando base 5<preço_m2<80 e condomínio<20 e iptu<16 e área útil<800m2
LOCpcia_valid<- LOCpci_valid %>% filter(area_util<=800 |is.na (area_util ))

#limpando dormitorios=na e valores numéricos
LOCpcia_valid$dormitorios_num<- as.numeric (LOCpcia_valid$dormitorios)
count(LOCpcia_valid)
LOCpcia_valid$suite_num<- as.numeric (LOCpcia_valid$suites)
LOCpcia_valid$dormitorios_num[is.na(LOCpcia_valid$dormitorios)] <- 0
LOCpcia_valid$dormitorios_num[LOCpcia_valid$dormitorios_num== 0] <- LOCpcia_valid$suite_num[LOCpcia_valid$dormitorios_num== 0]
count(LOCpcia_valid)

#histograma percentil de dormitorios
dorm_decile <- quantile(LOCpcia_valid$dormitorios, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
print(dorm_decile)
hist(dorm_decile, breaks = dorm_decile,main ="Histograma de dormitórios", xlab = "dormitórios",ylim = c(0, 1.))

#histograma extremos de dormitorios 
LOCm2_clean_dormitorios_num<-as.numeric(LOCpcia_valid$dormitorios)
custom_breaks <- c(0,1,2,4,9,Inf)
breaks <-cut(LOCm2_clean_dormitorios_num, breaks=custom_breaks, right= TRUE, include.lowest= TRUE, stringsAsFactors = FALSE)
freq_table<-table(breaks)
barplot(freq_table, main ="Histograma de dormitórios", xlab = "dormitórios", ylab = "Frequência", col = "blue", border = "black")

LOC_dorm <- LOCpcia_valid %>% group_by(dormitorios_num<1,dormitorios_num>=9)%>%
  summarise(total_count=n(),
            .groups = 'drop')

#gerando base 5<preço_m2<80 e condomínio<20 e iptu<16 e área útil<800m2 e dormitório<10
LOCpciad_valid<- LOCpcia_valid %>% filter(dormitorios_num<10 |is.na (dormitorios_num ))

#limpando banheiros=na e valores numéricos
LOCpciad_valid$banheiros_num<- as.numeric (LOCpciad_valid$banheiros)
count(LOCpciad_valid)

#histograma percentil de banheiros
banho_decile <- quantile(LOCpciad_valid$banheiros, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
print(banho_decile)
hist(banho_decile, breaks = banho_decile,main ="Histograma de Banheiros", xlab = "Banheiros",ylim = c(0, 0.5))

#histograma extremos de banheiros 
LOCm2_clean_banheiros_num<-as.numeric(LOCpciad_valid$banheiros)
custom_breaks <- c(0,1,2,4,8,Inf)
breaks <-cut(LOCm2_clean_banheiros_num, breaks=custom_breaks, right= TRUE, include.lowest= TRUE, stringsAsFactors = FALSE)
freq_table<-table(breaks)
barplot(freq_table, main ="Histograma de  Banheiros", xlab = "Banheiros", ylab = "Frequência", col = "blue", border = "black")

LOC_banh <- LOCpciad_valid %>% group_by(banheiros>8)%>%
  summarise(total_count=n(),
            .groups = 'drop')

#gerando base 5<preço_m2<80 e condomínio<20 e iptu<16 e área útil<800m2, dormitório<10 e banheiros <8
LOCpciadb_valid<- LOCpciad_valid %>% filter(banheiros<=8 |is.na (banheiros ))

#limpando vagas=na e valores numéricos
LOCpciadb_valid$vagas_num<- as.numeric (LOCpciadb_valid$vagas)
count(LOCpciadb_valid)

#histograma percentil de vagas
vagas_decile <- quantile(LOCpciadb_valid$vagas, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
print(vagas_decile)
hist(vagas_decile, breaks = vagas_decile,main ="Histograma de Vagas de Garagem", xlab = "Vagas",ylim = c(0, 1))

#histograma extremos de vagas 
LOCm2_clean_vagas_num<-as.numeric(LOCpciadb_valid$vagas)
custom_breaks <- c(0,1,2,4,8,Inf)
breaks <-cut(LOCm2_clean_vagas_num, breaks=custom_breaks, right= TRUE, include.lowest= TRUE, stringsAsFactors = FALSE)
freq_table<-table(breaks)
barplot(freq_table, main ="Histograma de Vagas de garagem", xlab = "Área Vagas", ylab = "Frequência", col = "blue", border = "black")

LOC_vagas <- LOCpciadb_valid %>% group_by(vagas<8)%>%
  summarise(total_count=n(),
            .groups = 'drop')

#gerando base 5<preço_m2<80 e condomínio<20 e iptu<16 e área útil<800m2, dormitório<10, banheiros <8 e vagas <8
LOCpciadbv_valid<- LOCpciadb_valid %>% filter(vagas<8 |is.na (vagas ))


#Filtros cruzados de area_util e vagas
LOC_areavagas<- LOCpciadbv_valid %>% filter(area_util_num>300 & vagas>8)
LOC_dormvagas<- LOCpciadbv_valid %>% filter(dormitorios_num< 1 & vagas>5)
LOC_dormbanheiro<- LOCpciadbv_valid %>% filter(dormitorios_num< 2 & banheiros>5)
LOC_areadorm<- LOCpciadbv_valid %>% filter(dormitorios_num> 4 & area_util_num <50)

LOCpciadbv_valid <- LOCpciadbv_valid%>% mutate (limp_1 = dormitorios_num< 2 & banheiros>5)
LOCpciadbv_valid <- LOCpciadbv_valid%>% mutate (limp_2 = dormitorios_num< 1 & vagas>5)
LOCpciadbv_valid <- LOCpciadbv_valid%>% mutate (limp_3 = dormitorios_num> 4 & area_util_num<50)

LOCfinal_valid <- LOCpciadbv_valid%>% filter (limp_1!="TRUE" & limp_2!="TRUE" &  limp_3!="TRUE")
count(LOCfinal_valid)
LOCfinal_valid <- dplyr::select(LOCfinal_valid, !limp_1, !limp_2, !limp_3)

#Locacao por tipo e ano
LOCACAO_anotipo <- LOCfinal_valid %>% group_by(tipo_imovel, Ano) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

LOCACAO_anotipo

#Gerar soma preco+IPTU+concomínio
LOCfinal_valid <- LOCfinal_valid %>%
  mutate(condominio_m2 = ifelse(is.na(condominio_m2), 0, condominio_m2))
LOCfinal_valid <- LOCfinal_valid %>%
  mutate(iptu_m2 = ifelse(is.na(iptu_m2), 0, iptu_m2))

LOCfinal_valid <- LOCfinal_valid%>% mutate (valorm2 = iptu_m2+preco_m2+condominio_m2)

#inflacionar valores
#aqui, se o seu arquivo de excel esta no diretorio de trabalho, nao precisa digitar o caminho todo
library(readxl)
FipeZap <- read_excel("FipeZap.xlsx")
#view(FipeZap)

LOCfinal_valid <- full_join(LOCfinal_valid, FipeZap, by =c("Mes", "Ano"))
LOCfinal_valid <- LOCfinal_valid%>% mutate (valorm2_r = valorm2/FipeZAP)
LOCfinal_valid <- LOCfinal_valid%>% mutate (precom2_r = preco_m2/FipeZAP)
LOCfinal_valid <- LOCfinal_valid%>% mutate (iptum2_r = iptu_m2/FipeZAP)
LOCfinal_valid <- LOCfinal_valid%>% mutate (condm2_r = condominio_m2/FipeZAP)

#Descritivas
sum_valor<- LOCfinal_valid  %>%
  group_by(Ano, tipo_imovel) %>%
  summarize(
    media_valor = mean(valorm2_r),
    mediana_valor = median(valorm2_r),
    sd_valor = sd(valorm2_r),
    min_valor = min(valorm2_r),
    max_valor = max(valorm2_r)
  )

ggplot(sum_valor, aes(x = Ano, y = media_valor, fill = tipo_imovel)) +
  geom_bar(stat="identity", position=position_dodge())+
  labs(x = "Ano", y = "Média valor/m2",fill= "Tipo de Imóvel", title = "Valor/m2 do aluguel+IPTU+Condomínio por ano e tipo de imóvel" )


#gerando uma subamostra pra gerar o mapa
LOCfinal_valid_sample <- LOCfinal_valid %>% sample_frac(size = 0.05)

#instalando pacotes para gerar os mapas
library(leaflet)
library(sf)
library(geobr)

diadema <- read_municipality(code_muni = 3513801,year=2010,showProgress = FALSE,simplified = FALSE)

leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%  
  addPolygons(data=diadema , color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))

leaflet() %>% addProviderTiles("CartoDB.Positron") %>% 
  addCircleMarkers(data=LOCfinal_valid_sample,lat=LOCfinal_valid_sample$latitude,lng=LOCfinal_valid_sample$longitude,color = "blue", radius = 0.01) %>% 
  addPolygons(data=diadema , color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront=TRUE))

diadema_buffer <- st_buffer(diadema,2000)
diadema_buffer <- st_simplify(diadema_buffer,dTolerance = 200)

leaflet() %>% addProviderTiles("CartoDB.Positron") %>%  
  addPolygons(data=diadema , color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%  addPolygons(data=diadema_buffer , color = "green", weight = 2, smoothFactor = 0.5,
                                                                                         opacity = 1.0, fillOpacity = 0,
                                                                                         highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>% addCircleMarkers(data=LOCfinal_valid_sample,lat=LOCfinal_valid_sample$latitude,lng=LOCfinal_valid_sample$longitude,color = "blue", radius = 0.01)
#selecionar apenas as amostrar que estao ate 2 km da fronteira de Diadema
LOCfinal_valid_sample_sf <- st_as_sf(LOCfinal_valid_sample, coords = c("longitude", "latitude"))

st_crs(LOCfinal_valid_sample_sf)
LOCfinal_valid_sample_sf <- st_set_crs(LOCfinal_valid_sample_sf, 4674)
st_crs(LOCfinal_valid_sample_sf)
st_crs(diadema_buffer)

leaflet() %>% addProviderTiles("CartoDB.Positron") %>%  
  addPolygons(data=diadema , color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%  addPolygons(data=diadema_buffer , color = "green", weight = 2, smoothFactor = 0.5,
                                                                                         opacity = 1.0, fillOpacity = 0,
                                                                                         highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>% addCircleMarkers(data=LOCfinal_valid_sample_sf,lat=LOCfinal_valid_sample$latitude,lng=LOCfinal_valid_sample$longitude,color = "blue", radius = 0.01)

LOCfinal_valid_sample_sf_inside <- st_intersection(LOCfinal_valid_sample_sf, diadema_buffer)

leaflet() %>% addProviderTiles("CartoDB.Positron") %>%  
  addPolygons(data=diadema , color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%  addPolygons(data=diadema_buffer , color = "green", weight = 2, smoothFactor = 0.5,
                                                                                         opacity = 1.0, fillOpacity = 0,
                                                                                         highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%   addCircleMarkers(data = LOCfinal_valid_sample_sf_inside,color = "blue", radius = 0.01)

#Mapa com de valores por m2 sample com legenda de preços quando aponto o cursor
pal <- colorQuantile(c("lightgreen", "darkblue"), domain = LOCfinal_valid_sample$valorm2_r, n = 5, alpha = 0.5)
map0 <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = diadema, color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = FALSE)) %>%
  addCircleMarkers(data = LOCfinal_valid_sample_sf, label = sprintf("%.2f", LOCfinal_valid_sample_sf$valorm2_r),
                   color = ~pal(valorm2_r), radius = 2, stroke = FALSE, fillOpacity = 0.8) %>% 
  addLegend(position = "bottomright", pal = pal, values = LOCfinal_valid_sample_sf$valorm2_r,title = "Valor do aluguel por m2", opacity = 0.8)

map0

#selecionar apenas as amostrar que estão ate 2 km da fronteira de Diadema
LOCfinal_valid_sample_sf <- st_as_sf(LOCfinal_valid_sample, coords = c("longitude", "latitude"))
st_crs(LOCfinal_valid_sample_sf)
LOCfinal_valid_sample_sf <- st_set_crs(LOCfinal_valid_sample_sf, 4674)
st_crs(LOCfinal_valid_sample_sf)
st_crs(diadema_buffer)

leaflet() %>% addProviderTiles("CartoDB.Positron") %>%  
  addPolygons(data=diadema , color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%  addPolygons(data=diadema_buffer , color = "green", weight = 2, smoothFactor = 0.5,
                                                                                         opacity = 1.0, fillOpacity = 0,
                                                                                         highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>% addCircleMarkers(data=LOCfinal_valid_sample_sf,lat=LOCfinal_valid_sample$latitude,lng=LOCfinal_valid_sample$longitude,color = "blue", radius = 0.01)

LOCfinal_valid_sample_sf_inside <- st_intersection(LOCfinal_valid_sample_sf, diadema_buffer)

leaflet() %>% addProviderTiles("CartoDB.Positron") %>%  
  addPolygons(data=diadema , color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%  addPolygons(data=diadema_buffer , color = "green", weight = 2, smoothFactor = 0.5,
                                                                                         opacity = 1.0, fillOpacity = 0,
                                                                                         highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)) %>%   addCircleMarkers(data = LOCfinal_valid_sample_sf_inside,color = "blue", radius = 0.01)

#Mapa com de valores por m2 insde buffer com legenda de preços quando aponto o cursor
pal <- colorQuantile(c("lightgreen", "darkblue"), domain = LOCfinal_valid_sample$valorm2_r, n = 5, alpha = 0.5)
map1 <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = diadema, color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = FALSE)) %>%
  addCircleMarkers(data = LOCfinal_valid_sample_sf_inside, label = sprintf("%.2f", LOCfinal_valid_sample_sf_inside$valorm2_r),
                   color = ~pal(valorm2_r), radius = 2, stroke = FALSE, fillOpacity = 0.8) %>% 
  addLegend(position = "bottomright", pal = pal, values = LOCfinal_valid_sample_sf_inside$valorm2_r,title = "Valor do aluguel por m2", opacity = 0.8)

map1

#gerando uma subamostra das amostras totais dentro do buffer 2km
LOCfinal_valid_sf <- st_as_sf(LOCfinal_valid, coords = c("longitude", "latitude"))

st_crs(LOCfinal_valid_sf)
LOCfinal_valid_sf <- st_set_crs(LOCfinal_valid_sf, 4674)
st_crs(LOCfinal_valid_sf)
st_crs(diadema_buffer)

LOCfinal_valid_sf_inside <- st_intersection(LOCfinal_valid_sf, diadema_buffer)

count(LOCfinal_valid_sf_inside) #ficamos com uma amostra de 233349 observacoes

#criar variável distância do centro de Diadema
centro_diadema <- st_point(c(-46.62563335778199,-23.68665579771163 ))  # ponto indicando o centro de Diadema
centro_diadema_sf <- st_sf(geometry = st_sfc(centro_diadema))
centro_diadema_sf <- st_set_crs(centro_diadema_sf, 4674)

LOCfinal_valid_sf_inside <- LOCfinal_valid_sf_inside %>%
  mutate(dist_center = as.numeric(st_distance(LOCfinal_valid_sf_inside, centro_diadema_sf)))
names(LOCfinal_valid_sf_inside)

#incluir variável latitude e longitude em coordenadas planas UTM 23s
LOCfinal_valid_sf_inside <- st_transform(LOCfinal_valid_sf_inside, crs = 31983)
coordinates <- st_coordinates(LOCfinal_valid_sf_inside)
LOCfinal_valid_sf_inside$latitude <- coordinates[, "Y"]
LOCfinal_valid_sf_inside$longitude <- coordinates[, "X"]
names(LOCfinal_valid_sf_inside)
LOCfinal_valid_sf_inside <- st_transform(LOCfinal_valid_sf_inside, crs = 4674)

#vizualizar esta amostra no mapa
plot(LOCfinal_valid_sf_inside[41])
summary(LOCfinal_valid_sf_inside[41])
hist(LOCfinal_valid_sf_inside$precom2_r)

#plotando um mapa com os valores outliers de valorm2_r > 100
LOCfinal_valid_sf_inside_filter <- LOCfinal_valid_sf_inside %>% filter(LOCfinal_valid_sf_inside$valorm2_r>100)
summary(LOCfinal_valid_sf_inside_filter$valorm2_r)

map2 <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = diadema, color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = FALSE)) %>%
  addCircleMarkers(data = LOCfinal_valid_sf_inside_filter, label = sprintf("%.2f", LOCfinal_valid_sf_inside_filter$valorm2_r),
                   color = ~pal(valorm2_r), radius = 2, stroke = FALSE, fillOpacity = 0.8) %>% 
  addLegend(position = "bottomright", pal = pal, values = LOCfinal_valid_sf_inside_filter$valorm2_r,title = "Valor do aluguel por m2", opacity = 0.8)
map2

#MODELO HEDÔNICO EM LOG
#Estimar valor hedônico para amostras dentro do buffer
# Converter dist_center em 'numeric'
LOCfinal_valid_sf_inside$distcenter <- as.numeric(LOCfinal_valid_sf_inside$dist_center)
LOCfinal_valid_sf_inside <- LOCfinal_valid_sf_inside %>%
  mutate(apart = case_when(tipo_imovel == "APARTAMENTO" ~ 1, TRUE ~ 0))

names(LOCfinal_valid_sf_inside)

#gerar logaritmos
LOCfinal_valid_sf_inside <- LOCfinal_valid_sf_inside %>%
  mutate(ln_valorm2 = log(valorm2_r),
         ln_area=log(area_util),
         ln_dorm=log(dormitorios),
         ln_suite=log(suites),
         ln_andar=log(andar),
         ln_vaga=log(vagas),
         ln_banho=log(banheiros),
         ln_dist=log(dist_center))

#verificar e corrigir infinitos
any(is.infinite(LOCfinal_valid_sf_inside$ln_valorm2))
any(is.infinite(LOCfinal_valid_sf_inside$ln_area))
any(is.infinite(LOCfinal_valid_sf_inside$ln_banho))
any(is.infinite(LOCfinal_valid_sf_inside$ln_dorm))
any(is.infinite(LOCfinal_valid_sf_inside$ln_suite))
any(is.infinite(LOCfinal_valid_sf_inside$ln_andar))
any(is.infinite(LOCfinal_valid_sf_inside$ln_dist))
any(is.infinite(LOCfinal_valid_sf_inside$ln_vaga))

LOCfinal_valid_sf_inside$ln_vaga <- replace(LOCfinal_valid_sf_inside$ln_vaga, is.infinite(LOCfinal_valid_sf_inside$ln_vaga), 0)
LOCfinal_valid_sf_inside$ln_dorm <- replace(LOCfinal_valid_sf_inside$ln_dorm, is.infinite(LOCfinal_valid_sf_inside$ln_dorm), 0)
LOCfinal_valid_sf_inside$ln_andar <- replace(LOCfinal_valid_sf_inside$ln_andar, is.infinite(LOCfinal_valid_sf_inside$ln_andar), 0)
LOCfinal_valid_sf_inside$ln_dorm <- replace(LOCfinal_valid_sf_inside$ln_dorm, is.infinite(LOCfinal_valid_sf_inside$ln_dorm), 0)
LOCfinal_valid_sf_inside$ln_suite <- replace(LOCfinal_valid_sf_inside$ln_suite, is.infinite(LOCfinal_valid_sf_inside$ln_suite), 0)

hedonic_inside <- lm(ln_valorm2 ~ ln_area +apart + ln_dorm + ln_suite + ln_andar + ln_vaga + ln_banho + latitude +longitude +ln_dist, data = LOCfinal_valid_sf_inside)

# Verificar os resultados da regressão
library(broom)
summary(hedonic_inside)
resultado_hedonic <- tidy(hedonic_inside)
write.csv(resultado_hedonic, file = "resultados_hedonico.csv", row.names = FALSE)

# Fazer previsões com o modelo
LOCfinal_valid_sf_inside$predicted_valorm2 <- predict(hedonic_inside, newdata = LOCfinal_valid_sf_inside)

#Descritivas
summary(LOCfinal_valid_sf_inside[c("predicted_valorm2","ln_valorm2","dormitorios","suites","vagas","banheiros")])

# Comparar preços reais com preços previstos
ggplot(data = LOCfinal_valid_sf_inside, aes(x = ln_valorm2, y = predicted_valorm2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Comparação entre Valores Reais e Previstos",
       x = "Log Valor Real (valor/m2)",
       y = "Log Valor Previsto")

#limpar memória
gc()

#teste de significância do modelo
resultado_anova <- anova(hedonic_inside)
print(resultado_anova)

# Estimar modelo de aluguel social em log
# Criar as características desejadas
LOCfinal_valid_sf_inside <- LOCfinal_valid_sf_inside %>% 
  mutate(area_s = log(50),
         dorm2 = log(2),
         suite0 = 0,
         vaga1 = log(1),
         banho1 = log(1))

# Acessar diretamente os coeficientes estimados do modelo 'hedonic_inside'
coeficientes <- coef(hedonic_inside)

# Calcular o valor de 'valorsocial_estlg' com base nos coeficientes e nas características criadas
LOCfinal_valid_sf_inside$lnvalorsocial_est <- coeficientes[1] +
  coeficientes[2] * LOCfinal_valid_sf_inside$area_s +
  coeficientes[3] * LOCfinal_valid_sf_inside$apart +
  coeficientes[4] * LOCfinal_valid_sf_inside$dorm2 +
  coeficientes[5] * LOCfinal_valid_sf_inside$suite0 +
  coeficientes[6] * LOCfinal_valid_sf_inside$ln_andar+
  coeficientes[7] * LOCfinal_valid_sf_inside$vaga1 +
  coeficientes[8] * LOCfinal_valid_sf_inside$banho1 +
  coeficientes[9] * LOCfinal_valid_sf_inside$latitude +
  coeficientes[10] * LOCfinal_valid_sf_inside$longitude +
  coeficientes[11] * LOCfinal_valid_sf_inside$ln_dist

summary(LOCfinal_valid_sf_inside$lnvalorsocial_est)
LOCfinal_valid_sf_inside <- LOCfinal_valid_sf_inside %>% 
  mutate(valorsocial_estlg = exp(lnvalorsocial_est))
summary(LOCfinal_valid_sf_inside$valorsocial_estlg)
summary(LOCfinal_valid_sf_inside[c("valorsocial_estlg","predicted_valorm2","ln_valorm2","dormitorios","suites","vagas","banheiros")])
column_vector <- LOCfinal_valid_sf_inside[["valorsocial_estlg"]]
quantiles <- quantile(column_vector, probs = c(0,0.25, 0.5, 0.75,1))
print(quantiles)
quartile_groups <- cut(column_vector, breaks = quantiles, labels = FALSE)
quartile_counts <- table(quartile_groups)
print(quartile_counts)

#descritiva somente de Diadema
diadema_data <- LOCfinal_valid_sf_inside[LOCfinal_valid_sf_inside$shp_municipio == "Diadema", ]
column_vector <- diadema_data[["valorsocial_estlg"]]
quantiles <- quantile(column_vector, probs = c(0,0.25, 0.5, 0.75,1))
print(quantiles)
quartile_groups <- cut(column_vector, breaks = quantiles, labels = FALSE)
quartile_counts <- table(quartile_groups)
print(quartile_counts)

#mapa com valores do aluguel social
pal <- colorNumeric(c("lightgreen", "darkblue"), 
                    domain = LOCfinal_valid_sf_inside$valorsocial_estlg, 
                    na.color = "gray")

map4 <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = diadema, color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = FALSE)) %>%
  addCircleMarkers(data = LOCfinal_valid_sf_inside, 
                   label = sprintf("%.2f", LOCfinal_valid_sf_inside$valorsocial_estlg),
                   color = ~pal(valorsocial_estlg), 
                   radius = 2, 
                   stroke = FALSE, 
                   fillOpacity = 0.8) %>% 
  addLegend(position = "bottomright", 
            pal = pal, 
            values = LOCfinal_valid_sf_inside$valorsocial_estlg,
            title = "Valor do aluguel 
            social por m2", 
            opacity = 0.8)

map4

names(LOCfinal_valid_sf_inside)

## interpolação para construção das curvas de preço
library(sf)
library(terra)
library(spData)
library(tmap)
library(viridis)
library(raster)
library(gstat)
library(tgp)

##criando uma base com as coordenadas e valores estimados
LOCfinal_valid_sf_inside_kirg <- LOCfinal_valid_sf_inside %>% dplyr::select(geometry, valorsocial_estlg)
## usando coordenadas planas 
LOCfinal_valid_sf_inside_kirg_UTM <- st_transform(LOCfinal_valid_sf_inside_kirg, crs = 31983)
coords <- st_coordinates(LOCfinal_valid_sf_inside_kirg_UTM)
valorsocial_XY_UTM <- data.frame(
  Longitude = coords[, 1],
  Latitude = coords[, 2],
  valorsocial_estlg = LOCfinal_valid_sf_inside_kirg_UTM$valorsocial_estlg
)
names(valorsocial_XY_UTM)
valorsocial_XY_UTM <- valorsocial_XY_UTM %>% sample_frac(size = 0.3)

#######################################################################################
# # rodando um modelo de interpolacao baseado em Local Polynomial Regression Fitting# #
#######################################################################################
valorsocial_XY_UTM_int <- interp.loess(valorsocial_XY_UTM$Longitude, valorsocial_XY_UTM$Latitude, valorsocial_XY_UTM$valorsocial_estlg, gridlen=c(100,100), span=0.6)
contour(valorsocial_XY_UTM_int)
class(valorsocial_XY_UTM_int)
#criando um grid para inputar os valores estimados
grid <- expand.grid(x = valorsocial_XY_UTM_int$x, y = valorsocial_XY_UTM_int$y)
grid$z <- matrix(valorsocial_XY_UTM_int$z, nrow = 10000)
grid_sf <- st_as_sf(grid, coords = c("x", "y"), crs = 31983)
class(grid_sf)
plot(grid_sf)
st_crs(grid_sf) 
names(grid_sf)
library(stars)
crs(diadema)
diadema_utm <- st_transform(diadema, crs = 31983)
grid_diadema <- st_intersection(grid_sf, diadema_utm)
plot(grid_diadema)
# # criando uma base espacial tipo raster com os valores interpolados
grid_diadema_raster<-st_rasterize(grid_diadema %>% dplyr::select(z, geometry))
plot(grid_diadema_raster)
names(diadema_utm)
diadema_utm <- diadema_utm[, !(names(diadema_utm) %in% c("name_muni", "code_state", "abbrev_state"))]
# # recortando o raster usando o limite de municipio de Diadema
grid_diadema_raster_crop<-st_crop(grid_diadema_raster ,diadema_utm,crop = TRUE)
plot(grid_diadema_raster_crop,reset = FALSE)
contour(grid_diadema_raster_crop,add=TRUE)
# # criando as curvas de nivel de preços
countours <-  st_contour(grid_diadema_raster_crop, contour_lines = TRUE)
plot(countours)
class(countours)
ggplot(buffer_interno) +
  geom_sf() +
  geom_sf(data = countours)
# # criando um buffer interno em relaçao a divisa de Diadema de modo a garantir que as
# # curvas de nível efetivamente cruzem a fornteira para garantir a subdivisao do municipio 
# # em zonas de preços (aqui usando uma distancia de 100 metros)
buffer_interno <- st_buffer(diadema_utm, -100) 
# # subdividindo o municpio em zonas de preços
zonas_preços <- lwgeom::st_split(buffer_interno,countours) %>%
  st_collection_extract("POLYGON")
zonas_preços_degree <- st_transform(zonas_preços, crs = 4674)
map4a <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = zonas_preços_degree, color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = FALSE))
map4a


#################################################################################################
# #rodando um modelo de interpolacao baseado em Inverse Distance Weighting (IDW) Interpolation# #
#################################################################################################
tmap_mode("plot")
map <- st_union(diadema_buffer) %>% st_sf()
tm_shape(map) + tm_polygons(alpha = 0.3) + tm_shape(LOCfinal_valid_sf_inside_kirg) +
  tm_dots("valorsocial_estlg", palette = "viridis")
###rodando a interpolacao
res <- gstat(formula = valorsocial_estlg ~ 1, locations = LOCfinal_valid_sf_inside_kirg_UTM,
             nmax = nrow(LOCfinal_valid_sf_inside_kirg_UTM), # use all the neighbors locations
             set = list(idp = 1)) # beta = 1 
resp <- predict(res, grid_sf)
resp$x <- st_coordinates(resp)[,1]
resp$y <- st_coordinates(resp)[,2]
resp$pred <- resp$var1.pred
ggplot() + geom_sf(data = resp, aes(color = pred)) +
  scale_color_viridis(name = "pred") + theme_bw()
class(resp)
grid_diadema1 <- st_intersection(resp, diadema_utm)
plot(grid_diadema1)

# # criando uma base espacial tipo raster com os valores interpolados
grid_diadema_raster1<-st_rasterize(grid_diadema1 %>% dplyr::select(var1.pred, geometry))
plot(grid_diadema_raster1)
# # recortando o raster usando o limite de municipio de Diadema
grid_diadema_raster1_crop<-st_crop(grid_diadema_raster1 ,diadema_utm,crop = TRUE)
plot(grid_diadema_raster1_crop,reset = FALSE)
contour(grid_diadema_raster1_crop,add=TRUE)
# # criando as curvas de nivel de preços
countours1 <-  st_contour(grid_diadema_raster1_crop, contour_lines = TRUE)
plot(countours1)
class(countours1)
ggplot(buffer_interno) +
  geom_sf() +
  geom_sf(data = countours1)
# # subdividindo o municpio em zonas de preços
zonas_preços1 <- lwgeom::st_split(buffer_interno,countours1) %>%
  st_collection_extract("POLYGON")

zonas_preços1_degree <- st_transform(zonas_preços1, crs = 4674)
map4b <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = zonas_preços1_degree, color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront =FALSE))

map4b

# rodando um modelo de interpolacao baseado em Kriging
LOCfinal_valid_sf_inside_kirg_UTM_s <- LOCfinal_valid_sf_inside_kirg_UTM %>% sample_frac(size = 0.05)
# Defina o tamanho da célula, que depende da resolução desejada
cell_size <- 150  # Ajuste conforme necessário
# Crie a grade com a geometria correta (polygon)
grid_sf_krig <- st_make_grid(LOCfinal_valid_sf_inside_kirg_UTM_s, cellsize = c(cell_size, cell_size))
st_crs(grid_sf_krig) <- st_crs(LOCfinal_valid_sf_inside_kirg_UTM_s)
# Converta a grade em um objeto com geometria sfc
grid_sf_krig <- grid_sf_krig %>%
  st_as_sf()
# Verifique se as células têm pontos e calcule a média dos valores
grid_with_means <- grid_sf_krig %>%
  st_join(LOCfinal_valid_sf_inside_kirg_UTM_s) %>%
  group_by(geometry) %>%
  summarise(valorsocial_estlg_mean = mean(valorsocial_estlg, na.rm = TRUE))
# Remova células sem valores válidos
grid_with_means <- grid_with_means[!is.na(grid_with_means$valorsocial_estlg_mean),]

# Crie um objeto variogram 
LOCfinal_valid_sf_inside_kirg_UTM_s <- LOCfinal_valid_sf_inside_kirg_UTM %>% sample_frac(size = 0.3)
variogram_model <- variogram(valorsocial_estlg ~ 1, LOCfinal_valid_sf_inside_kirg_UTM_s)
plot(variogram_model)
#fitted_variogram <- fit.variogram(variogram_model, vgm("Exc"))
fitted_variogram <- fit.variogram(object = variogram_model,
                                  model = vgm(psill = 22, model = "Sph",
                                              range = 1000,kappa=0.5, nugget = 1))
plot(variogram_model, model = fitted_variogram)
fitted_variogram

k <- gstat(formula = grid_with_means$valorsocial_estlg_mean ~ 1, data = grid_with_means, model = fitted_variogram)
kpred <- predict(k, grid_sf_krig)

ggplot() + geom_sf(data = kpred, aes(color = var1.pred)) +
  geom_sf(data = grid_with_means) +
  scale_color_viridis(name = "valorsocial_estlg_mean") + theme_bw()

raster_grid <- rast(ext(grid_sf), resolution =100)
raster_grid
valorkrig_raster <- rasterize(kpred, raster_grid, field="var1.pred", fun = "mean", na.rm = TRUE)
valorkrig_raster
resp_raster_cropped_k <- crop(valorkrig_raster, diadema_utm)
resp_raster_masked_k = mask(resp_raster_cropped_k, diadema_utm)
plot(resp_raster_masked_k)
#calculando as curvas de preço krig
curvas_preco_k = as.contour(resp_raster_masked_k) 
plot(resp_raster_masked_k, axes = FALSE)
plot(resp_raster_masked_k, add = TRUE)
plot(curvas_preco_k,add=TRUE)
class(curvas_preco_k)
#transformando as curvas em vetor simplefeature
curvas_preco_k_sf <- sf::st_as_sf(curvas_preco_k)
plot(curvas_preco_k_sf)
class(curvas_preco_k_sf)
st_crs(curvas_preco_k_sf) <- st_crs(31983)
curva_preco<- ggplot() + geom_sf(data = curvas_preco_k_sf,aes(color=level),lwd=1)+scale_colour_gradient(low = "cyan", high = "darkred") +
  geom_sf_label(data = curvas_preco_k_sf, aes(label = level), size = 2) +
  geom_sf(data=diadema_utm,alpha = 0.2)+ theme_bw()
curva_preco

# # subdividindo o municpio em zonas de preços
zonas_preços2 <- lwgeom::st_split(buffer_interno,curvas_preco_k_sf) %>%
  st_collection_extract("POLYGON")

plot (zonas_preços2)
class (zonas_preços2)
distritos_diadema <- read_sf("zonas_OP_Diadema.gpkg")
distrito_diadema_degree <- st_transform(distritos_diadema, crs = 4674)
crs(distrito_diadema_degree)

zonas_preços2_degree <- st_transform(zonas_preços2, crs = 4674)

curva_preco2 <- leaflet() %>%
  addPolygons(data = distrito_diadema_degree,fillOpacity = 0)%>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = zonas_preços2_degree,  color= 'red',weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront =FALSE))
curva_preco2

curva_preco3 <- leaflet() %>%
  addPolygons(data = distrito_diadema_degree,fillOpacity = 0)%>%
  addProviderTiles("Esri.WorldImagery") %>%
  addPolygons(data = zonas_preços2_degree,  color= 'yellow',weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront =FALSE))
curva_preco3

curva_preco4<- ggplot() + 
  geom_sf(data=diadema_utm,alpha = 0.2)+ theme_bw()+
  geom_sf(data = curvas_preco_k_sf,aes(color=level),lwd=1)+scale_colour_gradient(low = "cyan", high = "darkblue") +
  geom_sf_label(data = curvas_preco_k_sf, aes(label = level), size = 2) +
  geom_sf(data=distritos_diadema,color= 'red', lwd=1,alpha=0.2)
curva_preco4





#Filtro de imóveis com <50m2
LOCfinal_valid_sf_inside_50 <- LOCfinal_valid_sf_inside[LOCfinal_valid_sf_inside$area_util<50,]
summary(LOCfinal_valid_sf_inside_50[c("valorm2","dormitorios","suites","vagas","banheiros")])

pal <- colorNumeric(c("lightgreen", "darkblue"), 
                    domain = LOCfinal_valid_sf_inside_50$valorm2, 
                    na.color = "gray")

map5 <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = diadema, color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = FALSE)) %>%
  addCircleMarkers(data = LOCfinal_valid_sf_inside_50, 
                   label = sprintf("%.2f", LOCfinal_valid_sf_inside_50$valorm2),
                   color = ~pal(valorm2), 
                   radius = 2, 
                   stroke = FALSE, 
                   fillOpacity = 0.8) %>% 
  addLegend(position = "bottomright", 
            pal = pal, 
            values = LOCfinal_valid_sf_inside_50$valorm2,
            title = "Valor do aluguel 
            social por m2", 
            opacity = 0.8)

map5






