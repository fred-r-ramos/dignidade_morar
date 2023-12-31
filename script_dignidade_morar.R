library(tidyverse)

#aqui vou deixar as duas linhas digitadas e uma delas comentario para evitar ficar digitando sempre

setwd( "C:/Users/fredr/Dropbox/B_PROJETOS_PESQUISA_GV/ZAP_DATA_HABITACAO/dignidade_morar")
#setwd( "G:/My Drive/DOCS/Urbana_habitação/Dignidade_Morar/dignidade_morar")

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
LOCfinal_valid <-LOCfinal_valid %>% select(-limp_1 & -limp_2 & -limp_3)

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

leaflet() %>% addProviderTiles("CartoDB.Positron") %>%  
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
#Mapa com de valores por m2 com legenda de preços quando aponto o cursor
library(leaflet)
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

#Estimar valor hedônico para amostras dentro do buffer
# Converter dist_center em 'numeric'
LOCfinal_valid_sf_inside$distcenter <- as.numeric(LOCfinal_valid_sf_inside$dist_center)
LOCfinal_valid_sf_inside <- LOCfinal_valid_sf_inside %>%
  mutate(apart = case_when(tipo_imovel == "APARTAMENTO" ~ 1, TRUE ~ 0))

names(LOCfinal_valid_sf_inside)
hedonic_inside <- lm(valorm2 ~ area_util  +apart + dormitorios + suites + andar + vagas + banheiros + latitude +longitude +dist_center, data = LOCfinal_valid_sf_inside)

# Verificar os resultados da regressão
summary(hedonic_inside)

# Fazer previsões com o modelo
LOCfinal_valid_sf_inside$predicted_valorm2 <- predict(hedonic_inside, newdata = LOCfinal_valid_sf_inside)

#Descritivas
summary(LOCfinal_valid_sf_inside[c("predicted_valorm2","valorm2_r","dormitorios","suites","vagas","banheiros")])

# Comparar preços reais com preços previstos
ggplot(data = LOCfinal_valid_sf_inside, aes(x = valorm2_r, y = predicted_valorm2)) +
  geom_point() +
  geom_abline(intercept = -20, slope = 1, color = "red") +
  labs(title = "Comparação entre Preços Reais e Previstos",
       x = "Preço Real (valorm2)",
       y = "Preço Previsto")

#teste de significância do modelo
resultado_anova <- anova(hedonic_inside)
print(resultado_anova)

#teste de multicolinearidade (VIF), apresenta alguma multicolinearidade 
library(usdm)
library(car)
vif_result <- vif(hedonic_inside)
print(vif_result)

#Teste de autocorrelação dos resíduos (Durbin-Watson), com autocorrelação positiva
library(lmtest)
teste_durbin_watson <- dwtest(hedonic_inside)
print(teste_durbin_watson)

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


# Estimar modelo de aluguel social
# Criar as características desejadas
LOCfinal_valid_sf_inside <- LOCfinal_valid_sf_inside %>% 
  mutate(area_s =50,
         dorm2 = 2,
         suite0 = 0,
         vaga1 = 1,
         banho1 = 1)

# Acessar diretamente os coeficientes estimados do modelo 'hedonic_inside'
coeficientes <- coef(hedonic_inside)

# Calcular o valor de 'valorsocial_est' com base nos coeficientes e nas características criadas
LOCfinal_valid_sf_inside$valorsocial_est <- coeficientes[1] +
  coeficientes[2] * LOCfinal_valid_sf_inside$area_s +
  coeficientes[3] * LOCfinal_valid_sf_inside$apart +
  coeficientes[4] * LOCfinal_valid_sf_inside$dorm2 +
  coeficientes[5] * LOCfinal_valid_sf_inside$suite0 +
  coeficientes[6] * LOCfinal_valid_sf_inside$andar +
  coeficientes[7] * LOCfinal_valid_sf_inside$vaga1 +
  coeficientes[8] * LOCfinal_valid_sf_inside$banho1 +
  coeficientes[9] * LOCfinal_valid_sf_inside$latitude +
  coeficientes[10] * LOCfinal_valid_sf_inside$longitude +
  coeficientes[11] * LOCfinal_valid_sf_inside$dist_center

summary(LOCfinal_valid_sf_inside$valorsocial_est)

#mapa com valores do aluguel social
pal <- colorNumeric(c("lightgreen", "darkblue"), 
                    domain = LOCfinal_valid_sf_inside$valorsocial_est, 
                    na.color = "gray")

map3 <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = diadema, color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = FALSE)) %>%
  addCircleMarkers(data = LOCfinal_valid_sf_inside, 
                   label = sprintf("%.2f", LOCfinal_valid_sf_inside$valorsocial_est),
                   color = ~pal(valorsocial_est), 
                   radius = 2, 
                   stroke = FALSE, 
                   fillOpacity = 0.8) %>% 
  addLegend(position = "bottomright", 
            pal = pal, 
            values = LOCfinal_valid_sf_inside$valorsocial_est,
            title = "Valor do aluguel social por m2", 
            opacity = 0.8)
map3

#MODELO EM LOG
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
summary(hedonic_inside)

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

#Índice de Moran
library(spdep)
library(lmtest)

residuos <- residuals(hedonic_inside)
hist(residuos)
# Suponha que seus dados tenham informações de latitude e longitude em colunas chamadas "latitude" e "longitude"
coords <- cbind(LOCfinal_valid_sf_inside$latitude, LOCfinal_valid_sf_inside$longitude)
# Crie a matriz de vizinhança
#matriz_pesos <- dnearneigh(coords, d1 = 0, d2 = 400)  # Ajuste a distância conforme necessário
# Converta a matriz de pesos em um objeto listw
#matriz_pesos_listw <- nb2listw(matriz_pesos)
#moran_result <- moran.test(residuos, matriz_pesos_listw)
#moran_result$statistic
#moran_result$p.value

library(spdep)
library(deldir)

#limpar memória
#rm(list = ls())
gc()


#teste de significância do modelo
resultado_anova <- anova(hedonic_inside)
print(resultado_anova)

#teste de multicolinearidade (VIF), apresenta alguma multicolinearidade 
library(usdm)
library(car)
vif_result <- vif(hedonic_inside)
print(vif_result)

#Teste de autocorrelação dos resíduos (Durbin-Watson), com autocorrelação positiva
teste_durbin_watson <- dwtest(hedonic_inside)
print(teste_durbin_watson)

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

# Calcular o valor de 'valorsocial_est' com base nos coeficientes e nas características criadas
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
summary(LOCfinal_valid_sf_inside[c("valorsocial_est","valorsocial_estlg","predicted_valorm2","ln_valorm2","dormitorios","suites","vagas","banheiros")])
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

map3a <- leaflet() %>%
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

map3a


names(LOCfinal_valid_sf_inside)

## interpolação para construção das curvas de preço
library(spData)
library(sf)
library(terra)
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


# rodando um modelo de interpolacao baseado em Local Polynomial Regression Fitting
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
raster_grid <- rast(ext(grid_sf), resolution =150)
raster_grid
# criando uma base espacial tipo raster com os valores interpolados
valorsocial_raster <- rasterize(grid_sf, raster_grid, field=grid_sf$z, fun = min, na.rm = TRUE)
valorsocial_raster
plot(valorsocial_raster)
crs(valorsocial_raster)<-  "PROJCRS[\"SIRGAS 2000 / UTM zone 23S\",\n    BASEGEOGCRS[\"SIRGAS 2000\",\n        DATUM[\"Sistema de Referencia Geocentrico para las AmericaS 2000\",\n            ELLIPSOID[\"GRS 1980\",6378137,298.257222101,\n                LENGTHUNIT[\"metre\",1]]],\n        PRIMEM[\"Greenwich\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433]],\n        ID[\"EPSG\",4674]],\n    CONVERSION[\"UTM zone 23S\",\n        METHOD[\"Transverse Mercator\",\n            ID[\"EPSG\",9807]],\n        PARAMETER[\"Latitude of natural origin\",0,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8801]],\n        PARAMETER[\"Longitude of natural origin\",-45,\n            ANGLEUNIT[\"degree\",0.0174532925199433],\n            ID[\"EPSG\",8802]],\n        PARAMETER[\"Scale factor at natural origin\",0.9996,\n            SCALEUNIT[\"unity\",1],\n            ID[\"EPSG\",8805]],\n        PARAMETER[\"False easting\",500000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8806]],\n        PARAMETER[\"False northing\",10000000,\n            LENGTHUNIT[\"metre\",1],\n            ID[\"EPSG\",8807]]],\n    CS[Cartesian,2],\n        AXIS[\"(E)\",east,\n            ORDER[1],\n            LENGTHUNIT[\"metre\",1]],\n        AXIS[\"(N)\",north,\n            ORDER[2],\n            LENGTHUNIT[\"metre\",1]],\n    USAGE[\n        SCOPE[\"Engineering survey, topographic mapping.\"],\n        AREA[\"Brazil - between 48°W and 42°W, northern and southern hemispheres, onshore and offshore.\"],\n        BBOX[-33.5,-48,5.13,-42]],\n    ID[\"EPSG\",31983]]"
diadema_utm <- st_transform(diadema,31983)
#recortar o raster sobreopondo as fornateiras de Diadema
valorsocial_raster_cropped = crop(valorsocial_raster, diadema_utm)
plot(valorsocial_raster_cropped)
valorsocial_raster_masked = mask(valorsocial_raster_cropped, diadema_utm)
plot(valorsocial_raster_masked)
#calculando as curvas de preço
curvas_preco = as.contour(valorsocial_raster_masked) 
plot(valorsocial_raster_masked, axes = FALSE)
plot(valorsocial_raster_masked, add = TRUE)
plot(curvas_preco,add=TRUE)
class(curvas_preco)
plot(curvas_preco,add=TRUE)
#transformando as curvas em vetor simplefeature
curvas_preco_sf <- sf::st_as_sf(curvas_preco)
ggplot() + geom_sf(data = curvas_preco_sf) +geom_sf_label(data = curvas_preco_sf, aes(label = level), size = 2) +geom_sf(data=diadema_utm,alpha = 0.2)+ theme_bw()



# rodando um modelo de interpolacao baseado em Inverse Distance Weighting (IDW) Interpolation
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
valorsocial_raster1 <- rasterize(resp, raster_grid, field=resp$pred, fun = min, na.rm = TRUE)
resp_raster_cropped <- crop(valorsocial_raster1, diadema_utm)
resp_raster_masked = mask(resp_raster_cropped, diadema_utm)
plot(resp_raster_masked)
#calculando as curvas de preço
curvas_preco1 = as.contour(resp_raster_masked) 
plot(resp_raster_masked, axes = FALSE)
plot(resp_raster_masked, add = TRUE)
plot(curvas_preco1,add=TRUE)
class(curvas_preco1)
#transformando as curvas em vetor simplefeature
curvas_preco1_sf <- sf::st_as_sf(curvas_preco1)
class(curvas_preco1_sf)
st_crs(curvas_preco1_sf) <- st_crs(31983)
ggplot() + geom_sf(data = curvas_preco1_sf) +geom_sf_label(data = curvas_preco1_sf, aes(label = level), size = 2) +geom_sf(data=diadema_utm,alpha = 0.2)+ theme_bw()

# rodando um modelo de interpolacao baseado em Kriging
LOCfinal_valid_sf_inside_kirg_UTM_s <- LOCfinal_valid_sf_inside_kirg_UTM %>% sample_frac(size = 0.3)
variogram_model <- variogram(valorsocial_estlg ~ 1, LOCfinal_valid_sf_inside_kirg_UTM_s)
plot(variogram_model)
fitted_variogram <- fit.variogram(variogram_model, vgm("Exc"))
print(fitted_variogram)
plot(variogram_model, model = fitted_variogram)

####a interpolacao por krige esta tomando muito tempo
kriged <- krige(valorsocial_estlg ~ 1, LOCfinal_valid_sf_inside_kirg_UTM_s,grid_sf, model = fitted_variogram)
class(kriged)
plot(kriged)

#Filtro de imóveis com <50m2
LOCfinal_valid_sf_inside_50 <- LOCfinal_valid_sf_inside[LOCfinal_valid_sf_inside$area_util<50,]
summary(LOCfinal_valid_sf_inside_50[c("valorm2","dormitorios","suites","vagas","banheiros")])
#mapa
pal <- colorNumeric(c("lightgreen", "darkblue"), 
                    domain = LOCfinal_valid_sf_inside_50$valorm2, 
                    na.color = "gray")

map4 <- leaflet() %>%
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

map4



>>>>>>> f1b70d9151ef9533437371c1aca67fa186d2a7d7





# Gere n valores aleatórios entre 42 e 50
#area_s <- runif(n, min = 40, max = 50)
#dorm_s <- sample(0:2, n,replace = TRUE)
#vaga_s <- sample(0:1, n,replace = TRUE)

# Adicione os valores gerados como uma nova coluna na base de dados
#LOCfinal_valid_sf_inside$area_s <- log(area_s)
#LOCfinal_valid_sf_inside$dorm_s <- log(dorm_s)
#LOCfinal_valid_sf_inside$vaga_s <- log(vaga_s)

#any(is.infinite(LOCfinal_valid_sf_inside$area_s))
#any(is.infinite(LOCfinal_valid_sf_inside$dorm_s))
#any(is.infinite(LOCfinal_valid_sf_inside$vaga_s))

#LOCfinal_valid_sf_inside$dorm_s <- replace(LOCfinal_valid_sf_inside$dorm_s, is.infinite(LOCfinal_valid_sf_inside$dorm_s), 0)
#LOCfinal_valid_sf_inside$vaga_s <- replace(LOCfinal_valid_sf_inside$vaga_s, is.infinite(LOCfinal_valid_sf_inside$vaga_s), 0)

# Acessar diretamente os coeficientes estimados do modelo 'hedonic_inside'
#coeficientes <- coef(hedonic_inside)

# Calcular o valor de 'valorsocial_est' com base nos coeficientes e nas características criadas
#LOCfinal_valid_sf_inside$lnvalorsocial_est <- coeficientes[1] +
#  coeficientes[2] * LOCfinal_valid_sf_inside$area_s +
#  coeficientes[3] * LOCfinal_valid_sf_inside$apart +
#  coeficientes[4] * LOCfinal_valid_sf_inside$dorm_s +
#  coeficientes[5] * LOCfinal_valid_sf_inside$suite0 +
#  coeficientes[6] * LOCfinal_valid_sf_inside$ln_andar+
#  coeficientes[7] * LOCfinal_valid_sf_inside$vaga_s +
#  coeficientes[8] * LOCfinal_valid_sf_inside$banho1 +
#  coeficientes[9] * LOCfinal_valid_sf_inside$latitude +
#  coeficientes[10] * LOCfinal_valid_sf_inside$longitude +
#  coeficientes[11] * LOCfinal_valid_sf_inside$ln_dist

#summary(LOCfinal_valid_sf_inside$lnvalorsocial_est)
#LOCfinal_valid_sf_inside <- LOCfinal_valid_sf_inside %>% 
#  mutate(valorsocial_estlg = exp(lnvalorsocial_est))
#summary(LOCfinal_valid_sf_inside$valorsocial_estlg)
#summary(LOCfinal_valid_sf_inside[c("valorsocial_est","valorsocial_estlg","predicted_valorm2","ln_valorm2","dormitorios","suites","vagas","banheiros")])



