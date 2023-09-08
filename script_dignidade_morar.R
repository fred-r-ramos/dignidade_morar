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
LOC_dormbanheiro<- LOCpciadbv_valid %>% filter(dormitorios_num< 1 & banheiros>4)

LOCpciadbv_valid <- LOCpciadbv_valid%>% mutate (limp_1 = dormitorios_num< 1 & banheiros>4)
LOCpciadbv_valid <- LOCpciadbv_valid%>% mutate (limp_2 = dormitorios_num< 1 & vagas>5)

LOCfinal_valid <- LOCpciadbv_valid%>% filter (limp_1!="TRUE" & limp_2!="TRUE")
count(LOCfinal_valid)
LOCfinal_valid <-LOCfinal_valid %>% select(-limp_1 & -limp_2)

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
#Mapa com de valores por m2

library(leaflet)
pal <- colorQuantile(c("lightgreen", "darkblue"), domain = LOCfinal_valid_sample$valorm2_r, n = 5, alpha = 0.5)

# Criar o mapa com os limites dos municípios

map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = diadema, color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE)) %>%
  addCircleMarkers(data = LOCfinal_valid_sample, lat = ~latitude, lng = ~longitude,
                   color = ~pal(valorm2_r), radius = 2, stroke = FALSE, fillOpacity = 0.8)

#Adicionar a legenda após criar o mapa
map <- addLegend(map, position = "bottomright", pal = pal, values = LOCfinal_valid_sample$valorm2_r,
                 title = "Valor do aluguel por m2", opacity = 0.8)

map

#Novo mapa com legenda de preços quando aponto o cursor

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

count(LOCfinal_valid_sf_inside) #ficamos com uma amostra de 239271 observacoes

#vizualizar esta amostra no mapa
names(LOCfinal_valid_sf_inside)
plot(LOCfinal_valid_sf_inside[41])
summary(LOCfinal_valid_sf_inside[41])
hist(LOCfinal_valid_sf_inside$precom2_r)

#Estimar valor hedônico para amostras dentro do buffer
LOCfinal_valid_sf_inside <- LOCfinal_valid_sf_inside %>%
  mutate(apart = case_when(tipo_imovel == "APARTAMENTO" ~ 1, TRUE ~ 0))
LOCfinal_valid_sf_inside <- LOCfinal_valid_sf_inside %>%
  mutate(ln_valorm2 = log(valorm2_r))

names(LOCfinal_valid_sf_inside)

hedonic_inside <- lm(valorm2 ~ area_util + +apart + dormitorios + suites + andar + vagas + banheiros, data = LOCfinal_valid_sf_inside)

# Verificar os resultados da regressão
summary(hedonic_inside)

# Fazer previsões com o modelo
LOCfinal_valid_sf_inside$predicted_valorm2 <- predict(hedonic, newdata = LOCfinal_valid_sf_inside)

# Comparar preços reais com preços previstos
ggplot(data = LOCfinal_valid_sf_inside, aes(x = valorm2_r, y = predicted_valorm2)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Comparação entre Preços Reais e Previstos",
       x = "Preço Real (valorm2)",
       y = "Preço Previsto")


#teste de significância do modelo
resultado_anova <- anova(hedonic_inside)
print(resultado_anova)

#teste de multicolinearidade (VIF), apresenta alguma multicolinearidade em área e banbheiros, naturalmente
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
plot(LOCfinal_valid_sf_inside_filter$precom2_r)
map2 <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = diadema, color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2,bringToFront = FALSE)) %>%
  addCircleMarkers(data = LOCfinal_valid_sf_inside_filter, label = sprintf("%.2f", LOCfinal_valid_sf_inside_filter$valorm2_r),
                   color = ~pal(valorm2_r), radius = 2, stroke = FALSE, fillOpacity = 0.8) %>% 
  addLegend(position = "bottomright", pal = pal, values = LOCfinal_valid_sf_inside_filter$valorm2_r,title = "Valor do aluguel por m2", opacity = 0.8)
map2

library(dplyr)
# Selecionar as colunas relevantes para a identificação de duplicatas e ordenação
cols_for_duplicates <- c('area_util','tipo_imovel','cep','endereco',  'dormitorios', 'banheiros', 'vagas', 'suites', 'andar', 'ano_construcao', 'data_ref','geometry')

# Verificar observações duplicadas com base nas colunas selecionadas
duplicated_rows <- duplicated(LOCfinal_valid_sf_inside[, cols_for_duplicates])

# Gerar IDs únicos para observações duplicadas
LOCfinal_valid_sf_inside <- LOCfinal_valid_sf_inside %>%
  mutate(duplicate_id = cumsum(duplicated_rows)) %>%
  group_by(duplicate_id) %>%
  arrange(data_ref) %>%
  mutate(ID = row_number()) %>%
  ungroup()

# A coluna "ID" conterá os IDs únicos ordenados por data para observações duplicadas
# Verificar as primeiras linhas do conjunto de dados
head(LOCfinal_valid_sf_inside)

# Resumir a coluna "ID"
summary(LOCfinal_valid_sf_inside$ID)