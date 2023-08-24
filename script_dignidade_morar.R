library(tidyverse)

setwd( "G:/Meu Drive/DOCS/Urbana_habitação/Dignidade_Morar/dignidade_morar")

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

#limpando preco_m2=na e valores numéricos
LOCm2_clean <- LOCACAOm2_sem_duplicadas[!is.na(LOCACAOm2_sem_duplicadas$preco_m2), ]
LOCm2_clean$preco_m2_num<- as.numeric (LOCm2_clean$preco_m2)
count(LOCm2_clean)

#histograma percentil de preco_m2 
precom2_decile <- quantile(LOCm2_clean$preco_m2, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
print(precom2_decile)
hist(precom2_decile, breaks = precom2_decile,main ="Histograma de preço/m2", xlab = "Preço/m2")

#histograma extremos de preco_m2 
LOCm2_clean_num <-as.numeric(LOCm2_clean$preco_m2)
custom_breaks <- c(0,5,10,20,40,80,160,Inf)
breaks <-cut(LOCm2_clean_num, breaks=custom_breaks, right= TRUE, include.lowest= TRUE, stringsAsFactors = FALSE, na.rm = TRUE)
freq_table<-table(breaks)
barplot(freq_table, main ="Histograma de preço/m2", xlab = "Preço/m2", ylab = "Frequência", col = "blue", border = "black")

LOC_preco <- LOCm2_clean %>% group_by(preco_m2_num<5,preco_m2_num>160, na.rm = TRUE)%>%
  summarise(total_count=n(),
            .groups = 'drop')

#gerando base 5<preço_m2<160
LOCp_valid<- LOCm2_clean %>% filter(preco_m2_num>=5 & preco_m2_num<=160)

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
custom_breaks <- c(0,5,10,20,40,Inf)
breaks <-cut(LOCm2_clean_condominio_num, breaks=custom_breaks, right= TRUE, include.lowest= TRUE, stringsAsFactors = FALSE, na.rm = TRUE)
freq_table<-table(breaks)
barplot(freq_table, main ="Histograma de condomínio/m2", xlab = "Condomínio/m2", ylab = "Frequência", col = "blue", border = "black")

LOC_condominio <- LOCp_valid%>% group_by(condominio_m2<5,condominio_m2>40, na.rm = TRUE)%>%
  summarise(total_count=n(),
            .groups = 'drop')

#gerando base 5<preço_m2<160 e condomínio<80
LOCpc_valid<- LOCp_valid %>% filter(condominio_m2<=40 | is.na (condominio_m2))


#limpando iptu_m2=na e valores numéricos
LOCpc_valid$iptu_m2_num<- as.numeric (LOCpc_valid$iptu_m2)
count(LOCpc_valid)

#histograma percentil de iptu_m2
iptum2_decile <- quantile(LOCpc_valid$iptu_m2, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
print(iptum2_decile)
hist(iptum2_decile, breaks = iptum2_decile,main ="Histograma de IPTU/m2", xlab = "IPTU/m2",ylim = c(0, 1))

#histograma extremos de iptu_m2 
LOCm2_clean_iptu_num<-as.numeric(LOCp_valid$iptu_m2)
custom_breaks <- c(0,1,2,4,8,16,32,Inf)
breaks <-cut(LOCm2_clean_condominio_num, breaks=custom_breaks, right= TRUE, include.lowest= TRUE, stringsAsFactors = FALSE)
freq_table<-table(breaks)
barplot(freq_table, main ="Histograma de IPTU/m2", xlab = "IPTU/m2", ylab = "Frequência", col = "blue", border = "black")

LOC_iptu <- LOCpc_valid %>% group_by(iptu_m2<1,iptu_m2>32, na.rm = TRUE)%>%
  summarise(total_count=n(),
            .groups = 'drop')

#gerando base 5<preço_m2<160 e condomínio<80 e iptu<32
LOCpci_valid<- LOCpc_valid %>% filter(iptu_m2<=32 |is.na (iptu_m2 ))


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

#gerando base 5<preço_m2<160 e condomínio<80 e iptu<32 e área útil<800m2
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

LOC_dorm <- LOCpcia_valid %>% group_by(dormitorios_num<1,dormitorios_num>9)%>%
  summarise(total_count=n(),
            .groups = 'drop')

#gerando base 5<preço_m2<160 e condomínio<80 e iptu<32 e área útil<800m2 e dormitório<10
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
custom_breaks <- c(0,1,2,4,8,12,Inf)
breaks <-cut(LOCm2_clean_banheiros_num, breaks=custom_breaks, right= TRUE, include.lowest= TRUE, stringsAsFactors = FALSE)
freq_table<-table(breaks)
barplot(freq_table, main ="Histograma de  Banheiros", xlab = "Banheiros", ylab = "Frequência", col = "blue", border = "black")

LOC_banh <- LOCpciad_valid %>% group_by(banheiros>11)%>%
  summarise(total_count=n(),
            .groups = 'drop')

#gerando base 5<preço_m2<160 e condomínio<80 e iptu<32 e área útil<800m2 e dormitório<10
LOCpciadb_valid<- LOCpciad_valid %>% filter(banheiros<12 |is.na (banheiros ))

#limpando vagas=na e valores numéricos
LOCpciadb_valid$vagas_num<- as.numeric (LOCpciadb_valid$vagas)
count(LOCpciadb_valid)

#histograma percentil de vagas
vagas_decile <- quantile(LOCpciadb_valid$vagas, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE)
print(vagas_decile)
hist(vagas_decile, breaks = vagas_decile,main ="Histograma de Vagas de Garagem", xlab = "Vagas",ylim = c(0, 1))

#histograma extremos de vagas 
LOCm2_clean_vagas_num<-as.numeric(LOCpciadb_valid$vagas)
custom_breaks <- c(0,1,2,4,8,16,32,Inf)
breaks <-cut(LOCm2_clean_vagas_num, breaks=custom_breaks, right= TRUE, include.lowest= TRUE, stringsAsFactors = FALSE)
freq_table<-table(breaks)
barplot(freq_table, main ="Histograma de Vagas de garagem", xlab = "Área Vagas", ylab = "Frequência", col = "blue", border = "black")

LOC_vagas <- LOCpciadb_valid %>% group_by(vagas<10, vagas>32)%>%
  summarise(total_count=n(),
            .groups = 'drop')

#gerando base 5<preço_m2<160 e condomínio<80 e iptu<32 e área útil<800m2 e dormitório<10
LOCpciadbv_valid<- LOCpciadb_valid %>% filter(vagas<32 |is.na (vagas ))


#Filtros cruzados de area_util e vagas
LOC_areavagas<- LOCpciadbv_valid %>% filter(area_util_num>800 & vagas>32)
LOC_dormvagas<- LOCpciadbv_valid %>% filter(dormitorios_num< 1 & vagas>20)
LOC_dormbanheiro<- LOCpciadbv_valid %>% filter(dormitorios_num< 1 & banheiros>5)

LOCpciadbv_valid <- LOCpciadbv_valid%>% mutate (limp_1 = dormitorios_num< 1 & banheiros>5)
LOCpciadbv_valid <- LOCpciadbv_valid%>% mutate (limp_2 = dormitorios_num< 1 & vagas>20)

LOCfinal_valid <- LOCpciadbv_valid%>% filter (limp_1!="TRUE" & limp_2!="TRUE")
count(LOCfinal_valid)
LOCfinal_valid <-LOCfinal_valid %>% select(-limp_1 & -limp_2)

#Locacao por tipo e ano
LOCACAO_anotipo <- LOCfinal_valid %>% group_by(tipo_imovel, Ano) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

#Gerar soma preco+IPTU+concomínio
LOCfinal_valid <- LOCfinal_valid %>%
  mutate(condominio_m2 = ifelse(is.na(condominio_m2), 0, condominio_m2))
LOCfinal_valid <- LOCfinal_valid %>%
  mutate(iptu_m2 = ifelse(is.na(iptu_m2), 0, iptu_m2))

LOCfinal_valid <- LOCfinal_valid%>% mutate (valorm2 = iptu_m2+preco_m2+condominio_m2)


#deflacionar valores
library(readxl)
FipeZap <- read_excel("G:/Meu Drive/DOCS/Urbana_habitação/Dignidade_Morar/FipeZap.xlsx")
view(FipeZap)

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

ggplot(sum_valor, aes(x = Ano, y = tipo_imovel, size = media_valor)) +
  geom_point()+
  labs(x = "Ano", y = "Tipo de imóvel", size= "Média valor/m2", title = "Valor/m2 do aluguel+IPTU+Condomínio por ano e tipo de imóvel" ) +
  scale_size_continuous(range = c(5, 15))  # Adjust the range for bubble sizes



#gerando uma subamostra pra gerar o mapa
LOCfinal_valid_sample <- LOCfinal_valid %>% sample_frac(size = 0.05)

#instalando pacotes para gerar os mapas
library(leaflet)
library(sf)
library(geobr)

muni <- read_municipality(code_muni = 3513801,year=2010,showProgress = FALSE,simplified = FALSE)

leaflet() %>% addProviderTiles("CartoDB.Positron") %>%  
  addPolygons(data=muni , color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE))

leaflet() %>% addProviderTiles("CartoDB.Positron") %>% 
  addCircleMarkers(data=LOCfinal_valid_sample,lat=LOCfinal_valid_sample$latitude,lng=LOCfinal_valid_sample$longitude,color = "blue", radius = 0.01) %>% 
  addPolygons(data=muni , color = "red", weight = 2, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0,
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront=TRUE))