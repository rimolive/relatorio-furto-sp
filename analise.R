library(tidyverse)
library(xts)
library(leaflet)
library(ggmap)
library(caret)
library(corrplot)

load('furto.RData')

ggplot(furto.dataset, aes(ANO_BO)) +
  labs(x="Ano", y ="Número de BOs registrados", title="Quantidade de BOs registrados por ano em São Paulo") + 
  geom_bar()

plot_time <- function(dataset, year) {
  #Filtra o dataset de acordo com o ano
  dataset <- dataset %>%
    filter(CIDADE == "S.PAULO") %>%
    filter(ANO_BO == year) %>%
    group_by(DATAOCORRENCIA) %>%
    summarise(QUANTIDADE = n())
  
  dataset$DATAOCORRENCIA <- as.Date(dataset$DATAOCORRENCIA, "%d/%m/%Y")
  dataset <- dataset %>% filter(DATAOCORRENCIA >= as.Date(paste("01/01", year, sep="/"), "%d/%m/%Y"))
  dataset <- xts(dataset$QUANTIDADE, order.by = dataset$DATAOCORRENCIA)
  plot(dataset, ylim=c(0,900))
}

sample_geo_data <- function(dataset, year) {
  dataset <- furto.dataset %>%
    filter(!is.na(LATITUDE)) %>%
    filter(ANO_BO == year)
  
  dataset[sample(nrow(dataset), 500), ]
}

par(mfrow=c(2, 2))
plot_time(furto.dataset, '2014')
plot_time(furto.dataset, '2015')
plot_time(furto.dataset, '2016')
plot_time(furto.dataset, '2017')
  
mapa.2014 <- sample_geo_data(furto.dataset, '2014')
mapa.2015 <- sample_geo_data(furto.dataset, '2015')
mapa.2016 <- sample_geo_data(furto.dataset, '2016')
mapa.2017 <- sample_geo_data(furto.dataset, '2017')

mapa.2014 <- geocode(location=mapa.2014$LOGRADOURO, output="latlon", source="google")

leaflet(na.omit(mapa.2014)) %>%
  addTiles() %>%
  setView(-46.6333824, -23.5506507, zoom=10) %>%
  addMarkers(lng=~lon, lat=~lat)

furto.dataset$DATAOCORRENCIA <- as.Date(furto.dataset$DATAOCORRENCIA, "%d/%m/%Y")

carnaval.2014 <- furto.dataset %>%
  filter(DATAOCORRENCIA >= as.Date("01/03/2014", "%d/%m/%Y")) %>%
  filter(DATAOCORRENCIA <= as.Date("04/03/2014", "%d/%m/%Y")) %>%
  mutate(evento = "Carnaval") %>%
  mutate(publico = 7000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)
parada.gay.2014 <- furto.dataset %>%
  filter(DATAOCORRENCIA == as.Date("04/05/2014", "%d/%m/%Y")) %>%
  mutate(evento = "Parada Gay") %>%
  mutate(publico = 100000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)
virada.cultural.2014 <- furto.dataset %>%
  filter(DATAOCORRENCIA >= as.Date("17/05/2014", "%d/%m/%Y")) %>%
  filter(DATAOCORRENCIA <= as.Date("18/05/2014", "%d/%m/%Y")) %>%
  mutate(evento = "Virada Cultural") %>%
  mutate(publico = 400000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)
marcha.para.jesus.2014 <- furto.dataset %>%
  filter(DATAOCORRENCIA == as.Date("07/06/2014", "%d/%m/%Y")) %>%
  mutate(evento = "Marcha para Jesus") %>%
  mutate(publico = 25000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)
copa.do.mundo.2014 <- furto.dataset %>%
  filter(DATAOCORRENCIA >= as.Date("12/06/2014", "%d/%m/%Y")) %>%
  filter(DATAOCORRENCIA <= as.Date("13/07/2014", "%d/%m/%Y")) %>%
  mutate(evento = "Copa do Mundo") %>%
  mutate(publico = 68000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)

eventos.dataset <- rbind(carnaval.2014, parada.gay.2014, virada.cultural.2014, marcha.para.jesus.2014, copa.do.mundo.2014)
rm(carnaval.2014)
rm(parada.gay.2014)
rm(virada.cultural.2014)
rm(marcha.para.jesus.2014)
rm(copa.do.mundo.2014)

carnaval.2015 <- furto.dataset %>%
  filter(DATAOCORRENCIA >= as.Date("17/02/2015", "%d/%m/%Y")) %>%
  filter(DATAOCORRENCIA <= as.Date("22/02/2015", "%d/%m/%Y")) %>%
  mutate(evento = "Carnaval") %>%
  mutate(publico = 20000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)
parada.gay.2015 <- furto.dataset %>%
  filter(DATAOCORRENCIA == as.Date("07/06/2015", "%d/%m/%Y")) %>%
  mutate(evento = "Parada Gay") %>%
  mutate(publico = 2000000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)
virada.cultural.2015 <- furto.dataset %>%
  filter(DATAOCORRENCIA >= as.Date("20/06/2015", "%d/%m/%Y")) %>%
  filter(DATAOCORRENCIA <= as.Date("21/06/2015", "%d/%m/%Y")) %>%
  mutate(evento = "Virada Cultural") %>%
  mutate(publico = NA) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)
marcha.para.jesus.2015 <- furto.dataset %>%
  filter(DATAOCORRENCIA == as.Date("26/05/2015", "%d/%m/%Y")) %>%
  mutate(evento = "Marcha para Jesus") %>%
  mutate(publico = 340000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)

eventos.dataset <- rbind(eventos.dataset, carnaval.2015, parada.gay.2015, virada.cultural.2015, marcha.para.jesus.2015)
rm(carnaval.2015)
rm(parada.gay.2015)
rm(virada.cultural.2015)
rm(marcha.para.jesus.2015)

carnaval.2016 <- furto.dataset %>%
  filter(DATAOCORRENCIA >= as.Date("06/02/2016", "%d/%m/%Y")) %>%
  filter(DATAOCORRENCIA <= as.Date("09/02/2016", "%d/%m/%Y")) %>%
  mutate(evento = "Carnaval") %>%
  mutate(publico = 3000000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)
parada.gay.2016 <- furto.dataset %>%
  filter(DATAOCORRENCIA == as.Date("29/05/2016", "%d/%m/%Y")) %>%
  mutate(evento = "Parada Gay") %>%
  mutate(publico = 2000000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)
virada.cultural.2016 <- furto.dataset %>%
  filter(DATAOCORRENCIA >= as.Date("20/05/2016", "%d/%m/%Y")) %>%
  filter(DATAOCORRENCIA <= as.Date("22/05/2016", "%d/%m/%Y")) %>%
  mutate(evento = "Virada Cultural") %>%
  mutate(publico = 1100000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)
marcha.para.jesus.2016 <- furto.dataset %>%
  filter(DATAOCORRENCIA == as.Date("26/05/2016", "%d/%m/%Y")) %>%
  mutate(evento = "Marcha para Jesus") %>%
  mutate(publico = 340000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)

eventos.dataset <- rbind(eventos.dataset, carnaval.2016, parada.gay.2016, virada.cultural.2016, marcha.para.jesus.2016)
rm(carnaval.2016)
rm(parada.gay.2016)
rm(virada.cultural.2016)
rm(marcha.para.jesus.2016)

carnaval.2017 <- furto.dataset %>%
  filter(DATAOCORRENCIA >= as.Date("25/02/2017", "%d/%m/%Y")) %>%
  filter(DATAOCORRENCIA <= as.Date("28/02/2017", "%d/%m/%Y")) %>%
  mutate(evento = "Carnaval") %>%
  mutate(publico = 9000000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)
parada.gay.2017 <- furto.dataset %>%
  filter(DATAOCORRENCIA == as.Date("18/06/2017", "%d/%m/%Y")) %>%
  mutate(evento = "Parada Gay") %>%
  mutate(publico = 3000000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)
virada.cultural.2017 <- furto.dataset %>%
  filter(DATAOCORRENCIA >= as.Date("20/05/2017", "%d/%m/%Y")) %>%
  filter(DATAOCORRENCIA <= as.Date("21/05/2017", "%d/%m/%Y")) %>%
  mutate(evento = "Virada Cultural") %>%
  mutate(publico = 1600000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)
marcha.para.jesus.2017 <- furto.dataset %>%
  filter(DATAOCORRENCIA == as.Date("15/06/2017", "%d/%m/%Y")) %>%
  mutate(evento = "Marcha para Jesus") %>%
  mutate(publico = 2000000) %>%
  select(ANO_BO, DATAOCORRENCIA, evento, publico)

eventos.dataset <- rbind(eventos.dataset, carnaval.2017, parada.gay.2017, virada.cultural.2017, marcha.para.jesus.2017)
rm(carnaval.2017)
rm(parada.gay.2017)
rm(virada.cultural.2017)
rm(marcha.para.jesus.2017)

names(eventos.dataset) <- c("Ano", "Data", "Evento", "Publico")

eventos <- eventos.dataset %>%
  group_by(Ano, Evento, Publico) %>%
  summarise(Ocorrencias = n())

rm(eventos.dataset)

index <- createDataPartition(eventos$Evento,
                             p=0.8,
                             list=FALSE,
                             times=1)

eventos[is.na(eventos$Publico), ]$Publico <- 1000000
eventos.train <- eventos[index, ]
eventos.test <- eventos[-index, ]

fitControl <- trainControl(method="cv",
             number=10)

set.seed(849)
lmFit <- train(Publico ~ .,
               data=eventos.train,
               method="lm",
               trControl=fitControl,
               verbose=FALSE)

pred <- predict(lmFit, eventos.test)
p <- c(pred[1], pred[2], pred[3], pred[4])
prediction <- data.frame(ano = eventos.test$Ano,
                         evento = eventos.test$Evento,
                         publico = eventos.test$Publico,
                         previsto = p)
prediction
corrplot(cor(eventos[, -2]))

save.image("furto.RData")
