library(tidyverse)
library(xts)
library(leaflet)
library(ggmap)

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
  plot(dataset)
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
