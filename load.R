library(dplyr)
library(rgdal)
library(ggplot2)

# dados_aqui <- 'https://drive.google.com/folderview?id=0ByKsqUnItyBhU2RmdUloTnJGRGM&usp=sharing'
# system('iconv ESTB2013.txt -f "WINDOWS-1252" -t "UTF-8" > data/ESTB2013_conv.txt')

dados_estb_2013 <- readRDS('data/estb_2013.rds')
mapa <- readOGR(dsn = 'data/SP/35MUE250GC_SIR.shp', layer = '35MUE250GC_SIR')

qtd_empresas <- dados_estb_2013 %>%
  select(cod_municipio = `Município`) %>%
  mutate(cod_municipio = as.character(cod_municipio)) %>%
  count(cod_municipio) %>%
  ungroup %>%
  mutate(faixa = cut(n, c(0, 100, 1000, 5000, 10000, Inf), dig.lab = 50))

mapa@data <- mapa@data %>%
  mutate(cod_municipio = as.character(substr(CD_GEOCODM, 1, 6))) %>%
  inner_join(qtd_empresas, 'cod_municipio') %>%
  mutate(ID = as.character(ID))

d_mapa <- fortify(mapa, region = 'cod_municipio') %>%
  tbl_df %>%
  mutate(cod_municipio = as.character(id)) %>%
  inner_join(mapa@data, 'cod_municipio')

sao_paulo <- d_mapa %>%
  distinct(cod_municipio) %>%
  filter(NM_MUNICIP == 'SÃO PAULO') %>%
  select(cod_municipio, NM_MUNICIP, n, long, lat)

ggplot() +
  geom_map(aes(x = long, y = lat, map_id = id, fill = faixa),
           map=d_mapa, colour='black', size=.1, data=d_mapa) +
  coord_equal() +
  theme_bw()

d_mapa %>%
  distinct(cod_municipio) %>%
  arrange(desc(n)) %>%
  select(NM_MUNICIP, n) %>%
  slice(1:15) %>%
  (knitr::kable)
