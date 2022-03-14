#### Bibliotecas ----

# Manipulação dos dados
library(readxl)
library(tidyverse)

# Manipulação de dados e gráficos espaciais com os dados do IBGE
library(sidrar)

# Só utilizar se for a primeira vez que for usar o geobr
#utils::remove.packages('geobr')
#devtools::install_github("ipeaGIT/geobr", subdir = "r-package")

library(geobr)

library(crul)
library(sf)
library(ggspatial)

# Mapa com marcadores
library(leaflet)

library(stringi) # Manipulação de Strings

# Ambiente de trabalho
setwd("C:/Users/Macaubas/Desktop/Python/Labimec/Artesanato/Dados/Limpos")

#### Importando os dados ----
##### a) Demanda Local ----
demanda_local = read_csv("DEMANDA_LOCAL_LIMPO.csv") %>% 
  rename(bairro = `2__BAIRRO_ONDE_MORA`) # Reonmeando nome dos bairros

##### b) Demanda Turista ----
demanda_turista = read_csv("DEMANDA_TURISTA_LIMPO.csv") %>% 
  select(ESTADO_ORIGEM)

##### c) Oferta Local ----
oferta_local =  read_csv("oferta_artesanal_pb_2022_limpo.csv") %>% 
  select(ESTADO_ONDE_NASCEU, ESTADO_ONDE_MOROU)


##### d) Dados geométricos ----

# Bairros de João Pessoa
geometria_bairros = geobr::read_neighborhood(year = 2010) %>% 
  filter(code_muni == 2507507) %>%  # Código do IBGE de João Pessoa
  rename(bairro = name_neighborhood) %>%  # Renomeando nome dos bairros para português
  select(bairro, geom) # Colunas de interesse para análise

# Estados
geometria_estados = geobr::read_state(year = 2010) %>% 
  select(abbrev_state, geom)


#### Transformações ----

##### a) Colocando bairros em letras maiúsculas dos bairros ----
geometria_bairros = data.frame(lapply(geometria_bairros , function(v) {
  if (is.character(v)) return(toupper(v))
  else return(v)
}))


##### b) Retirando acentuação dos bairros ----
geometria_bairros = geometria_bairros  %>%
  mutate(bairro = stri_trans_general(str = bairro, id = "Latin-ASCII"))

##### c) Agrupando informações por bairro e estados ----

# Bairros
bairros = demanda_local %>% 
  group_by(bairro) %>% 
  summarize(QNT = n()) %>%  
  mutate(PERC = QNT*100/sum(QNT),
         PERC = round(PERC,2))

# Estados
estado_nasceu = oferta_local %>% 
  group_by(ESTADO_ONDE_NASCEU) %>% 
  summarize(QNT = n()) %>%  
  mutate(PERC = QNT*100/sum(QNT),
         PERC = round(PERC,2))

estado_morou = oferta_local %>% 
  group_by(ESTADO_ONDE_MOROU) %>% 
  summarize(QNT = n()) %>%  
  mutate(PERC = QNT*100/sum(QNT),
         PERC = round(PERC,2))

estado_origem = demanda_turista %>% 
  group_by(ESTADO_ORIGEM) %>% 
  summarize(QNT = n()) %>%  
  mutate(PERC = QNT*100/sum(QNT),
         PERC = round(PERC,2))

  

#### Merging dos dados ----

##### a) FULL OUTER JOIN ----

# Bairros
dados_bairros = full_join(bairros, geometria_bairros, 
                            by = c("bairro"))

# Estados
dados_estados_nasceu = full_join(estado_nasceu, geometria_estados,
                                 by = c('ESTADO_ONDE_NASCEU' = 'abbrev_state'))


dados_estados_morou = full_join(estado_morou, geometria_estados,
                                 by = c('ESTADO_ONDE_MOROU' = 'abbrev_state'))


dados_estados_origem = full_join(estado_origem, geometria_estados,
                                by = c('ESTADO_ORIGEM' = 'abbrev_state'))


# Removendo do ambiente de trabalho bases não utilizadas
rm(demanda_local,
   demanda_turista,
   oferta_local,
   geometria_bairros,
   geometria_estados,
   bairros,
   estado_morou,
   estado_nasceu,
   estado_origem)


#### DataViz ----

# Personalização do gráfico
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.border = element_blank(),
                 panel.background = element_blank())

tema <- theme(text = element_text(family = 'Avenir')
                 ,panel.grid.major = element_line(color = '#cccccc' 
                                                  ,linetype = 'dashed'
                                                  ,size = .3
                 )
                 ,panel.background = element_rect(fill = 'aliceblue')
                 ,plot.title = element_text(size = 20)
                 ,plot.subtitle = element_text(size = 14)
                 ,axis.title = element_blank()
                 ,axis.text = element_text(size = 10)
)


##### a) Mapa dos bairros ----
mapa_bairros_vivem = ggplot() +
  geom_sf(data=dados_bairros, aes(geometry = geometry, fill=PERC), color = 'black', size=0.15) +
  scale_fill_viridis_c(option = "mako", begin = 0.3, direction = -1) + 
  labs(fill = "Percentual (%)",
       title = "A maioria dos visitantes locais moram na praia",
       subtitle = "Os bairros de Cabo Branco e Bancários são os mais citados",
       caption = "Fonte: Elaboração própria com dados coletados e do IBGE") +
  theme_minimal() +
  no_axis +
  tema + 
  theme(legend.position='left',
        legend.margin=margin(0, 0, 0, 0)); mapa_bairros_vivem

# Salvando gráfico
ggsave("C:/Users/Macaubas/Desktop/Python/Labimec/Artesanato/Imagens/Mapas/bairro_vivem.jpg",  # jpg, png, eps, tex, etc.
       plot = mapa_bairros_vivem, # Usar lastplot() quando não quiser referenciar o objeto ggplot direto
       width = 12, height = 8, 
       units = "in", #  Outras opções c("in", "cm", "mm")
       dpi = 300)


##### b) Mapa onde nasceu ----
mapa_estado_nasceu= ggplot() +
  geom_sf(data=dados_estados_nasceu, aes(geometry = geom, fill=PERC), color = 'black', size=0.15) +
  scale_fill_viridis_c(option = "mako", begin = 0.3, direction = -1) + 
  labs(fill = "Percentual (%)",
       title = "A maioria dos artesãos nasceram na Paraíba",
       subtitle = "Há artesões de todas as regiões do Brasil",
       caption = "Fonte: Elaboração própria com dados coletados e do IBGE") +
  theme_minimal() +
  no_axis +
  tema + 
  theme(legend.position='left',
        legend.margin=margin(0, 0, 0, 0)); mapa_estado_nasceu

# Salvando gráfico
ggsave("C:/Users/Macaubas/Desktop/Python/Labimec/Artesanato/Imagens/Mapas/estados_nasceram.jpg",  # jpg, png, eps, tex, etc.
       plot = mapa_estado_nasceu, # Usar lastplot() quando não quiser referenciar o objeto ggplot direto
       width = 12, height = 8, 
       units = "in", #  Outras opções c("in", "cm", "mm")
       dpi = 300)


##### c) Mapa onde morou ----
mapa_estado_morou = ggplot() +
  geom_sf(data=dados_estados_morou, aes(geometry = geom, fill=PERC), color = 'black', size=0.15) +
  scale_fill_viridis_c(option = "rocket", begin = 0.3, direction = -1) + 
  labs(fill = "Percentual (%)",
       title = "A maioria dos artesãos já moraram em municípios da Paraíba",
       subtitle = "Uma parcela já morou no Norte do País",
       caption = "Fonte: Elaboração própria com dados coletados e do IBGE") +
  theme_minimal() +
  no_axis +
  tema + 
  theme(legend.position='left',
        legend.margin=margin(0, 0, 0, 0)); mapa_estado_morou

# Salvando gráfico
ggsave("C:/Users/Macaubas/Desktop/Python/Labimec/Artesanato/Imagens/Mapas/estados_morou.jpg",  # jpg, png, eps, tex, etc.
       plot = mapa_estado_morou, # Usar lastplot() quando não quiser referenciar o objeto ggplot direto
       width = 12, height = 8, 
       units = "in", #  Outras opções c("in", "cm", "mm")
       dpi = 300)


##### d) Mapa estado origem ----
mapa_estado_origem = ggplot() +
  geom_sf(data=dados_estados_origem, aes(geometry = geom, fill=PERC), color = 'black', size=0.15) +
  scale_fill_viridis_c(option = "rocket", begin = 0.3, direction = -1) + 
  labs(fill = "Percentual (%)",
       title = "A maioria dos turistas entrevistado são da Paraíba, Bahia e São Paulo",
       subtitle = "Há turistas de todas as regiões do Brasil",
       caption = "Fonte: Elaboração própria com dados coletados e do IBGE") +
  theme_minimal() +
  no_axis +
  tema + 
  theme(legend.position='left',
        legend.margin=margin(0, 0, 0, 0)); mapa_estado_origem

# Salvando gráfico
ggsave("C:/Users/Macaubas/Desktop/Python/Labimec/Artesanato/Imagens/Mapas/estados_origem.jpg",  # jpg, png, eps, tex, etc.
       plot = mapa_estado_origem, # Usar lastplot() quando não quiser referenciar o objeto ggplot direto
       width = 12, height = 8, 
       units = "in", #  Outras opções c("in", "cm", "mm")
       dpi = 300)
