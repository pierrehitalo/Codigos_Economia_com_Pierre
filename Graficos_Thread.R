# Thread X: 5 Gráficos com ggplot2 e leaflet (@economia_com_Pierre)
# Autor: Pierre Hitalo
# Data: 12/05/2025

# Pacotes
library(ggplot2)
library(leaflet)

# 1. Gráfico de Dispersão
df_dispersao <- data.frame(PIB = c(5, 6, 7, 8, 6.5, 7.5, 8.5, 9), 
                           Desigualdade = c(0.3, 0.4, 0.35, 0.5, 0.45, 0.38, 0.42, 0.48))
p1 <- ggplot(df_dispersao, aes(x = PIB, y = Desigualdade)) + 
  geom_point(color = '#1f77b4', size = 3) + 
  labs(title = 'PIB vs. Desigualdade', x = 'PIB (R$ bi)', y = 'Índice de Gini') + 
  theme_minimal()
ggsave("dispersao.png", p1, width = 6, height = 4)

# 2. Histograma
df_hist <- data.frame(Salario = c(2, 2.5, 3, 2.8, 3.2, 4, 4.5, 3.8, 2.9, 3.5, 4.2, 3.1))
p2 <- ggplot(df_hist, aes(x = Salario)) + 
  geom_histogram(bins = 6, fill = '#2ca02c', color = 'black') + 
  labs(title = 'Distribuição de Salários', x = 'Salário (R$ mil)', y = 'Frequência') + 
  theme_minimal()
ggsave("histograma.png", p2, width = 6, height = 4)

# 3. Mapa com leaflet
df_mapa <- data.frame(lng = c(-60, -58, -62), lat = c(-3, -4, -2), valor = c(100, 150, 80))
m <- leaflet(df_mapa) %>% 
  addTiles() %>% 
  setView(lng = -60, lat = -3, zoom = 5) %>% 
  addCircles(lng = ~lng, lat = ~lat, radius = ~valor*1000, color = 'red')
# Para salvar como imagem, use screenshot ou: install.packages('webshot2'); webshot2::webshot('mapa.html', 'mapa.png')
# htmlwidgets::saveWidget(m, "mapa.html")

# 4. Gráfico de Barras
df_barras <- data.frame(Regiao = c('Norte', 'Sul', 'Nordeste', 'Sudeste'), 
                        Externalidade = c(120, 80, 100, 90))
p3 <- ggplot(df_barras, aes(x = Regiao, y = Externalidade, fill = Regiao)) + 
  geom_bar(stat = 'identity') + 
  labs(title = 'Externalidades por Região', x = 'Região', y = 'Externalidade (tCO2)') + 
  theme_minimal()
ggsave("barras.png", p3, width = 6, height = 4)

# 5. Gráfico de Série Temporal
df_serie <- data.frame(Ano = 2015:2024, PIB = c(5.9, 6.0, 5.8, 6.2, 6.5, 6.7, 6.4, 6.8, 7.0, 7.2))
p4 <- ggplot(df_serie, aes(x = Ano, y = PIB)) + 
  geom_line(color = '#ff7f0e', size = 1) + 
  geom_point(size = 2) + 
  labs(title = 'Crescimento do PIB', x = 'Ano', y = 'PIB (R$ tri)') + 
  theme_minimal()
ggsave("serie_temporal.png", p4, width = 6, height = 4)

