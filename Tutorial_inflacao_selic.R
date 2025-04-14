# =============================================================================
# Economia com Pierre 📊
# Juros altos atraem investimento estrangeiro?
# Tutorial: Baixando dados do Banco Central e analisando!
# Data: 14/04/2025
# =============================================================================

# Instale os pacotes se não tiver
install.packages("ggplot2")
install.packages("dplyr")
install.packages("rbc")

library(rbcb)  # Para baixar os dados do Bacen
library(dplyr) # Manipulação dos dados
library(ggplot2) # Visualização dos dados

# Usando a API para baixar os dados

# Taxa Selic mensal (série 4390)
selic <- get_series(
  code = c("Selic" = 4390),
  start_date = "2005-01-01",
  end_date = Sys.Date()
)

# IPCA mensal (série 433)
ipca <- get_series(
  code = c("IPCA" = 433),
  start_date = "2005-01-01",
  end_date = Sys.Date()
)


# Juntar as séries
dados <- selic %>%
  left_join(ipca, by = "date")

# Criar gráfico com escala ajustada
ggplot(dados, aes(x = date)) +
  geom_line(aes(y = Selic , color = "Selic (% a.a.)")) +
  geom_line(aes(y = IPCA / 2, color = "IPCA (escala ajustada)")) + # Aumentei o fator de 5 para 10
  scale_y_continuous(
    name = "Selic (% a.a.)",
    sec.axis = sec_axis(~ . / 2, name = "IPCA (% mensal)") # Ajuste correspondente no eixo secundário
  ) +
  labs(
    title = "Taxa Selic vs IPCA (Mensal, 2005-2025)",
    subtitle = "IPCA ajustado (/2) para melhor visualização",
    x = "Data",
    color = "Série"
  ) +
  theme_minimal()
