# =============================================================================
# Economia com Pierre 📊
# Ouro ou S&P 500: Quem rendeu mais nos últimos 25 anos?
# Tutorial: Baixando dados do ouro e S&P500
# Data: 15/04/2025
# =============================================================================

# Instalar pacotes, se necessário
install.packages(c("quantmod", "ggplot2", "dplyr", "tidyr"))

# Carregar bibliotecas
library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr) 

# Definir período
start_date <- "2020-01-01"
end_date <- Sys.Date()

# Extrair dados do ouro (GC=F) e S&P 500 (^GSPC)
getSymbols("GC=F", src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)
getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = TRUE)

# Selecionar preços de fechamento ajustados
gold <- Cl(`GC=F`)  # Preço de fechamento do ouro
sp500 <- Cl(`GSPC`)  # Preço de fechamento do S&P 500

# Converter para data frame e juntar
data <- merge.xts(gold, sp500, join = "inner")
data_df <- data.frame(
  date = index(data),
  Gold = as.numeric(data[, 1]),
  SP500 = as.numeric(data[, 2])
)

# Normalizar os preços (para comparar o crescimento relativo)
# Dividimos todos os valores pelo primeiro preço (base 100)
data_df <- data_df %>%
  mutate(
    Gold_Norm = 100 * Gold / first(Gold),
    SP500_Norm = 100 * SP500 / first(SP500)
  )

# Transformar para formato longo (para ggplot)
data_long <- data_df %>%
  select(date, Gold_Norm, SP500_Norm) %>%
  pivot_longer(cols = c(Gold_Norm, SP500_Norm), names_to = "Series", values_to = "Value")

# Criar gráfico comparativo
ggplot(data_long, aes(x = date, y = Value, color = Series)) +
  geom_line() +
  scale_color_manual(values = c("Gold_Norm" = "gold", "SP500_Norm" = "blue"),
                     labels = c("Ouro", "S&P 500")) +
  labs(
    title = "Ouro vs S&P 500: Crescimento Normalizado (2020-2025)",
    subtitle = "Base 100 em 01/01/2020",
    x = "Data",
    y = "Crescimento (Base 100)",
    color = "Série"
  ) +
  theme_minimal()
