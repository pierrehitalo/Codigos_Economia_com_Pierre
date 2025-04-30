# =============================================================================
# Economia com Pierre 📊
# Dashboard 
# Tutorial: Rodando um Dashboard com dados simulados
# Data: 30/04/2025
# =============================================================================

library(shiny)
library(quantmod)
library(ggplot2)
library(plotly)
library(DT)
library(leaflet)

# Definindo o ticker fixo
ticker <- "TSLA"

# Obtendo dados da ação (TSLA) até o dia atual (30/04/2025)
getSymbols(ticker, from = "2024-01-01", to = "2025-04-30", auto.assign = TRUE)

# Preparando os dados
data <- Ad(get(ticker))  # Preços ajustados
data_pct <- diff(data) / lag(data) * 100  # Variação percentual diária
data_vol <- Vo(get(ticker))  # Volume
data_df <- data.frame(Date = index(data), Price = as.numeric(data), Volume = as.numeric(data_vol))
data_pct_df <- data.frame(Date = index(data_pct), Pct_Change = as.numeric(data_pct))

# Dados fictícios de volume por país (simulado)
total_volume <- sum(data_df$Volume, na.rm = TRUE)
geo_data <- data.frame(
  Country = c("United States", "China", "Germany", "Japan", "United Kingdom", "Brazil"),
  Volume = c(total_volume * 0.4, total_volume * 0.25, total_volume * 0.15, total_volume * 0.1, total_volume * 0.07, total_volume * 0.03),
  lat = c(37.0902, 35.8617, 51.1657, 36.2048, 55.3781, -14.2350),
  lng = c(-95.7129, 104.1954, 10.4515, 138.2529, -3.4360, -51.9253)
)

# Definindo a UI (interface do usuário)
ui <- fluidPage(
  # Esse aqui muda o plano de fundo pra azul claro usando CSS
  tags$head(
    tags$style(HTML("
      body {background-color: #e6f2ff;}
      .value-box {
        background-color: #ffffff;
        border-radius: 5px;
        padding: 10px;
        margin: 5px;
        text-align: center;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      .value-box h3 {margin: 0; font-size: 24px;}
      .value-box p {margin: 5px 0 0; font-size: 14px; color: #666;}
    "))
  ),
  
  # Título do dashboard
  titlePanel(paste("Dashboard de", ticker, "- 2024 a 30/04/2025")),
  
  # KPIs no topo
  fluidRow(
    column(3, uiOutput("currentPrice")),
    column(3, uiOutput("dailyChange")),
    column(3, uiOutput("maxPrice")),
    column(3, uiOutput("minPrice"))
  ),
  
  # Gráficos e mapa
  fluidRow(
    column(4, h3("Preço ao Longo do Tempo"), plotlyOutput("linePlot")),
    column(4, h3("Variação Diária"), plotlyOutput("piePlot")),
    column(4, h3("Volume por Mês"), plotOutput("barPlot"))
  ),
  fluidRow(
    column(6, h3("Volume por Região"), plotOutput("mapPlot")),
    column(6, h3("Top 5 Dias de Variação"), DTOutput("rankTable"))
  ),
  
  # Mapa mundial com Leaflet
  fluidRow(
    column(12, h3("Distribuição Global de Volume de Negociação"), leafletOutput("worldMap", height = 400))
  )
)

# Definindo o Server (lógica do backend)
server <- function(input, output, session) {
  # Função para criar caixas de valor (KPIs) usando HTML
  output$currentPrice <- renderUI({
    HTML(paste0(
      '<div class="value-box" style="background-color: #4e73df; color: white;">
        <h3>$', round(tail(data_df$Price, 1), 2), '</h3>
        <p>Preço Atual - ', ticker, '</p>
      </div>'
    ))
  })
  
  output$dailyChange <- renderUI({
    change <- tail(data_pct_df$Pct_Change, 1)
    color <- if (change >= 0) "#1cc88a" else "#e74a3b"
    HTML(paste0(
      '<div class="value-box" style="background-color: ', color, '; color: white;">
        <h3>', round(change, 2), '%</h3>
        <p>Variação Diária - ', ticker, '</p>
      </div>'
    ))
  })
  
  output$maxPrice <- renderUI({
    HTML(paste0(
      '<div class="value-box" style="background-color: #6f42c1; color: white;">
        <h3>$', round(max(data_df$Price), 2), '</h3>
        <p>Máximo 2024-2025 - ', ticker, '</p>
      </div>'
    ))
  })
  
  output$minPrice <- renderUI({
    HTML(paste0(
      '<div class="value-box" style="background-color: #fd7e14; color: white;">
        <h3>$', round(min(data_df$Price), 2), '</h3>
        <p>Mínimo 2024-2025 - ', ticker, '</p>
      </div>'
    ))
  })
  
  # Esse aqui cria o gráfico de linha com Plotly
  output$linePlot <- renderPlotly({
    plot_ly(data_df, x = ~Date, y = ~Price, type = "scatter", mode = "lines", line = list(color = "blue")) %>%
      layout(title = paste("Preço de", ticker, "ao Longo do Tempo"),
             xaxis = list(title = "Data"),
             yaxis = list(title = "Preço (USD)"))
  })
  
  # Esse aqui cria o gráfico de pizza com Plotly
  output$piePlot <- renderPlotly({
    pct_change <- na.omit(data_pct_df$Pct_Change)
    pct_pos <- sum(pct_change > 0, na.rm = TRUE)
    pct_neg <- sum(pct_change < 0, na.rm = TRUE)
    pie_data <- data.frame(
      Category = c("Positiva", "Negativa"),
      Count = c(pct_pos, pct_neg)
    )
    plot_ly(pie_data, labels = ~Category, values = ~Count, type = "pie",
            marker = list(colors = c("green", "red"))) %>%
      layout(title = paste("Distribuição de Variações Diárias -", ticker))
  })
  
  # Esse aqui cria o gráfico de barras
  output$barPlot <- renderPlot({
    data_df$Month <- format(data_df$Date, "%Y-%m")
    monthly_vol <- aggregate(Volume ~ Month, data = data_df, sum)
    ggplot(monthly_vol, aes(x = Month, y = Volume)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = paste("Volume de Negociação por Mês -", ticker), x = "Mês", y = "Volume") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Esse aqui cria um mapa simples (simulado) com barras
  output$mapPlot <- renderPlot({
    ggplot(geo_data, aes(x = Country, y = Volume, fill = Country)) +
      geom_bar(stat = "identity") +
      labs(title = paste("Volume por Região -", ticker), x = "Região", y = "Volume") +
      theme_minimal() +
      scale_fill_manual(values = c("United States" = "green", "China" = "blue", "Germany" = "red", 
                                   "Japan" = "purple", "United Kingdom" = "orange", "Brazil" = "gray"))
  })
  
  # Esse aqui cria a tabela de ranking
  output$rankTable <- renderDT({
    data_pct_df$Abs_Change <- abs(data_pct_df$Pct_Change)
    top_days <- head(data_pct_df[order(-data_pct_df$Abs_Change), ], 5)
    datatable(top_days, options = list(pageLength = 5))
  })
  
  # Esse aqui cria o mapa mundial com Leaflet
  output$worldMap <- renderLeaflet({
    # Definindo uma paleta de cores em tons de azul com escala ajustada
    pal <- colorNumeric(
      palette = c("#e6f2ff", "#4e73df"),  # De azul claro a azul escuro
      domain = geo_data$Volume,
      n = 10  # Mais gradações pra destacar variações menores
    )
    
    leaflet(data = geo_data) %>%
      addTiles() %>%
      addCircles(
        lng = ~lng, lat = ~lat,
        radius = ~Volume / max(geo_data$Volume) * 200000,  # Escala reduzida pra pintar mais
        color = ~pal(Volume),
        fillOpacity = 0.7,
        popup = ~paste(Country, ": ", format(Volume, big.mark = ","), " ações")
      ) %>%
      addLegend(
        pal = pal, values = ~Volume,
        title = paste("Volume de", ticker),
        position = "bottomright"
      )
  })
}

# Esse aqui roda o aplicativo Shiny
shinyApp(ui = ui, server = server)
