library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(leaflet)
library(maps)
library(lubridate)
library(tidyr)
library(countrycode)

# Dados
dados <- read.csv('hotel_bookings.csv') %>%
  mutate_all(~ifelse(. == 'NULL', NA, .)) %>%
  mutate(
    reservation_status_date = ymd(reservation_status_date),
    anomes = paste0(arrival_date_year, "-", match(arrival_date_month, month.name)),
    country_full = countrycode(country, 'iso3c', 'country.name')
  )

# UI
ui <- navbarPage("Dashboard Hotel Bookings",
                 
                 # Página 1 - Home
                 tabPanel("Home",
                          sidebarLayout(
                            sidebarPanel(
                              # Filtro de data
                              dateInput("start_date", "Data Inicial:", value = min(dados$reservation_status_date)),
                              dateInput("end_date", "Data Final:", value = max(dados$reservation_status_date)),
                              # Variável de interesse (boxplot e medidas)
                              selectInput("boxvar", "Variável:", choices = c("adr", "lead_time", "booking_changes", "stays_in_week_nights", "stays_in_weekend_nights", "adults", "children", "babies")),
                              # Slider para selecionar o número de agências a serem exibidas
                              sliderInput("top_agents", "Top Agências a Exibir:", min = 1, max = n_distinct(dados$agent), value = 10),
                              # Filtro de país
                              selectInput("country_filter", "País:", choices = c("Todos", unique(dados$country_full)), selected = "Todos"),
                              width = 3
                            ),
                            
                            mainPanel(
                              # Exibir as medidas estatísticas
                              fluidRow(
                                column(6, verbatimTextOutput("soma")),
                                column(6, verbatimTextOutput("media")),
                                column(6, verbatimTextOutput("canceladas"))
                              ),
                              # Exibir os gráficos em 3x3
                              fluidRow(
                                column(6, plotlyOutput("pizzaPlot")),
                                column(6, plotlyOutput("linePlot")),
                                column(6, plotOutput("boxPlot")),
                                column(6, plotOutput("barPlot2")),
                                column(6, plotlyOutput("barPlot3")),
                                column(6, leafletOutput("map"))
                              )
                            )
                          )),
                 
                 # Página 2 - City Hotel
                 tabPanel("City Hotel",
                          sidebarLayout(
                            sidebarPanel(
                              # Filtro de data
                              dateInput("start_date_city", "Data Inicial:", value = min(dados$reservation_status_date)),
                              dateInput("end_date_city", "Data Final:", value = max(dados$reservation_status_date)),
                              # Variável de interesse (boxplot e medidas)
                              selectInput("boxvar_city", "Variável:", choices = c("adr", "lead_time", "booking_changes", "stays_in_week_nights", "stays_in_weekend_nights", "adults", "children", "babies")),
                              # Slider para selecionar o número de agências a serem exibidas
                              sliderInput("top_agents_city", "Top Agências a Exibir:", min = 1, max = n_distinct(dados$agent), value = 10),
                              # Filtro de país
                              selectInput("country_filter_city", "País:", choices = c("Todos", unique(dados$country_full)), selected = "Todos"),
                              width = 3
                            ),
                            
                            mainPanel(
                              # Exibir as medidas estatísticas
                              fluidRow(
                                column(6, verbatimTextOutput("soma_city")),
                                column(6, verbatimTextOutput("media_city")),
                                column(6, verbatimTextOutput("canceladas_city"))
                              ),
                              # Exibir os gráficos em 3x3
                              fluidRow(
                                column(6, plotlyOutput("pizzaPlot_city")),
                                column(6, plotlyOutput("linePlot_city")),
                                column(6, plotOutput("boxPlot_city")),
                                column(6, plotOutput("barPlot2_city")),
                                column(6, plotlyOutput("barPlot3_city")),
                                column(6, leafletOutput("map_city"))
                              )
                            )
                          )),
                 
                 # Página 3 - Resort Hotel
                 tabPanel("Resort Hotel",
                          sidebarLayout(
                            sidebarPanel(
                              # Filtro de data
                              dateInput("start_date_resort", "Data Inicial:", value = min(dados$reservation_status_date)),
                              dateInput("end_date_resort", "Data Final:", value = max(dados$reservation_status_date)),
                              # Variável de interesse (boxplot e medidas)
                              selectInput("boxvar_resort", "Variável:", choices = c("adr", "lead_time", "booking_changes", "stays_in_week_nights", "stays_in_weekend_nights", "adults", "children", "babies")),
                              # Slider para selecionar o número de agências a serem exibidas
                              sliderInput("top_agents_resort", "Top Agências a Exibir:", min = 1, max = n_distinct(dados$agent), value = 10),
                              # Filtro de país
                              selectInput("country_filter_resort", "País:", choices = c("Todos", unique(dados$country_full)), selected = "Todos"),
                              width = 3
                            ),
                            
                            mainPanel(
                              # Exibir as medidas estatísticas
                              fluidRow(
                                column(6, verbatimTextOutput("soma_resort")),
                                column(6, verbatimTextOutput("media_resort")),
                                column(6, verbatimTextOutput("canceladas_resort"))
                              ),
                              # Exibir os gráficos em 3x3
                              fluidRow(
                                column(6, plotlyOutput("pizzaPlot_resort")),
                                column(6, plotlyOutput("linePlot_resort")),
                                column(6, plotOutput("boxPlot_resort")),
                                column(6, plotOutput("barPlot2_resort")),
                                column(6, plotlyOutput("barPlot3_resort")),
                                column(6, leafletOutput("map_resort"))
                              )
                            )
                          ))
)

# Servidor
server <- function(input, output, session) {
  
  # Função para filtrar os dados baseado nos inputs
  filtered_data <- reactive({
    data <- dados %>%
      filter(reservation_status_date >= input$start_date & 
               reservation_status_date <= input$end_date)
    if (input$country_filter != "Todos") {
      data <- data %>%
        filter(country_full == input$country_filter)
    }
    data
  })
  
  # Função para filtrar os dados do City Hotel baseado nos inputs
  filtered_data_city <- reactive({
    data <- dados %>%
      filter(reservation_status_date >= input$start_date_city & 
               reservation_status_date <= input$end_date_city)
    if (input$country_filter_city != "Todos") {
      data <- data %>%
        filter(country_full == input$country_filter_city)
    }
    data
  })
  
  # Função para filtrar os dados do Resort Hotel baseado nos inputs
  filtered_data_resort <- reactive({
    data <- dados %>%
      filter(reservation_status_date >= input$start_date_resort & 
               reservation_status_date <= input$end_date_resort)
    if (input$country_filter_resort != "Todos") {
      data <- data %>%
        filter(country_full == input$country_filter_resort)
    }
    data
  })
  
  # Renderização dos gráficos para a página Home
  
  # Média variável selecionada - Geral
  output$media <- renderText({
    var_data <- filtered_data()[[input$boxvar]]
    paste("Média de", input$boxvar, ":", round(mean(var_data), 2))
  })
  
  # Soma variável selecionada - Geral
  output$soma <- renderText({
    var_data <- filtered_data()[[input$boxvar]]
    paste("Total de", input$boxvar, ":", round(sum(var_data), 2))
  })
  
  # Preço médio x data - Geral
  output$linePlot <- renderPlotly({
    data <- filtered_data() %>%
      mutate(anomes = as.Date(paste0(anomes, "-01"))) %>%
      group_by(anomes) %>%
      summarise(media_adr = mean(adr)) %>%
      arrange(anomes)
    
    fig <- plot_ly(data, x = ~anomes, y = ~media_adr, type = 'scatter') %>%
      layout(title = 'Preço Médio por Data',
             xaxis = list(title = 'Data'),
             yaxis = list(title = 'Preço Médio (ADR)'))
    
    fig
  })
  
  # Boxplot - Geral
  output$boxPlot <- renderPlot({
    var_data <- filtered_data()[[input$boxvar]]
    
    ggplot() +
      geom_boxplot(data = data.frame(x = var_data), aes(x = x), fill = "lightblue") +
      labs(title = "Distribuição da Variável Selecionada",
           x = input$boxvar) +
      theme_minimal() +
      theme(axis.text.y = element_blank()) # Oculta os ticks do eixo Y
  })
  
  # Rosca 1 - Geral
  output$pizzaPlot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(reservation_status) %>%
      summarise(soma_mudancas = sum(booking_changes))
    
    fig <- plot_ly(data, labels = ~reservation_status, values = ~soma_mudancas, type = 'pie', 
                   textposition = 'inside', textinfo = 'label+percent',
                   marker = list(colors = c('lightblue', 'steelblue', 'lightgrey')))
    
    fig
  })
  
  # Gráfico de barras para as agências - Geral
  output$barPlot2 <- renderPlot({
    # Contagem de reservas por agências
    reservations_by_agent <- filtered_data() %>%
      filter(!is.na(agent)) %>%  # Removendo valores NA nas agências
      group_by(agent) %>%
      summarise(total_reservations = n())
    
    # Ordenando as agências por número de reservas
    top_agents <- reservations_by_agent %>%
      arrange(desc(total_reservations)) %>%
      slice_head(n = input$top_agents)
    
    ggplot(top_agents, aes(x = reorder(agent, -total_reservations), y = total_reservations)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(title = "Top Agências por Número de Reservas",
           x = "Agências",
           y = "Número de Reservas") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Gráfico de barras para países - Geral
  output$barPlot3 <- renderPlotly({
    reservations_by_country <- filtered_data() %>%
      filter(!is.na(country)) %>%  # Removendo valores NA nos países
      group_by(country_full) %>%
      summarise(total_reservations = n())
    
    top_countries <- reservations_by_country %>%
      arrange(desc(total_reservations)) %>%
      slice_head(n = 10)
    
    fig <- plot_ly(top_countries, x = ~country_full, y = ~total_reservations, type = 'bar', marker = list(color = 'steelblue')) %>%
      layout(title = 'Top 10 Países por Número de Reservas',
             xaxis = list(title = 'Países'),
             yaxis = list(title = 'Número de Reservas'))
    
    fig
  })
  
  # Mapa de calor dos países - Geral
  output$map <- renderLeaflet({
    reservations_by_country <- filtered_data() %>%
      filter(!is.na(country)) %>%  # Removendo valores NA nos países
      group_by(country_full) %>%
      summarise(total_reservations = n())
    
    country_coords <- maps::world.cities %>%
      group_by(country.etc) %>%
      summarise(lat = mean(lat), lon = mean(long)) %>%
      filter(country.etc %in% reservations_by_country$country_full)
    
    country_data <- reservations_by_country %>%
      inner_join(country_coords, by = c("country_full" = "country.etc"))
    
    leaflet(country_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~log(total_reservations + 1) * 3,
        popup = ~paste(country_full, ": ", total_reservations, " reservas"),
        color = 'steelblue',  
        fillColor = ~colorNumeric("BuGn", domain = NULL)(total_reservations),  
        fillOpacity = 0.8,  
        layerId = ~country_full
      )
  })
  
  # Medidas de Cancelamento - Geral
  output$canceladas <- renderText({
    total_reservas <- nrow(filtered_data())
    reservas_canceladas <- filtered_data() %>%
      filter(reservation_status == "Canceled") %>%
      nrow()
    
    paste("Reservas Canceladas: ", reservas_canceladas, " (", round((reservas_canceladas / total_reservas) * 100, 2), "%)", sep = "")
  })
  
  # Renderização dos gráficos para a página City Hotel
  
  # Média variável selecionada - City Hotel
  output$media_city <- renderText({
    var_data <- filtered_data_city()[[input$boxvar_city]]
    paste("Média de", input$boxvar_city, ":", round(mean(var_data), 2))
  })
  
  # Soma variável selecionada - City Hotel
  output$soma_city <- renderText({
    var_data <- filtered_data_city()[[input$boxvar_city]]
    paste("Total de", input$boxvar_city, ":", round(sum(var_data), 2))
  })
  
  # Preço médio x data - City Hotel
  output$linePlot_city <- renderPlotly({
    data <- filtered_data_city() %>%
      mutate(anomes = as.Date(paste0(anomes, "-01"))) %>%
      group_by(anomes) %>%
      summarise(media_adr = mean(adr)) %>%
      arrange(anomes)
    
    fig <- plot_ly(data, x = ~anomes, y = ~media_adr, type = 'scatter') %>%
      layout(title = 'Preço Médio por Data - City Hotel',
             xaxis = list(title = 'Data'),
             yaxis = list(title = 'Preço Médio (ADR)'))
    
    fig
  })
  
  # Boxplot - City Hotel
  output$boxPlot_city <- renderPlot({
    var_data <- filtered_data_city()[[input$boxvar_city]]
    
    ggplot() +
      geom_boxplot(data = data.frame(x = var_data), aes(x = x), fill = "lightblue") +
      labs(title = "Distribuição da Variável Selecionada - City Hotel",
           x = input$boxvar_city) +
      theme_minimal() +
      theme(axis.text.y = element_blank()) # Oculta os ticks do eixo Y
  })
  
  # Rosca 1 - City Hotel
  output$pizzaPlot_city <- renderPlotly({
    data <- filtered_data_city() %>%
      group_by(reservation_status) %>%
      summarise(soma_mudancas = sum(booking_changes))
    
    fig <- plot_ly(data, labels = ~reservation_status, values = ~soma_mudancas, type = 'pie', 
                   textposition = 'inside', textinfo = 'label+percent',
                   marker = list(colors = c('lightblue', 'steelblue', 'lightgrey')))
    
    fig
  })
  
  
  # Gráfico de barras para as agências - City Hotel
  output$barPlot2_city <- renderPlot({
    # Contagem de reservas por agências
    reservations_by_agent <- filtered_data_city() %>%
      filter(!is.na(agent)) %>%  # Removendo valores NA nas agências
      group_by(agent) %>%
      summarise(total_reservations = n())
    
    # Ordenando as agências por número de reservas
    top_agents <- reservations_by_agent %>%
      arrange(desc(total_reservations)) %>%
      slice_head(n = input$top_agents_city)
    
    ggplot(top_agents, aes(x = reorder(agent, -total_reservations), y = total_reservations)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(title = "Top Agências por Número de Reservas - City Hotel",
           x = "Agências",
           y = "Número de Reservas") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Gráfico de barras para países - City Hotel
  output$barPlot3_city <- renderPlotly({
    reservations_by_country <- filtered_data_city() %>%
      filter(!is.na(country)) %>%  # Removendo valores NA nos países
      group_by(country_full) %>%
      summarise(total_reservations = n())
    
    top_countries <- reservations_by_country %>%
      arrange(desc(total_reservations)) %>%
      slice_head(n = 10)
    
    fig <- plot_ly(top_countries, x = ~country_full, y = ~total_reservations, type = 'bar', marker = list(color = 'steelblue')) %>%
      layout(title = 'Top 10 Países por Número de Reservas - City Hotel',
             xaxis = list(title = 'Países'),
             yaxis = list(title = 'Número de Reservas'))
    
    fig
  })
  
  # Mapa de calor dos países - City Hotel
  output$map_city <- renderLeaflet({
    reservations_by_country <- filtered_data_city() %>%
      filter(!is.na(country)) %>%  # Removendo valores NA nos países
      group_by(country_full) %>%
      summarise(total_reservations = n())
    
    country_coords <- maps::world.cities %>%
      group_by(country.etc) %>%
      summarise(lat = mean(lat), lon = mean(long)) %>%
      filter(country.etc %in% reservations_by_country$country_full)
    
    country_data <- reservations_by_country %>%
      inner_join(country_coords, by = c("country_full" = "country.etc"))
    
    leaflet(country_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~log(total_reservations + 1) * 3,
        popup = ~paste(country_full, ": ", total_reservations, " reservas"),
        color = 'steelblue',  
        fillColor = ~colorNumeric("BuGn", domain = NULL)(total_reservations),  
        fillOpacity = 0.8,  
        layerId = ~country_full
      )
    
    
    
    
  })
  
  # Medidas de Cancelamento - City Hotel
  output$canceladas_city <- renderText({
    total_reservas <- nrow(filtered_data_city())
    reservas_canceladas <- filtered_data_city() %>%
      filter(reservation_status == "Canceled") %>%
      nrow()
    
    paste("Reservas Canceladas: ", reservas_canceladas, " (", round((reservas_canceladas / total_reservas) * 100, 2), "%)", sep = "")
  })
  
  # Renderização dos gráficos para a página Resort Hotel
  
  # Média variável selecionada - Resort Hotel
  output$media_resort <- renderText({
    var_data <- filtered_data_resort()[[input$boxvar_resort]]
    paste("Média de", input$boxvar_resort, ":", round(mean(var_data), 2))
  })
  
  # Soma variável selecionada - Resort Hotel
  output$soma_resort <- renderText({
    var_data <- filtered_data_resort()[[input$boxvar_resort]]
    paste("Total de", input$boxvar_resort, ":", round(sum(var_data), 2))
  })
  
  # Preço médio x data - Resort Hotel
  output$linePlot_resort <- renderPlotly({
    data <- filtered_data_resort() %>%
      mutate(anomes = as.Date(paste0(anomes, "-01"))) %>%
      group_by(anomes) %>%
      summarise(media_adr = mean(adr)) %>%
      arrange(anomes)
    
    fig <- plot_ly(data, x = ~anomes, y = ~media_adr, type = 'scatter') %>%
      layout(title = 'Preço Médio por Data - Resort Hotel',
             xaxis = list(title = 'Data'),
             yaxis = list(title = 'Preço Médio (ADR)'))
    
    fig
  })
  
  # Boxplot - Resort Hotel
  output$boxPlot_resort <- renderPlot({
    var_data <- filtered_data_resort()[[input$boxvar_resort]]
    
    ggplot() +
      geom_boxplot(data = data.frame(x = var_data), aes(x = x), fill = "lightblue") +
      labs(title = "Distribuição da Variável Selecionada - Resort Hotel",
           x = input$boxvar_resort) +
      theme_minimal() +
      theme(axis.text.y = element_blank()) # Oculta os ticks do eixo Y
  })
  
  # Rosca 1 - Resort Hotel
  output$pizzaPlot_resort <- renderPlotly({
    data <- filtered_data_resort() %>%
      group_by(reservation_status) %>%
      summarise(soma_mudancas = sum(booking_changes))
    
    fig <- plot_ly(data, labels = ~reservation_status, values = ~soma_mudancas, type = 'pie', 
                   textposition = 'inside', textinfo = 'label+percent',
                   marker = list(colors = c('lightblue', 'steelblue', 'lightgrey')))
    
    fig
  })
  
  # Gráfico de barras para as agências - Resort Hotel
  output$barPlot2_resort <- renderPlot({
    # Contagem de reservas por agências
    reservations_by_agent <- filtered_data_resort() %>%
      filter(!is.na(agent)) %>%  # Removendo valores NA nas agências
      group_by(agent) %>%
      summarise(total_reservations = n())
    
    # Ordenando as agências por número de reservas
    top_agents <- reservations_by_agent %>%
      arrange(desc(total_reservations)) %>%
      slice_head(n = input$top_agents_resort)
    
    ggplot(top_agents, aes(x = reorder(agent, -total_reservations), y = total_reservations)) +
      geom_bar(stat = "identity", fill = "lightblue") +
      labs(title = "Top Agências por Número de Reservas - Resort Hotel",
           x = "Agências",
           y = "Número de Reservas") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Gráfico de barras para países - Resort Hotel
  output$barPlot3_resort <- renderPlotly({
    reservations_by_country <- filtered_data_resort() %>%
      filter(!is.na(country)) %>%  # Removendo valores NA nos países
      group_by(country_full) %>%
      summarise(total_reservations = n())
    
    top_countries <- reservations_by_country %>%
      arrange(desc(total_reservations)) %>%
      slice_head(n = 10)
    
    fig <- plot_ly(top_countries, x = ~country_full, y = ~total_reservations, type = 'bar', marker = list(color = 'steelblue')) %>%
      layout(title = 'Top 10 Países por Número de Reservas - Resort Hotel',
             xaxis = list(title = 'Países'),
             yaxis = list(title = 'Número de Reservas'))
    
    fig
  })
  
  # Mapa de calor dos países - Resort Hotel
  output$map_resort <- renderLeaflet({
    reservations_by_country <- filtered_data_resort() %>%
      filter(!is.na(country)) %>%  # Removendo valores NA nos países
      group_by(country_full) %>%
      summarise(total_reservations = n())
    
    country_coords <- maps::world.cities %>%
      group_by(country.etc) %>%
      summarise(lat = mean(lat), lon = mean(long)) %>%
      filter(country.etc %in% reservations_by_country$country_full)
    
    country_data <- reservations_by_country %>%
      inner_join(country_coords, by = c("country_full" = "country.etc"))
    
    leaflet(country_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~log(total_reservations + 1) * 3,
        popup = ~paste(country_full, ": ", total_reservations, " reservas"),
        color = 'steelblue',  
        fillColor = ~colorNumeric("BuGn", domain = NULL)(total_reservations),  
        fillOpacity = 0.8,  
        layerId = ~country_full
      )
    
    
  })
  
  # Medidas de Cancelamento - Resort Hotel
  output$canceladas_resort <- renderText({
    total_reservas <- nrow(filtered_data_resort())
    reservas_canceladas <- filtered_data_resort() %>%
      filter(reservation_status == "Canceled") %>%
      nrow()
    
    paste("Reservas Canceladas: ", reservas_canceladas, " (", round((reservas_canceladas / total_reservas) * 100, 2), "%)", sep = "")
  })
  
  # Observa eventos de cliques no mapa - Geral
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click)) {
      country <- click$id
      updateSelectInput(session, "country_filter", selected = country)
    }
  })
  
  
  # Observa eventos de cliques no mapa - City Hotel
  observeEvent(input$map_city_marker_click, {
    click <- input$map_city_marker_click
    if (!is.null(click)) {
      country <- click$id
      updateSelectInput(session, "country_filter_city", selected = country)
    }
  })
  
  # Observa eventos de cliques no mapa - Resort Hotel
  observeEvent(input$map_resort_marker_click, {
    click <- input$map_resort_marker_click
    if (!is.null(click)) {
      country <- click$id
      updateSelectInput(session, "country_filter_resort", selected = country)
    }
  })
  
}

# Criação do aplicativo Shiny
shinyApp(ui = ui, server = server)
