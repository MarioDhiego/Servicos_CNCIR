# Carregar Bibliotecas
library(shiny)
library(shinydashboard)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)

# Carregar Base de Dados
dados <- read_excel("Banco_CNCIR.xlsx")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "PAINEL DAS CIRETRAN'S"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("PAINEL GERAL", tabName = "geral", icon = icon("chart-bar")),
      menuItem("PAINEL DE FILTROS", tabName = "filtros", icon = icon("filter"))
    )
  ),
  
dashboardBody(
  tabItems(
# ABA 1: GERAL
tabItem(tabName = "geral",
        fluidRow(
          box(title = "TIPOS DE SERVIÇOS DE CNH", width = 7, plotlyOutput("grafico_total_servicos", height = "500px")),
          box(title = "TABELA DE SERRVIÇOS POR MUNICÍPIOS X MESES", width = 5,DTOutput("tabela_cruzada")),
          box(title = "REGISTROS POR MESES", width = 7, plotlyOutput("grafico_mensal_geral", height = "500px")),
          box(title = "TABELA DE REGISTROS POR MUNICÍIOS X MESES", width = 5, DTOutput("tabela_mensal_geral")),
          box(title = "TIPIFICAÇÃO DA CNH", width = 7, plotlyOutput("grafico_tipo_cnh_geral", height = "500px"))
          )
        ),
      
# ABA 2: COM FILTROS
tabItem(tabName = "filtros",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE, title = "Filtros",
              fluidRow(column(2,
                              selectInput("filtro_ano", "Selecione o Ano:",
                                          choices = sort(unique(dados$Ano)), selected = unique(dados$Ano))),
                      column(2,
                             selectInput("filtro_mes", "Selecione o Mês:",
                                         choices = sort(unique(dados$Mes)), selected = unique(dados$Mes))),
                      column(2,
                             selectInput("filtro_municipio", "Selecione o Município:",
                                         choices = sort(unique(dados$Municipio)), selected = unique(dados$Municipio))
                      )
                    )
                )
              ),
              fluidRow(
                box(title = "Frequência de Registros por Mês (Filtrado)", width = 10, 
                    plotlyOutput("grafico_mensal_filtros", height = "500px")),
                box(title = "Total de Serviços de CNH (Filtrado)", width = 10, 
                    plotlyOutput("grafico_total_servicos_filtros", height = "500px")),
                box(title = "Total por Tipo de CNH (Filtrado)", width = 10, 
                    plotlyOutput("grafico_tipo_cnh_filtros", height = "500px"))
              )
      )
    )
  )
)
# SERVER
server <- function(input, output, session) {
#============================= ABA 1: GERAL ===================================#
# Gráfico por Serviços
  output$grafico_total_servicos <- renderPlotly({
    df_total <- dados %>%
      select(Renovacao, Mudanca_Categoria, `1_CNH`, Reabilitacao, `2Via_CNH`, CNH_Definitiva) %>%
      summarise(across(everything(), sum, na.rm = TRUE)) %>%
      pivot_longer(everything(), names_to = "Servico", values_to = "Total") %>%
      mutate(
        Percentual = Total / sum(Total) * 100,
        Label = paste0(format(Total, big.mark = "."), "\n(", round(Percentual, 1), "%)")
      ) %>%
      arrange(Total)
    
    p1 <- ggplot(df_total, aes(x = reorder(Servico, -Percentual), y = Total,
                               text = paste0("Serviço: ", Servico, 
                                             "<br>Total: ", format(Total, big.mark = "."), 
                                             "<br>Percentual: ", round(Percentual, 1), "%"))) +
      geom_col(fill = "royalblue") +
      geom_text(
        aes(label = paste0(format(Total, big.mark = "."), "\n(", round(Percentual, 1), "%)")),
        position = position_stack(vjust = 0.5),
        color = "white", size = 4
      ) +
      scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
      labs(x = "Serviço", y = "Quantidade Total") +
      theme_minimal()
    
    ggplotly(p1, tooltip = "text")
  })
  
# Tabela Município x Tipo_CNH
  output$tabela_cruzada <- renderDT({
    dados %>%
      mutate(across(c(Renovacao, Mudanca_Categoria, `1_CNH`, Reabilitacao, `2Via_CNH`, CNH_Definitiva), ~replace_na(., 0))) %>%
      group_by(Municipio, Tipo_CNH) %>%
      summarise(
        Total_Servicos = sum(Renovacao + Mudanca_Categoria + `1_CNH` + Reabilitacao + `2Via_CNH` + CNH_Definitiva, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Total_Servicos)) %>%
      datatable(
        rownames = FALSE,
        options = list(pageLength = 10, autoWidth = TRUE),
        colnames = c("Município", "Tipo CNH", "Total de Serviços")
      )
  })  
#==============================================================================#

#==============================================================================#
# Gráfico por Meses  
output$grafico_mensal_geral <- renderPlotly({
    meses_ordenados <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                         "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
df_mes <- dados %>%
      filter(Mes %in% meses_ordenados) %>%
      group_by(Mes) %>%
      summarise(Total_mes = sum(Total, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(
        Mes = factor(Mes, levels = meses_ordenados),
        Percentual = Total_mes / sum(Total_mes) * 100
      ) %>%
      arrange(Total_mes)
    
    p2 <- ggplot(df_mes, aes(x = reorder(Mes, Total_mes), y = Total_mes,
                             text = paste("Mês:", Mes, "<br>Total:", format(Total_mes, big.mark = ".")))) +
      geom_col(fill = "red") +
      geom_text(
        aes(label = paste0(format(Total_mes, big.mark = "."), "\n(", round(Percentual, 1), "%)")),
        position = position_stack(vjust = 0.5),
        color = "white", size = 4
      ) +
      scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      labs(x = "Mês", y = "Total") +
      theme_minimal()
    
    ggplotly(p2, tooltip = "text")
  })

# Tabela Município x Meses
output$tabela_mensal_geral <- renderDT({
  meses_ordenados <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                       "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
  
  dados %>%
    filter(Mes %in% meses_ordenados) %>%
    group_by(Municipio, Mes, Tipo_CNH) %>%
    summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") %>%
    mutate(Mes = factor(Mes, levels = meses_ordenados)) %>%
    arrange(Mes) %>%
    datatable(
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE,
      colnames = c("Mês", "Tipo de CNH" ,"Total de Registros")
    )
})












#==============================================================================#
  

#==============================================================================#
#Gráfico CNH  
  output$grafico_tipo_cnh_geral <- renderPlotly({
    df_tipo <- dados %>%
      filter(!is.na(Tipo_CNH)) %>%
      count(Tipo_CNH) %>%
      arrange(n)
    
    p <- ggplot(df_tipo, aes(x = reorder(Tipo_CNH, n), y = n,
                             text = paste("Tipo CNH:", Tipo_CNH, "<br>Total:", format(n, big.mark = ".")))) +
      geom_col(fill = "lightgreen") +
      geom_text(aes(label = format(n, big.mark = ".")), hjust = -0.1, color = "black", size = 4) +
      coord_flip() +
      labs(x = "Tipo CNH", y = "Total de Registros") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
#==============================================================================#  
  

#====================== ABA 2: COM FILTROS ====================================#

  dados_filtrados <- reactive({
    dados %>%
      filter(
        Ano %in% input$filtro_ano,
        Mes %in% input$filtro_mes,
        Municipio %in% input$filtro_municipio
      )
  })
  
  output$grafico_mensal_filtros <- renderPlotly({
    df_mes <- dados_filtrados() %>%
      count(Mes) %>%
      mutate(Mes = factor(Mes, levels = month.name),
             Percentual = n / sum(n) * 100) %>%
      arrange(Mes)
    
    p <- ggplot(df_mes, aes(x = Mes, y = n,
                            text = paste("Mês:", Mes, "<br>Registros:", format(n, big.mark = ".")))) +
      geom_col(fill = "seagreen") +
      geom_text(aes(label = paste0(format(n, big.mark = "."), "\n(", round(Percentual, 1), "%)")),
                vjust = -0.2, color = "black", size = 3.5) +
      scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
      labs(x = "Mês", y = "Número de Registros") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$grafico_total_servicos_filtros <- renderPlotly({
    df_total <- dados_filtrados() %>%
      select(Renovacao, Mudanca_Categoria, `1_CNH`, Reabilitacao, `2Via_CNH`, CNH_Definitiva) %>%
      summarise(across(everything(), sum, na.rm = TRUE)) %>%
      pivot_longer(everything(), names_to = "Servico", values_to = "Total") %>%
      mutate(
        Servico = recode(Servico,
                         "Renovacao" = "Renovação",
                         "Mudanca_Categoria" = "Mudança de Categoria",
                         "1_CNH" = "1ª CNH",
                         "Reabilitacao" = "Reabilitação",
                         "2Via_CNH" = "2ª Via CNH",
                         "CNH_Definitiva" = "CNH Definitiva"),
        Percentual = Total / sum(Total) * 100
      ) %>%
      arrange(Total)
    
    p <- ggplot(df_total, aes(x = reorder(Servico, Total), y = Total,
                              text = paste("Serviço:", Servico, "<br>Total:", format(Total, big.mark = ".")))) +
      geom_col(fill = "royalblue") +
      geom_text(aes(label = paste0(format(Total, big.mark = "."), "\n(", round(Percentual, 1), "%)")),
                hjust = -0.1, color = "black", size = 4) +
      coord_flip() +
      scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
      labs(x = "Serviço", y = "Total Filtrado") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  output$grafico_tipo_cnh_filtros <- renderPlotly({
    df_tipo <- dados_filtrados() %>%
      filter(!is.na(Tipo_CNH)) %>%
      count(Tipo_CNH) %>%
      arrange(n)
    
    p <- ggplot(df_tipo, aes(x = reorder(Tipo_CNH, n), y = n,
                             text = paste("Tipo CNH:", Tipo_CNH, "<br>Total:", format(n, big.mark = ".")))) +
      geom_col(fill = "tomato") +
      geom_text(aes(label = format(n, big.mark = ".")), hjust = -0.1, color = "black", size = 4) +
      coord_flip() +
      labs(x = "Tipo CNH", y = "Total de Registros") +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

# Rodar app
shinyApp(ui, server)
