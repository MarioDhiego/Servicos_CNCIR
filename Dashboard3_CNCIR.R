#=========================================================================================================================#
# Carregar Bibliotecas
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(dashboardthemes)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(scales)
library(DT)
library(readxl)
library(leaflet)      
library(classInt)    
library(BAMMtools)
library(htmlwidgets)
#=========================================================================================================================#

#=========================================================================================================================#
# Carregar Base de Dados
dados <- read_excel("Banco_CNCIR.xlsx")
#=========================================================================================================================#

#=========================================================================================================================#
# UI (Front End)
ui <- dashboardPage(
  dashboardHeader(title = "CIRETRAN'S", titleWidth = 230),
  dashboardSidebar(
    
    tags$head(
      tags$style(HTML("
      #setor {
        width: 300% !important;
      }
    "))
    ),
    tags$img(src = "detran1.jpeg", width = 230, height = 180),
    useShinyjs(),
    sidebarMenu(
      menuItem("PAINEL GERAL", tabName = "geral", icon = icon("chart-bar")),
      menuItem("PAINEL DE FILTROS", tabName = "filtros", icon = icon("filter"))
    )
  ),
  dashboardBody(
    tabItems(
#=========================================================================================================================#
# ABA 1: GERAL
tabItem(tabName = "geral",
        fluidRow(
          box(title = "TIPOS DE SERVIÇOS DE CNH", width = 7, plotlyOutput("grafico_total_servicos", height = "500px")),
          box(title = "TABELA DE SERVIÇOS POR MUNICÍPIOS X MESES", width = 5,DTOutput("tabela_cruzada")),
          box(title = "REGISTROS POR MESES", width = 7, plotlyOutput("grafico_mensal_geral", height = "500px")),
          box(title = "TABELA DE REGISTROS POR MUNICÍIOS X MESES", width = 5, DTOutput("tabela_mensal_geral")),
          box(title = "TIPIFICAÇÃO DA CNH", width = 7, plotlyOutput("grafico_tipo_cnh_geral", height = "500px")),
          box(title = "TABELA DE TIPO CNH", width = 5, DTOutput("tabela_tipo_cnh_geral"))
          )
        ),
#=========================================================================================================================#
# ABA 2: COM FILTROS
tabItem(tabName = "filtros",
        fluidRow(
          box(width = 12, status = "primary", solidHeader = TRUE, title = "Filtros",
              fluidRow(column(2,selectInput("filtro_ano", "Selecione o Ano:",
                                            choices = c("TODOS", sort(unique(dados$Ano))), 
                                            selected = "TODOS")),
                      column(2,selectInput("filtro_mes", "Selecione o Mês:",
                                           choices = c("TODOS", sort(unique(dados$Mes))), 
                                           selected = "TODOS")),
                      column(2,selectInput("filtro_municipio", "Selecione o Município:",
                                           choices = c("TODOS", sort(unique(dados$Municipio))), 
                                           selected = "TODOS")
                      ),
                      column(2,actionButton("reset_filtros", "REINICIAR", icon = icon("redo"))),
              )
                    )
              ),
              fluidRow(
               # box(title = "Frequência de Registros por Mês (Filtrado)", width = 10, 
                #    plotlyOutput("grafico_mensal_filtros", height = "500px")),
                box(title = "", width = 7, 
                    plotlyOutput("grafico_total_servicos_filtros", height = "500px")),
                box(title = "", width = 5, 
                    plotlyOutput("grafico_tipo_cnh_filtros", height = "500px"))
              )
      )
    )
  )
)
#=========================================================================================================================#
# SERVER (Back End)
server <- function(input, output, session) {
  
#observeEvent(input$reset_filtros, {
#  updateSelectInput(session, "filtro_ano", selected = unique(dados$Ano))
#  updateSelectInput(session, "filtro_mes", selected = unique(dados$Mes))
#  updateSelectInput(session, "filtro_municipio", selected = unique(dados$Municipio))
#  })
  
  observeEvent(input$reset_filtros, {
    updateSelectInput(session, "filtro_ano", selected = "TODOS")
    updateSelectInput(session, "filtro_mes", selected = "TODOS")
    updateSelectInput(session, "filtro_municipio", selected = "TODOS")
  })
  
  
  
  
#============================= ABA 1: GERAL ==============================================================================#
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
      geom_col(fill = "blue") +
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
#=========================================================================================================================#

#=========================================================================================================================#
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

#=========================================================================================================================#
# Tabela Município x Meses
output$tabela_mensal_geral <- renderDT({
  meses_ordenados <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                       "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
  
  dados %>%
    filter(Mes %in% meses_ordenados) %>%
    group_by(Municipio, Mes) %>%
    summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") %>%
    mutate(Mes = factor(Mes, levels = meses_ordenados)) %>%
    arrange(Mes) %>%
    datatable(
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE,
      colnames = c("Município", "Mês","Total de Registros")
    )
})
#=========================================================================================================================#
  
#=========================================================================================================================#
#Gráfico CNH  
output$grafico_tipo_cnh_geral <- renderPlotly({
  df_tipo <- dados %>%
    filter(!is.na(Tipo_CNH)) %>%
    group_by(Tipo_CNH) %>%
    summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") %>%
    arrange(Total)
  
  p3 <- ggplot(df_tipo, aes(x = reorder(Tipo_CNH, Total), y = Total,
                           text = paste("Tipo CNH:", Tipo_CNH, "<br>Total:", format(Total, big.mark = ".")))) +
    geom_col(fill = "lightgreen") +
    geom_text(aes(label = format(Total, big.mark = ".")), hjust = -0.1, color = "black", size = 4) +
    coord_flip() +
    labs(x = "Tipo CNH", y = "Total de Registros") +
    theme_minimal()
  
  ggplotly(p3, tooltip = "text")
})

#=========================================================================================================================#
# Tabela Município x CNH  
output$tabela_tipo_cnh_geral <- renderDT({
  df <- dados %>%
    filter(!is.na(Tipo_CNH), !is.na(Municipio)) %>%
    group_by(Municipio, Tipo_CNH) %>%
    summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") %>%
    group_by(Municipio) %>%
    mutate(
      Percentual = round(Total / sum(Total) * 100, 1),
      Total = format(Total, big.mark = ".", decimal.mark = ","),
      Percentual = paste0(Percentual, " %")
    ) %>%
    ungroup()
  
  datatable(
    df,
    rownames = FALSE,
    colnames = c("Município", "Tipo CNH", "Total", "Percentual"),
    options = list(pageLength = 10, autoWidth = TRUE)
  )
  })
#=========================================================================================================================#
  

#====================== ABA 2: COM FILTROS ===============================================================================#
dados_filtrados <- reactive({
  dados %>%
    filter(
      if (input$filtro_ano == "TODOS") TRUE else Ano == input$filtro_ano,
      if (input$filtro_mes == "TODOS") TRUE else Mes == input$filtro_mes,
      if (input$filtro_municipio == "TODOS") TRUE else Municipio == input$filtro_municipio
    )
})

#=========================================================================================================================# 

  
#=========================================================================================================================#
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
    
    p4 <- ggplot(df_total, aes(x = reorder(Servico, Total), y = Total,
                              text = paste("Serviço:", Servico, "<br>Total:", format(Total, big.mark = ".")))) +
      geom_col(fill = "royalblue") +
      geom_text(aes(label = paste0(format(Total, big.mark = "."), "\n(", round(Percentual, 1), "%)")),
                hjust = -0.1, color = "black", size = 4) +
      coord_flip() +
      scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
      labs(
        x = "", 
        y = "Total de Serviços",
        title = paste("Serviços de CNH -",
                      if (input$filtro_municipio != "TODOS") input$filtro_municipio else "Município",
                      "|", if (input$filtro_mes != "TODOS") input$filtro_mes else "Mes",
                      "|", if (input$filtro_ano != "TODOS") input$filtro_ano else "Ano")
      ) +
      theme_minimal()
    ggplotly(p4, tooltip = "text")
  })
#=========================================================================================================================#  
  
#=========================================================================================================================#
  output$grafico_tipo_cnh_filtros <- renderPlotly({
    df_tipo <- dados_filtrados() %>%
      filter(!is.na(Tipo_CNH)) %>%
      group_by(Tipo_CNH) %>%
      summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") %>%
      arrange(Total)
    p5 <- ggplot(df_tipo, aes(x = reorder(Tipo_CNH, Total), y = Total,
                              text = paste("Tipo CNH:", Tipo_CNH, "<br>Total:", format(Total, big.mark = ".")))) +
      geom_col(fill = "tomato") +
      geom_text(aes(label = format(Total, big.mark = ".")), hjust = -0.1, color = "black", size = 4) +
      #coord_flip() +
      labs(
        x = "",
        y = "Total de Registros",
        title = paste("Distribuição por Tipo de CNH -",
                      if (input$filtro_municipio != "TODOS") input$filtro_municipio else "Município",
                      "|", if (input$filtro_mes != "TODOS") input$filtro_mes else "Mes",
                      "|", if (input$filtro_ano != "TODOS") input$filtro_ano else "Ano")
      ) +
      theme_minimal()
    ggplotly(p5, tooltip = "text")
    
  })
}
#=========================================================================================================================#


# Rodar app
shinyApp(ui, server)
