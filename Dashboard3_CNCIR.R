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

# Função auxiliar para formatar números
formatar_numero <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",")
}

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
      menuItem("TABELA GERAL", tabName = "tabgeral", icon = icon("table")),
      menuItem("PAINEL DE FILTROS", tabName = "filtros", icon = icon("filter"))
    )
  ),
  dashboardBody(
    tabItems(
#=========================================================================================================================#
# ABA 1: GRAFICO GERAL
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
# Aba 1 : TABELA GERAL
tabItem(tabName = "tabgeral",
        fluidRow(
          selectInput("filtro_servico_top", "Filtrar por Serviço:",
                      choices = c("Total Geral", "Renovação", "1ª CNH", "Mudança de Categoria", 
                                  "Reabilitação", "2ª Via CNH", "CNH Definitiva"),
                      selected = "Total Geral"),
          box(title = "Top 10 Municípios", width = 8, DTOutput("tabela_top_municipios")),
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

#=========================================================================================================================#  
  observeEvent(input$reset_filtros, {
    updateSelectInput(session, "filtro_ano", selected = "TODOS")
    updateSelectInput(session, "filtro_mes", selected = "TODOS")
    updateSelectInput(session, "filtro_municipio", selected = "TODOS")
  })
  
  dados_filtrados <- reactive({
    dados %>%
      filter(
        if (input$filtro_ano == "TODOS") TRUE else Ano == input$filtro_ano,
        if (input$filtro_mes == "TODOS") TRUE else Mes == input$filtro_mes,
        if (input$filtro_municipio == "TODOS") TRUE else Municipio == input$filtro_municipio
      )
  })
#=========================================================================================================================#
  

#============================= ABA 1: GERAL ==============================================================================#
# Gráfico por Serviços
  output$grafico_total_servicos <- renderPlotly({
    df_total <- dados %>%
      select(Renovacao, Mudanca_Categoria, `1_CNH`, Reabilitacao, `2Via_CNH`, CNH_Definitiva) %>%
      summarise(across(everything(), sum, na.rm = TRUE)) %>%
      pivot_longer(everything(), names_to = "Servico", values_to = "Total") %>%
      mutate(
        Percentual = Total / sum(Total) * 100,
        Label = paste0(formatar_numero(Total), "\n(", round(Percentual, 1), "%)")
      ) %>%
      arrange(Total)
    
    p1 <- ggplot(df_total, aes(x = reorder(Servico, -Percentual), y = Total,
                               text = paste0("Serviço: ", Servico, 
                                             "<br>Total: ", formatar_numero(Total), 
                                             "<br>Percentual: ", round(Percentual, 1), "%"))) +
      geom_col(fill = "blue") +
      geom_text(
        aes(label = paste0(formatar_numero(Total), "\n(", round(Percentual, 1), "%)")),
        position = position_stack(vjust = 0.5),
        color = "white", size = 4
      ) +
      scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
      labs(x = "Serviço", y = "Quantidade Total") +
      theme_minimal()
    
    ggplotly(p1, tooltip = "text")
  })
#=========================================================================================================================#
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
                             text = paste("Mês:", Mes, "<br>Total:", formatar_numero(Total_mes)))) +
      geom_col(fill = "red") +
      geom_text(
        aes(label = paste0(formatar_numero(Total_mes), "\n(", round(Percentual, 1), "%)")),
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
  df <- dados %>%
    filter(!is.na(Tipo_CNH)) %>%
    group_by(Tipo_CNH) %>%
    summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") %>%
    mutate(Percentual = Total / sum(Total) * 100)
  
  p3 <- ggplot(df, aes(x = reorder(Tipo_CNH, Total), y = Total,
                      text = paste("Tipo CNH:", Tipo_CNH, "<br>Total:", formatar_numero(Total), "<br>", round(Percentual,1), "%"))) +
    geom_col(fill = "darkgreen") +
    geom_text(aes(label = paste0(formatar_numero(Total), "\n(", round(Percentual,1), "%)")),
              position = position_stack(vjust = 0.5), color = "white", size = 4) +
    coord_flip() +
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
      Total = formatar_numero(Total),
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
                              text = paste("Serviço:", Servico, "<br>Total:", formatar_numero(Total)))) +
      geom_col(fill = "royalblue") +
      geom_text(aes(label = paste0(formatar_numero(Total), "\n(", round(Percentual, 1), "%)")),
                position = position_stack(vjust = 0.5),
                color = "white", size = 4) +
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
      mutate(Percentual = Total / sum(Total) * 100) %>%
      arrange(Total)
    
    p5 <- ggplot(df_tipo, aes(x = reorder(Tipo_CNH, Total), y = Total,
                              text = paste("Tipo CNH:", Tipo_CNH, 
                                           "<br>Total:", formatar_numero(Total), 
                                           "<br>Percentual:", round(Percentual, 1), "%"))) +
      geom_col(fill = "tomato") +
      geom_text(aes(label = paste0(formatar_numero(Total), "\n(", round(Percentual, 1), "%)")),
                position = position_stack(vjust = 0.5),
                color = "white", size = 4) +
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

#=========================================================================================================================#



#=========================================================================================================================#

  output$tabela_top_municipios <- renderDT({
    colunas_servicos <- c("Renovacao", "1_CNH", "Mudanca_Categoria", "Reabilitacao", "2Via_CNH", "CNH_Definitiva")
    
    # Base agregada
    df <- dados %>%
      group_by(Municipio) %>%
      summarise(
        Renovacao = sum(Renovacao, na.rm = TRUE),
        `1ª CNH` = sum(`1_CNH`, na.rm = TRUE),
        `Mudança de Categoria` = sum(Mudanca_Categoria, na.rm = TRUE),
        Reabilitacao = sum(Reabilitacao, na.rm = TRUE),
        `2ª Via CNH` = sum(`2Via_CNH`, na.rm = TRUE),
        `CNH Definitiva` = sum(CNH_Definitiva, na.rm = TRUE),
        Total = sum(c_across(all_of(colunas_servicos)), na.rm = TRUE),
        .groups = "drop"
      )
    
    # Determinar coluna filtrada
    servico_escolhido <- input$filtro_servico_top
    coluna_ordenar <- switch(servico_escolhido,
                             "Renovação" = "Renovacao",
                             "1ª CNH" = "1ª CNH",
                             "Mudança de Categoria" = "Mudança de Categoria",
                             "Reabilitação" = "Reabilitacao",
                             "2ª Via CNH" = "2ª Via CNH",
                             "CNH Definitiva" = "CNH Definitiva",
                             "Total Geral" = "Total")
    
    # Se for "Total Geral", mostrar todos os serviços
    if (servico_escolhido == "Total Geral") {
      df_final <- df %>%
        arrange(desc(Total)) %>%
        slice_head(n = 144)
      colunas <- c("Município", "Renovação", "1ª CNH", "Mudança de Categoria",
                   "Reabilitação", "2ª Via CNH", "CNH Definitiva", "Total Geral")
    } else {
      df_final <- df %>%
        select(Municipio, !!servico_escolhido := .data[[coluna_ordenar]], Total) %>%
        arrange(desc(.data[[servico_escolhido]])) %>%
        slice_head(n = 144)
      colunas <- c("Município", servico_escolhido, "Total Geral")
    }
    
    # Formatar Números
    df_formatado <- df_final %>%
      mutate(across(where(is.numeric), ~formatar_numero(.x)))
    
    datatable(
      df_formatado,
      extensions = 'Buttons',
      style = "auto",
      options = list(
        pageLength = 10,
        dom = '<"d-flex justify-content-center"B>frtip',
        buttons = list(
          list(extend = 'csv',
               text = '<i class="fa fa-file-csv" style="color:white; background-color:#007bff; padding:2px 6px; border-radius:4px; font-size:11px;"></i> CSV'),
          list(extend = 'excel',
               text = '<i class="fa fa-file-excel" style="color:white; background-color:#28a745; padding:2px 6px; border-radius:4px; font-size:11px;"></i> Excel'),
          list(extend = 'pdf',
               text = '<i class="fa fa-file-pdf" style="color:white; background-color:#dc3545; padding:2px 6px; border-radius:4px; font-size:11px;"></i> PDF'),
          list(extend = 'print',
               text = '<i class="fa fa-print" style="color:white; background-color:#6c757d; padding:2px 6px; border-radius:4px; font-size:11px;"></i> Imprimir')
        ),
        autoWidth = TRUE,
        scrollY = "400px",
        searching = TRUE
      ),
      rownames = FALSE,
      escape = FALSE,
      colnames = colunas
    )
  })
  
  
  

  
  
}


#=========================================================================================================================#














# Rodar app
shinyApp(ui, server)
