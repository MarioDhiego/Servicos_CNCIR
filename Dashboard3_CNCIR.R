#=========================================================================================================================#
# Carregar Bibliotecas
#=========================================================================================================================#
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
library(leaflet)
library(geobr)
library(sf)
library(classInt)
library(BAMMtools)
library(htmlwidgets)
library(RColorBrewer)
#=========================================================================================================================#


#=========================================================================================================================#
# Carregar Base de Dados
# Certifique-se que o arquivo "Banco_CNCIR.xlsx" está na mesma pasta do script.
dados <- read_excel("Banco_CNCIR.xlsx")
validate(
  need(!is.null(dados), "Erro ao carregar 'Banco_CNCIR.xlsx'. Verifique o caminho e formato.")
)

# Função auxiliar para formatar números
formatar_numero <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", nsmall = 0)
}

# SUGESTÃO: Vetor para ordenar os meses cronologicamente nos filtros e gráficos
meses_ordenados <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                     "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
#=========================================================================================================================#
# UI (Front End)
#=========================================================================================================================#
ui <- dashboardPage(
  dashboardHeader(title = "CIRETRAN'S", titleWidth = 230),
  dashboardSidebar(
    tags$img(src = "detran1.jpeg", width = 230, height = 180),
    useShinyjs(),
    sidebarMenu(
      menuItem("PAINEL GERAL", tabName = "geral", icon = icon("chart-bar")),
      menuItem("TABELA GERAL", tabName = "tabgeral", icon = icon("table")),
      menuItem("PAINEL DE FILTROS", tabName = "filtros", icon = icon("filter")),
      menuItem("MAPA DE SERVIÇOS", tabName = "mapa", icon = icon("map-marked-alt"))
    )
  ),
  dashboardBody(
    tags$head(
      #========================================================================#
      # -------------  SCRIPT PARA CORRIGIR O REDIMENSIONAMENTO ---------------#
      #========================================================================#
      tags$script(HTML("
          // Este código 'escuta' por um clique em qualquer botão de maximizar
          $(document).on('click', '[data-card-widget=\"maximize\"]', function(){
            // Ele espera um curto período (350ms) para a animação da caixa terminar
            setTimeout(function() {
              // E então dispara um evento de 'resize' na janela.
              // Todos os gráficos (echarts4r, leaflet) escutam por este evento
              // e se redimensionam para caber no novo espaço.
              $(window).trigger('resize');
            }, 350);
          });
        ")),
      #========================================================================#
      # Código CSS para aumentar a imagem
      tags$style(HTML("
  /* Ajusta a altura de TODO o container da marca */
  .brand-link {
    height: 60px !important; /* Aumenta a área total do logo */
  }

  /* Ajusta a imagem DENTRO do container */
  .brand-image {
    height: 50px !important;      /* A altura da imagem */
    width: auto !important;       /* Largura automática para manter proporção */
    margin-top: 5px !important;   /* Ajusta a margem superior para melhor alinhamento */
  }
"))
    ),
    tabItems(
#=========================================================================================================================#
# ABA 1: PAINEL GERAL
      tabItem(tabName = "geral",
              fluidRow(
                box(title = "TIPOS DE SERVIÇOS DE CNH", width = 7, plotlyOutput("grafico_total_servicos", height = "500px")),
                box(title = "SERVIÇOS POR MUNICÍPIO X TIPO DE CNH", width = 5, DTOutput("tabela_cruzada")),
                box(title = "REGISTROS POR MESES", width = 7, plotlyOutput("grafico_mensal_geral", height = "500px")),
                box(title = "TABELA DE REGISTROS POR MUNICÍPIO X MÊS", width = 5, DTOutput("tabela_mensal_geral")),
                box(title = "TIPIFICAÇÃO DA CNH", width = 7, plotlyOutput("grafico_tipo_cnh_geral", height = "500px")),
                box(title = "TABELA DE TIPO CNH", width = 5, DTOutput("tabela_tipo_cnh_geral"))
              )
      ),
#=========================================================================================================================#
# ABA 4: MAPA (COM BOTÃO DE RESET)
      tabItem(tabName = "mapa",
              fluidRow(
                box(width = 10, 
                    status = "primary", 
                    solidHeader = FALSE,
                    maximizable = TRUE,
                    collapsed = FALSE,
                    headerBorder = TRUE,
                    title = "Visão Espacial dos Serviços de CNH",
                    fluidRow(
                      column(2, 
                             selectInput("filtro_servico_mapa", "Tipo de Serviços:",
                                         choices = c("Renovação", "1ª CNH", "Mudança de Categoria",
                                                     "Reabilitação", "2ª Via CNH", "CNH Definitiva", "Total Geral"),
                                         selected = "Total Geral")
                      ),
                      column(2, 
                             tags$div(style = "margin-top: 25px;", 
                                      actionButton("reset_mapa", "REINICIAR", 
                                                   icon = icon("globe-americas"))
                             )
                      )
                    ),
                    hr(),
                    withSpinner(
                    leafletOutput("mapa_servicos", height = "600px")
                    )
                )
              )
      ),
  
      
      
      
#=========================================================================================================================#
# Aba 2 : TABELA GERAL
      tabItem(tabName = "tabgeral",
              fluidRow(
                selectInput("filtro_servico_top", "Filtrar por Serviço:",
                            choices = c("Total Geral", "Renovação", "1ª CNH", "Mudança de Categoria",
                                        "Reabilitação", "2ª Via CNH", "CNH Definitiva"),
                            selected = "Total Geral"),
                box(title = "TABELA GERAL POR MUNICÍPIO", 
                    width = 12, 
                    DTOutput("tabela_top_municipios"))
              )
      ),
#=========================================================================================================================#
# ABA 3: PAINEL DE FILTROS
      tabItem(tabName = "filtros",
              fluidRow(
                box(width = 12, status = "primary", solidHeader = TRUE, title = "Filtros",
                    fluidRow(
                      column(2, selectInput("filtro_ano", "Selecione o Ano:",
                                            choices = c("TODOS", sort(unique(dados$Ano))),
                                            selected = "TODOS")),
                      # SUGESTÃO: Filtro de mês agora usa a ordem cronológica.
                      column(2, selectInput("filtro_mes", "Selecione o Mês:",
                                            choices = c("TODOS", meses_ordenados),
                                            selected = "TODOS")),
                      column(2, selectInput("filtro_municipio", "Selecione o Município:",
                                            choices = c("TODOS", sort(unique(dados$Municipio))),
                                            selected = "TODOS")),
                      column(2, actionButton("reset_filtros", "REINICIAR", icon = icon("redo")))
                    )
                )
              ),
              fluidRow(
                box(title = "Serviços de CNH com Filtro", width = 7,
                    plotlyOutput("grafico_total_servicos_filtros", height = "500px")),
                box(title = "Tipo de CNH com Filtro", width = 5,
                    plotlyOutput("grafico_tipo_cnh_filtros", height = "500px"))
              )
      )
    )
  )
)
#=========================================================================================================================#
# SERVER (Back End)
#=========================================================================================================================#
server <- function(input, output, session) {
  
  observeEvent(input$reset_mapa, {
    updateSelectInput(session, "filtro_servico_mapa", selected = "Total Geral")
  })  

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
  
#============================= ABA 1: GERAL ==============================================================================#
#-Gráfico por Serviços
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
  
#-Tabela Município x Tipo_CNH
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
  
#-Gráfico por Meses
  output$grafico_mensal_geral <- renderPlotly({
    df_mes <- dados %>%
      filter(Mes %in% meses_ordenados) %>%
      group_by(Mes) %>%
      summarise(Total_mes = sum(Total, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(
        Mes = factor(Mes, levels = meses_ordenados),
        Percentual = Total_mes / sum(Total_mes) * 100
      )
    
    p2 <- ggplot(df_mes, aes(x = Mes, y = Total_mes,
                             text = paste("Mês:", Mes, "<br>Total:", formatar_numero(Total_mes)))) +
      geom_col(fill = "red") +
      geom_text(
        aes(label = paste0(formatar_numero(Total_mes), "\n(", round(Percentual, 1), "%)")),
        position = position_stack(vjust = 0.5),
        color = "white", size = 4
      ) +
      scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
      labs(x = "Mês", y = "Total") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p2, tooltip = "text")
  })
  
#-Tabela Município x Meses
  output$tabela_mensal_geral <- renderDT({
    dados %>%
      filter(Mes %in% meses_ordenados) %>%
      group_by(Municipio, Mes) %>%
      summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") %>%
      mutate(Mes = factor(Mes, levels = meses_ordenados)) %>%
      arrange(Mes, desc(Total)) %>%
      datatable(
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE,
        colnames = c("Município", "Mês","Total de Registros")
      )
  })
  
#-Gráfico CNH
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
      labs(x = NULL, y = "Total") +
      theme_minimal()
    
    ggplotly(p3, tooltip = "text")
  })
  
#-Tabela Município x CNH
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
#====================== ABA 2: TABELA GERAL ==============================================================================#
  
output$tabela_top_municipios <- renderDT({
    # Este vetor precisa corresponder aos nomes das colunas no arquivo Excel original ("dados")
    colunas_servicos <- c("Renovacao", "1_CNH", "Mudanca_Categoria", "Reabilitacao", "2Via_CNH", "CNH_Definitiva")
    
    df <- dados %>%
      group_by(Municipio) %>%
      # CORREÇÃO: Os nomes das colunas criadas agora são idênticos aos do filtro.
      summarise(
        `Renovação` = sum(Renovacao, na.rm = TRUE),
        `1ª CNH` = sum(`1_CNH`, na.rm = TRUE),
        `Mudança de Categoria` = sum(Mudanca_Categoria, na.rm = TRUE),
        `Reabilitação` = sum(Reabilitacao, na.rm = TRUE),
        `2ª Via CNH` = sum(`2Via_CNH`, na.rm = TRUE),
        `CNH Definitiva` = sum(CNH_Definitiva, na.rm = TRUE),
        Total = sum(c_across(all_of(colunas_servicos)), na.rm = TRUE),
        .groups = "drop"
      )
    
    servico_escolhido <- input$filtro_servico_top
    
    if (servico_escolhido == "Total Geral") {
      df_final <- df %>%
        rename(`Total Geral` = Total) %>%
        arrange(desc(`Total Geral`))
      
      colunas <- c("Município", "Renovação", "1ª CNH", "Mudança de Categoria",
                   "Reabilitação", "2ª Via CNH", "CNH Definitiva", "Total Geral")
    } else {
      df_final <- df %>%
        rename(`Total Geral` = Total) %>%

#-Agora 'all_of(servico_escolhido)' vai funcionar pois o nome existe.
        select(Municipio, all_of(servico_escolhido), `Total Geral`) %>%
        arrange(desc(.data[[servico_escolhido]]))
      
      colunas <- c("Município", servico_escolhido, "Total Geral")
    }
    
    df_formatado <- df_final %>%
      mutate(across(where(is.numeric), ~formatar_numero(.x)))
    
    datatable(
      df_formatado,
      extensions = 'Buttons',
      options = list(
        pageLength = 10,
        dom = '<"d-flex justify-content-center"B>frtip',
        buttons = list(
          list(extend = 'csv', text = '<i class="fa fa-file-csv"></i> CSV'),
          list(extend = 'excel', text = '<i class="fa fa-file-excel"></i> Excel'),
          list(extend = 'pdf', text = '<i class="fa fa-file-pdf"></i> PDF'),
          list(extend = 'print', text = '<i class="fa fa-print"></i> Imprimir')
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
  
#====================== ABA 3: FILTROS =================================================================================#
  
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
      filter(Total > 0) # Adicionado para não mostrar barras com valor zero
    
    p4 <- ggplot(df_total, aes(x = reorder(Servico, Total), y = Total,
                               text = paste("Serviço:", Servico, "<br>Total:", formatar_numero(Total)))) +
      geom_col(fill = "blue") +
      geom_text(aes(label = paste0(formatar_numero(Total), "\n(", round(Percentual, 1), "%)")),
                position = position_stack(vjust = 0.5),
                color = "white", size = 4) +
      coord_flip() +
      scale_y_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
      labs(x = NULL, y = "Total de Serviços") +
      theme_minimal()
    ggplotly(p4, tooltip = "text")
  })
  
  output$grafico_tipo_cnh_filtros <- renderPlotly({
    df_tipo <- dados_filtrados() %>%
      filter(!is.na(Tipo_CNH)) %>%
      group_by(Tipo_CNH) %>%
      summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop") %>%
      mutate(Percentual = Total / sum(Total) * 100) %>%
      filter(Total > 0) # Adicionado para não mostrar barras com valor zero
    
    p5 <- ggplot(df_tipo, aes(x = reorder(Tipo_CNH, Total), y = Total,
                              text = paste("Tipo CNH:", Tipo_CNH,
                                           "<br>Total:", formatar_numero(Total),
                                           "<br>Percentual:", round(Percentual, 1), "%"))) +
      geom_col(fill = "red") +
      geom_text(aes(label = paste0(formatar_numero(Total), "\n(", round(Percentual, 1), "%)")),
                position = position_stack(vjust = 0.5),
                color = "white", size = 4) +
      # SUGESTÃO: Gráfico agora é horizontal para consistência.
      coord_flip() +
      labs(x = NULL, y = "Total de Registros") +
      theme_minimal()
    ggplotly(p5, tooltip = "text")
  })

#=========================================================================================================================#


#=========================================================================================================================#
# LÓGICA PARA O MAPA DE SERVIÇOS (CARREGANDO ARQUIVO geopa.rds)
  
   output$mapa_servicos <- renderLeaflet({
     
    mapa_para <- readRDS("geopa.rds")
    validate(
      need(!is.null(mapa_para), "Arquivo 'geopa.rds' não encontrado ou está corrompido. Certifique-se de que o arquivo está na mesma pasta do seu aplicativo.")
    )
    
    dados_agregados_mapa <- dados %>%
      group_by(Municipio) %>%
      summarise(
        `Renovação` = sum(Renovacao, na.rm = TRUE),
        `1ª CNH` = sum(`1_CNH`, na.rm = TRUE),
        `Mudança de Categoria` = sum(Mudanca_Categoria, na.rm = TRUE),
        `Reabilitação` = sum(Reabilitacao, na.rm = TRUE),
        `2ª Via CNH` = sum(`2Via_CNH`, na.rm = TRUE),
        `CNH Definitiva` = sum(CNH_Definitiva, na.rm = TRUE),
        `Total Geral` = sum(Total, na.rm = TRUE),
        .groups = "drop"
      )
    remover_acentos <- function(texto) {
      iconv(texto, to = "ASCII//TRANSLIT")
    }
    mapa_para_join <- mapa_para %>%
      mutate(Municipio_join = toupper(remover_acentos(name_muni)))
    
    dados_agregados_join <- dados_agregados_mapa %>%
      mutate(Municipio_join = toupper(remover_acentos(Municipio)))
    
    dados_mapa_final <- mapa_para_join %>%
      left_join(dados_agregados_join, by = "Municipio_join")
    
# 4. Criar o mapa com Leaflet 
    servico_selecionado <- input$filtro_servico_mapa
    dados_mapa_final$valor_selecionado <- dados_mapa_final[[servico_selecionado]]
    dados_mapa_final$valor_selecionado[is.na(dados_mapa_final$valor_selecionado)] <- 0
    
  
# Usando a paleta de cores "Blues"
    paleta_cores <- colorNumeric(
      palette = c("white", "#74B4E8", "#1F83E0", "#1D44B8", "#090991"),
      domain = dados_mapa_final$valor_selecionado
    )
    rotulos_popup <- sprintf(
      "<strong>%s</strong><br/>%s: %s",
      dados_mapa_final$name_muni,
      servico_selecionado,
      formatar_numero(dados_mapa_final$valor_selecionado)
    ) %>% lapply(htmltools::HTML)
    
    leaflet(data = dados_mapa_final,
            options = leafletOptions(minZoom = 0, maxZoom = 15, zoomControl = TRUE),
            width = "100%", height = "600px") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      addPolygons(
        fillColor = ~paleta_cores(valor_selecionado),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = rotulos_popup,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = paleta_cores,
        values = ~valor_selecionado,
        opacity = 0.7,
        title = servico_selecionado,
        position = "bottomright"
      )
  })
#=========================================================================================================================#  
  

  
}

#=========================================================================================================================#
# Rodar o aplicativo
#=========================================================================================================================#
shinyApp(ui, server)