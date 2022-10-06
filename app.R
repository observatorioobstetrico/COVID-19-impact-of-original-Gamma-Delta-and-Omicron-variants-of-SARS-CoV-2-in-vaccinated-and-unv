library(shiny)
library(dplyr)
library(magrittr)
library(readxl)
library(shinydashboard)
library(questionr)
library(kableExtra)
library(ggplot2)
library(highcharter)
library(summarytools)
library(modelsummary)
library(abjData)
library(leaflet)
library(leaflet.extras)
library(stringr)
library(reactable)
library(htmltools)
library(zoo)
library(plotly)
library(lubridate)
library(googlesheets4)
library(shinyjs)
# library(logbin)
library(DescTools)
library(tidymodels)
library(themis)
library(dplyr)
library(glmtoolbox)
# library(glmx)
# library(LDdiag)
#library(hnp)
# library(patchwork)

# Carregando a base de dados ----

dados5 <- readRDS("dados6.rds")

dados5 <- dados5 %>% 
  mutate(vacina_cov1 = case_when(
    variante == "original" ~ "não",
    vacina_cov == "sim" ~ "sim",
    vacina_cov == "não" ~ "não",
    TRUE ~ NA_character_
  ))


dados5$variante <- factor(dados5$variante,
                          levels = c("original", "gama", "delta","omicron"))


dados5 <- dados5 %>% 
  mutate(dt_1dose = as.Date(DOSE_1_COV, format = "%d/%m/%Y")) %>% 
  mutate(dt_2dose = as.Date(DOSE_2_COV, format = "%d/%m/%Y")) %>% 
  mutate(doses = case_when(
    vacina_cov1 == "sim" & is.na(dt_1dose) 
    & is.na(dt_2dose) ~ "pelo menos uma dose",
    !is.na(dt_2dose) ~ "duas doses",
    !is.na(dt_1dose) & is.na(dt_2dose) ~ "pelo menos uma dose",
    TRUE ~ "não informado"))


# Alterações iniciais na base ----

## Reajuste de levels ----
dados5$faixa_et <- factor(dados5$faixa_et,
                          levels = c("<20", "20-34", ">=35"))


dados5$escol <- factor(dados5$escol,
                       levels = c("sem escol", "fund1", "fund2", "medio", "superior"))

dados5 <- dados5 %>% 
  mutate(escol1 = as.factor(case_when(
    escol == "sem escol" ~ "sem escol",
    escol == "fund1" ~ "fund",
    escol == "fund2" ~ "fund",
    escol == "medio"  ~ "medio",
    escol == "superior"  ~ "superior")))


dados5 <- dados5 %>% 
  mutate(intubacao_SN = as.factor(case_when(
    suport_ven  == "não" ~ "não",
    suport_ven  == "não invasivo" ~ "não",
    suport_ven  == "invasivo" ~ "sim")))



## Restante de alterações ----
dados5$raca_sel <- dados5$raca
dados5$raca_sel <-
  ifelse(is.na(dados5$raca), "não informado", dados5$raca)

dados5$vacina_cov_sel <-
  ifelse(is.na(dados5$vacina_cov1), "nao informado", dados5$vacina_cov1)


dados5$CLASSI_FIN <- as.factor(dados5$CLASSI_FIN)


dados5$DT_SIN_PRI <- dmy(dados5$DT_SIN_PRI)
dados5$DT_EVOLUCA <- dmy(dados5$DT_EVOLUCA)

dados5 <- dados5 %>% 
  mutate(vacinacov_variante2 = as.factor(case_when(
    variante == "original" ~ "original",
    variante == "gama" & vacina_cov1 == "sim" ~ "gama_vacinasim",
    variante == "gama" & vacina_cov1 == "não" ~ "gama_vacinanao",
    variante == "delta" & vacina_cov1 == "sim" ~ "delta_vacinasim",
    variante == "delta" & vacina_cov1 == "não" ~ "delta_vacinanao",
    variante == "omicron" & vacina_cov1 == "sim" ~ "omicron_vacinasim",
    variante == "omicron" & vacina_cov1 == "não" ~ "omicron_vacinanao")))

dados5$vacinacov_variante2 <- relevel(dados5$vacinacov_variante2,ref="original")

teste_breslowday <- function(dados1, dados2, dados3, dados4, var) {
  tab1 <- array(0, dim = c(2,2,2))
  tab2 <- array(0, dim = c(2,2,2))
  tab3 <- array(0, dim = c(2,2,2))
  tab1[,,1] <- table(dados1$grupos, as.character(dados1[[var]]))
  tab1[,,2] <- table(dados2$grupos, as.character(dados2[[var]]))
  tab2[,,1] <- table(dados1$grupos, as.character(dados1[[var]]))
  tab2[,,2] <- table(dados3$grupos, as.character(dados3[[var]]))
  tab3[,,1] <- table(dados1$grupos, as.character(dados1[[var]]))
  tab3[,,2] <- table(dados4$grupos, as.character(dados4[[var]]))
  a <- BreslowDayTest(tab1, correct = TRUE)
  b <- BreslowDayTest(tab2, correct = TRUE)
  d <- BreslowDayTest(tab3, correct = TRUE)
  out <- data.frame(comp=c("orig-gama","orig-delta","orig-omicr"),
                    stat = c(a$statistic, b$statistic, d$statistic),
                    p_valor = c(a$p.value, b$p.value, d$p.value)
  )
  return(out)
}

dados_smote <- function(dados,var){
  dados1 <- dados %>% select(variante,vacinacov_variante2,vacina_cov_sel,grupos,var) %>% drop_na()
  
  # set.seed(69)
  
  ds_rec <- recipe(vacinacov_variante2 ~., data = dados1) %>%
    step_smotenc(vacinacov_variante2,seed=69) %>%
    prep()
  
  new_data <- juice(ds_rec)
  
  return(new_data)
  
}

dados_downsample <- function(dados,var){
  dados1 <- dados %>% select(variante,vacinacov_variante2,vacina_cov_sel,grupos,var) %>% drop_na()
  
  # set.seed(69)
  
  ds_rec <- recipe(vacinacov_variante2 ~., data = dados1) %>%
    step_downsample(vacinacov_variante2,seed = 69) %>%
    prep()
  
  new_data <- juice(ds_rec)
  
  return(new_data)
  
}

sticky_style <-
  list(
    position = "sticky",
    left = 0,
    background = "#fff",
    zIndex = 1,
    borderRight = "1px solid #eee"
  )

# Dia de hoje ----
hoje <- Sys.Date()


humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

table <- "responses"

appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# info for sharing this app on facebook/twitter ----
share <- list(title = "Inscrição na Newsletter")

# Custom dropdownMenu function for share icons ----
customSentence <- function(numItems, type) {
  paste("Feedback")
}

customSentence_share <- function(numItems, type) {
  paste("Gostou? Compartilhe!")
}

dropdownMenuCustom <- function (..., 
                                type = c("messages", "notifications", "tasks"), 
                                badgeStatus = "primary", 
                                icon = NULL,
                                .list = NULL, 
                                customSentence = customSentence){
  type <- match.arg(type)
  if (!is.null(badgeStatus)) shinydashboard:::validateStatus(badgeStatus)
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li")
  dropdownClass <- paste0("dropdown ", type, "-menu")
  if (is.null(icon)) {
    icon <- switch(type, 
                   messages = shiny::icon("envelope"), 
                   notifications = shiny::icon("warning"), 
                   tasks = shiny::icon("tasks"))
  }
  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  }
  else {
    badge <- tags$span(class = paste0("label label-", badgeStatus), 
                       numItems)
  }
  tags$li(
    class = dropdownClass, 
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon, 
      badge
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        customSentence(numItems, type)
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}

# User Interface ----
ui <-
  dashboardPage(
    title = "Análise Variantes Gestantes e Puérperas",
    dashboardHeader(
      title = strong('Análise Variantes Gestantes e Puérperas'),
      dropdownMenuCustom(
        type = 'message',
        customSentence = customSentence,
        messageItem(
          from = "observatorioobstetricobr@gmail.com",
          message =  "",
          icon = icon("envelope"),
          href = "mailto:observatorioobstetricobr@gmail.com"
        ),
        icon = icon('envelope')
      ),
      dropdownMenuCustom(
        type = 'message',
        customSentence = customSentence_share,
        icon = icon("share-alt"),
        messageItem(
          from = 'Twitter',
          message = "",
          icon = icon("twitter"),
          href = "https://twitter.com/intent/tweet?url=Observat%C3%B3rio%20Obst%C3%A9trico%20Brasileiro%20COVID-19%0Ahttps%3A%2F%2Fobservatorioobstetrico.shinyapps.io%2Fcovid_gesta_puerp_br%2F"
        ),
        messageItem(
          from = 'Facebook',
          message = "",
          icon = icon("facebook"),
          href = "https://www.facebook.com/sharer/sharer.php?u=https%3A%2F%2Fobservatorioobstetrico.shinyapps.io%2Fcovid_gesta_puerp_br%2F"
        ),
        messageItem(
          from = 'WhatsApp',
          message = "",
          icon = icon("whatsapp-square"),
          href = "https://web.whatsapp.com/send?text=https%3A%2F%2Fobservatorioobstetrico.shinyapps.io%2Fcovid_gesta_puerp_br%2F"
        ),
        messageItem(
          from = 'LinkedIn',
          message = "",
          icon = icon("linkedin"),
          href = "http://www.linkedin.com/shareArticle?mini=true&url=https%3A%2F%2Fobservatorioobstetrico.shinyapps.io%2Fcovid_gesta_puerp_br%2F&title=Observat%C3%B3rio%20Obst%C3%A9trico%20Brasileiro%20COVID-19%20"
        )
      )
    ),
    dashboardSidebar(
      ## Menu ----
      sidebarMenu(
        style = "position: fixed; overflow: visible;",
        # menuItem("Início", tabName = "inicio"),
        # menuItem("Documentação", tabName = "doc"),
        # menuItem("Informações Gerais", tabName = "info"),
        menuItem("Análise Geral", tabName = "tab_cruzada"),
        menuItem("Modelos", tabName = "modelos"),
        menuItem("Análise com Smote", tabName = "anal_var"),
        menuItem("Análise com Downsample", tabName = "anal_downsample")
        # menuItem("Comparação OR Smote", tabName = "comp_or_smote"),
        # menuItem("Comparação OR Down-Sample", tabName = "comp_or_downsample")
        # menuItem("Mapas", tabName = "mps_casos")
      )
    ),
    ### Item Inicio ----
    dashboardBody(
      tags$head(tags$style(
        HTML(
          '
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #0A1E3C;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #0A1E3C;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #0A1E3C;
                              }

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #0A1E3C;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #32A0FF;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #0A1E3C;
                              color: #FFFFFF;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #32A0FF;
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #32A0FF;
                              }
                              '
        ),
        HTML("hr {border-top: 1px solid #0A1E3C;}")
      )),
      tabItems(
        ### Item Análise cruzada ----
        tabItem(tabName = "tab_cruzada",
                fluidRow(
                  box(
                    collapsible = TRUE,
                    width = 4,
                    title = "Selecione",
                    status = "primary",
                    solidHeader = FALSE,
                    h4(strong("Variáveis de interesse")),
                    selectInput(
                      inputId = "caracteristicas1",
                      label = "Característica na linha:",
                      choices = c(
                        "Momento gestacional" = "classi_gesta_puerp",
                        "Região do Brasil"  = "region",
                        "UF do Brasil"  = "SG_UF",
                        "Raça" = "raca",
                        "Escolaridade" = "escol",
                        "Escolaridade1" = "escol1",
                        "Faixa etária" = "faixa_et",
                        "Mudança município" = "mudou_muni",
                        "Zona da residência" = "zona",
                        "SG para SRAG" = "sg_para_srag",
                        "Infecção no hospital" = "inf_inter",
                        "Contato com suíno" = "cont_ave_suino",
                        "Vacina contra gripe" = "vacina",
                        "Antiviral" = "antiviral",
                        "Febre" = "febre",
                        "Tosse" = "tosse",
                        "Garganta" = "garganta",
                        "Dispinéia" = "dispneia",
                        "Desconforto respiratório" = "desc_resp",
                        "Saturação" = "saturacao",
                        "Diarréia" = "diarreia",
                        "Vômito" = "vomito",
                        "Dor abdominal" = "dor_abd",
                        "Fadiga" = "fadiga",
                        "Perda olfativa" = "perd_olft",
                        "Perda paladar" = "perd_pala",
                        "Cardiovascular" = "cardiopati",
                        "Hematológica" = "hematologi",
                        "Hepática" = "hepatica",
                        "Asma" = "asma",
                        "Diabetes" = "diabetes",
                        "Neuropatia" = "neuro",
                        "Pneumopatia" = "pneumopati",
                        "Imunodepressão" = "imunodepre",
                        "Renal" = "renal",
                        "Obesidade" = "obesidade",
                        "UTI" = "uti",
                        "Suporte ventilatório" = "suport_ven",
                        "Intubação S/N" = "intubacao_SN",
                        "Evolução" = "evolucao",
                        "Vacina Covid" = "vacina_cov1"
                      ),
                      selected = "evolucao",
                      width = "220px"
                    ),
                    # selectInput(
                    #   inputId = "caracteristicas2",
                    #   label = "Característica na coluna:",
                    #   choices = c(
                    #     "Ano do caso" = "ano",
                    #     "Diagnóstico COVID-19" = "classi_covid",
                    #     "Momento gestacional" = "classi_gesta_puerp",
                    #     "Região do Brasil"  = "region",
                    #     "UF do Brasil"  = "SG_UF",
                    #     "Raça" = "raca",
                    #     "Escolaridade" = "escol",
                    #     "Faixa etária" = "faixa_et",
                    #     "Histórico de viagem" = "hist_viagem",
                    #     "Mudança município" = "mudou_muni",
                    #     "Zona da residência" = "zona",
                    #     "SG para SRAG" = "sg_para_srag",
                    #     "Infecção no hospital" = "inf_inter",
                    #     "Contato com suíno" = "cont_ave_suino",
                    #     "Vacina contra gripe" = "vacina",
                    #     "Antiviral" = "antiviral",
                    #     "Internação" = "hospital",
                    #     "Febre" = "febre",
                    #     "Tosse" = "tosse",
                    #     "Garganta" = "garganta",
                    #     "Dispinéia" = "dispneia",
                    #     "Desconforto respiratório" = "desc_resp",
                    #     "Saturação" = "saturacao",
                    #     "Diarréia" = "diarreia",
                    #     "Vômito" = "vomito",
                    #     "Dor abdominal" = "dor_abd",
                    #     "Fadiga" = "fadiga",
                    #     "Perda olfativa" = "perd_olft",
                    #     "Perda paladar" = "perd_pala",
                    #     "Cardiovascular" = "cardiopati",
                    #     "Hematológica" = "hematologi",
                    #     "Hepática" = "hepatica",
                    #     "Asma" = "asma",
                    #     "Diabetes" = "diabetes",
                    #     "Neuropatia" = "neuro",
                    #     "Pneumopatia" = "pneumopati",
                    #     "Imunodepressão" = "imunodepre",
                    #     "Renal" = "renal",
                    #     "Obesidade" = "obesidade",
                    #     "UTI" = "uti",
                    #     "Suporte ventilatório" = "suport_ven",
                    #     "Evolução" = "evolucao"
                    #   ),
                    #   selected = "evolucao",
                    #   width = "220px"
                    # ),
                    strong("Selecionar só casos válidos?"),
                    checkboxInput("na1", "Excluir casos faltantes?",
                                  value = TRUE),
                    hr(),
                    h4(strong(
                      "Variantes"
                    )),
                    checkboxGroupInput(
                      inputId = "classivariante",
                      label = "Tipo de Variante de COVID-19:",
                      choices = c(
                        "Original" = "original",
                        "Gama" = "gama",
                        "Delta" = "delta",
                        "Omicron" = "omicron"
                      ),
                      selected = c("original","gama","delta","omicron")
                    ),
                    hr(),
                    h4(strong(
                      "Características gestantes e/ou puérperas"
                    )),
                    sliderInput(
                      inputId = "idade1",
                      label = "Intervalo de idade:",
                      min = min(dados5$idade_anos),
                      max = max(dados5$idade_anos),
                      value = c(min(dados5$idade_anos), max(dados5$idade_anos))
                    ),
                    checkboxGroupInput(
                      inputId = "GestantePuerpera1",
                      label = "Idade gestacional/puérpera:",
                      choices = c(
                        "1° trimestre" = "1tri",
                        "2° trimestre" = "2tri",
                        "3° trimestre" = "3tri",
                        "Idade gestacional ignorada" = "IG_ig",
                        "Puérpera" = "puerp"
                      ),
                      selected = c("1tri", "2tri", "3tri", "IG_ig", "puerp")
                    ),
                    #hr(),
                    # h4(strong("Diagnóstico")),
                    # checkboxGroupInput(
                    #   inputId = "classiCovid1",
                    #   label = "Tipo de diagnóstico de COVID:",
                    #   choices = c(
                    #     "PCR" = "pcr",
                    #     "Antigênio" = "antigenio",
                    #     "Sorologia" = "sorologia",
                    #     "Outro" = "outro",
                    #     "Não confirmado ou não COVID-19" = "não"
                    #   ),
                    #   selected = c("pcr", "antigenio", "sorologia", "outro", "não")
                    # ),
                    hr(),
                    h4(strong("Vacina")),
                    checkboxGroupInput(
                      inputId = "vacinacov",
                      label = "Tomou a vacina?",
                      choices = c(
                        "Sim" = "sim",
                        "Não" = "não",
                        "Não informado" = "nao informado"
                      ),
                      selected = c("sim","não","nao informado")
                    )
                    # checkboxGroupInput(
                    #   inputId = "dosescov",
                    #   label = "Tomou quantas doses?:",
                    #   choices = c(
                    #     "Pelo menos uma dose" = "pelo menos uma dose",
                    #     "Duas doses" = "duas doses",
                    #     "Não informado" = "não informado"
                    #   ),
                    #   selected = c("pelo menos uma dose","duas doses","não informado")
                    # )
                  ),
                  box(
                    width = 8,
                    status = "primary",
                    div(tabsetPanel(
                      tabPanel("Tabela Cruzada",
                               highcharter::highchartOutput("plot11"),
                               verbatimTextOutput("table1"),
                               h3(strong("Teste de Fisher")),
                               verbatimTextOutput("print1")),
                      tabPanel("Teste de Breslow-Day",
                               verbatimTextOutput("print4"))
                      
                      # tabPanel("Análise dados com smote",
                      #          ),
                      # tabPanel("Análise dados com down-sample",
                      #          highcharter::highchartOutput("plot12"),
                      #          p(
                      #            "O gráfico acima indica a porcentagem de causas da variável selecionada (ex: 'óbito' para evolução, 'invasivo' para Intubação, 'sim' para febre) em cada variante no grupo vacinado e não vacinado"
                      #          ))
                    )),
                    h3(strong("Observação")),
                    p(
                      "<NA> na tabela acima indica os casos faltantes ou ignorados (não resposta) das variáveis em questão.
                  No gráfico, essa informação aparece na categoria com número (por exemplo, número 2)."
                    ),
                  p(
                    "Caso queira só analisar os casos válidos (sem considerar os casos faltantes), no canto superior esquerdo em 'Selecionar só casos válidos?',
                selecione o botão 'Excluir casos faltantes?'."
                  ),
                p(
                  "O Teste de Breslow-Day acima é analisado condicionando as variantes com os grupos Gestantes ou Puérperas e a variável selecionada"
                )
                
                  )
                )),
        ### Item Análise smote ----
        tabItem(tabName = "anal_var",
                fluidRow(
                  box(
                    collapsible = TRUE,
                    width = 4,
                    title = "Selecione",
                    status = "primary",
                    solidHeader = FALSE,
                    h4(strong("Variáveis de interesse")),
                    selectInput(
                      inputId = "caracteristicas2",
                      label = "Característica na linha:",
                      choices = c(
                        "Momento gestacional" = "classi_gesta_puerp",
                        "Região do Brasil"  = "region",
                        "UF do Brasil"  = "SG_UF",
                        "Raça" = "raca",
                        "Escolaridade" = "escol",
                        "Escolaridade1" = "escol1",
                        "Faixa etária" = "faixa_et",
                        "Mudança município" = "mudou_muni",
                        "Zona da residência" = "zona",
                        "SG para SRAG" = "sg_para_srag",
                        "Infecção no hospital" = "inf_inter",
                        "Contato com suíno" = "cont_ave_suino",
                        "Vacina contra gripe" = "vacina",
                        "Antiviral" = "antiviral",
                        "Febre" = "febre",
                        "Tosse" = "tosse",
                        "Garganta" = "garganta",
                        "Dispinéia" = "dispneia",
                        "Desconforto respiratório" = "desc_resp",
                        "Saturação" = "saturacao",
                        "Diarréia" = "diarreia",
                        "Vômito" = "vomito",
                        "Dor abdominal" = "dor_abd",
                        "Fadiga" = "fadiga",
                        "Perda olfativa" = "perd_olft",
                        "Perda paladar" = "perd_pala",
                        "Cardiovascular" = "cardiopati",
                        "Hematológica" = "hematologi",
                        "Hepática" = "hepatica",
                        "Asma" = "asma",
                        "Diabetes" = "diabetes",
                        "Neuropatia" = "neuro",
                        "Pneumopatia" = "pneumopati",
                        "Imunodepressão" = "imunodepre",
                        "Renal" = "renal",
                        "Obesidade" = "obesidade",
                        "UTI" = "uti",
                        "Suporte ventilatório" = "suport_ven",
                        "Intubação S/N" = "intubacao_SN",
                        "Evolução" = "evolucao",
                        "Vacina Covid"="vacina_cov1"
                      ),
                      selected = "evolucao",
                      width = "220px"
                    ),
                    hr(),
                    h4(strong(
                      "Variantes"
                    )),
                    checkboxGroupInput(
                      inputId = "classivariante2",
                      label = "Tipo de Variante de COVID-19:",
                      choices = c(
                        "Original" = "original",
                        "Gama" = "gama",
                        "Delta" = "delta",
                        "Omicron" = "omicron"
                      ),
                      selected = c("original","gama","delta","omicron")
                    ),
                    hr(),
                    h4(strong(
                      "Características gestantes e/ou puérperas"
                    )),
                    sliderInput(
                      inputId = "idade2",
                      label = "Intervalo de idade:",
                      min = min(dados5$idade_anos),
                      max = max(dados5$idade_anos),
                      value = c(min(dados5$idade_anos), max(dados5$idade_anos))
                    ),
                    checkboxGroupInput(
                      inputId = "GestantePuerpera2",
                      label = "Idade gestacional/puérpera:",
                      choices = c(
                        "1° trimestre" = "1tri",
                        "2° trimestre" = "2tri",
                        "3° trimestre" = "3tri",
                        "Idade gestacional ignorada" = "IG_ig",
                        "Puérpera" = "puerp"
                      ),
                      selected = c("1tri", "2tri", "3tri", "IG_ig", "puerp")
                    ),
                    hr(),
                    h4(strong("Vacina")),
                    checkboxGroupInput(
                      inputId = "vacinacov2",
                      label = "Tomou a vacina?",
                      choices = c(
                        "Sim" = "sim",
                        "Não" = "não",
                        "Não informado" = "nao informado"
                      ),
                      selected = c("sim","não","nao informado")
                    )
                    # h4(strong("Diagnóstico")),
                    # checkboxGroupInput(
                    #   inputId = "classiCovid2",
                    #   label = "Tipo de diagnóstico de COVID:",
                    #   choices = c(
                    #     "PCR" = "pcr",
                    #     "Antigênio" = "antigenio",
                    #     "Sorologia" = "sorologia",
                    #     "Outro" = "outro",
                    #     "Não confirmado ou não COVID-19" = "não"
                    #   ),
                    #   selected = c("pcr", "antigenio", "sorologia", "outro", "não")
                    # ),
                    # h4(strong("Vacina")),
                    # checkboxGroupInput(
                    #   inputId = "vacinacov2",
                    #   label = "Tomou a vacina?:",
                    #   choices = c(
                    #     "Sim" = "sim",
                    #     "Não" = "não",
                    #     "Não informado" = "nao informado"
                    #   ),
                    #   selected = c("sim","não","nao informado")
                    # )
                    # checkboxGroupInput(
                    #   inputId = "dosescov2",
                    #   label = "Tomou quantas doses?:",
                    #   choices = c(
                    #     "Pelo menos uma dose" = "pelo menos uma dose",
                    #     "Duas doses" = "duas doses",
                    #     "Não informado" = "não informado"
                    #   ),
                    #   selected = c("pelo menos uma dose","duas doses","não informado")
                    # )
                  ),
                  box(
                    width = 8,
                    status = "primary",
                    div(tabsetPanel(
                      tabPanel("Tabela Cruzada",
                               highcharter::highchartOutput("plot21"),
                               verbatimTextOutput("table2"),
                               h3(strong("Teste de Fisher")),
                               verbatimTextOutput("print2")),
                      tabPanel("Teste de Breslow-Day",
                               verbatimTextOutput("print5"))
                    )),
                    h3(strong("Observação")),
                    p(
                      "<NA> na tabela acima indica os casos faltantes ou ignorados (não resposta) das variáveis em questão.
                  No gráfico, essa informação aparece na categoria com número (por exemplo, número 2)."
                    ),
                  p(
                    "Caso queira só analisar os casos válidos (sem considerar os casos faltantes), no canto superior esquerdo em 'Selecionar só casos válidos?',
                selecione o botão 'Excluir casos faltantes?'."
                  ),
                p(
                  "O Teste de Breslow-Day acima é analisado condicionando as variantes com os grupos Gestantes ou Puérperas e a variável selecionada"
                )
                
                
                  )
                )
                
        ),
        ### Item Análise por variante ----
        tabItem(tabName = "anal_downsample",
                fluidRow(
                  box(
                    collapsible = TRUE,
                    width = 4,
                    title = "Selecione",
                    status = "primary",
                    solidHeader = FALSE,
                    h4(strong("Variáveis de interesse")),
                    selectInput(
                      inputId = "caracteristicas3",
                      label = "Característica na linha:",
                      choices = c(
                        "Momento gestacional" = "classi_gesta_puerp",
                        "Região do Brasil"  = "region",
                        "UF do Brasil"  = "SG_UF",
                        "Raça" = "raca",
                        "Escolaridade" = "escol",
                        "Escolaridade1" = "escol1",
                        "Faixa etária" = "faixa_et",
                        "Mudança município" = "mudou_muni",
                        "Zona da residência" = "zona",
                        "SG para SRAG" = "sg_para_srag",
                        "Infecção no hospital" = "inf_inter",
                        "Contato com suíno" = "cont_ave_suino",
                        "Vacina contra gripe" = "vacina",
                        "Antiviral" = "antiviral",
                        "Febre" = "febre",
                        "Tosse" = "tosse",
                        "Garganta" = "garganta",
                        "Dispinéia" = "dispneia",
                        "Desconforto respiratório" = "desc_resp",
                        "Saturação" = "saturacao",
                        "Diarréia" = "diarreia",
                        "Vômito" = "vomito",
                        "Dor abdominal" = "dor_abd",
                        "Fadiga" = "fadiga",
                        "Perda olfativa" = "perd_olft",
                        "Perda paladar" = "perd_pala",
                        "Cardiovascular" = "cardiopati",
                        "Hematológica" = "hematologi",
                        "Hepática" = "hepatica",
                        "Asma" = "asma",
                        "Diabetes" = "diabetes",
                        "Neuropatia" = "neuro",
                        "Pneumopatia" = "pneumopati",
                        "Imunodepressão" = "imunodepre",
                        "Renal" = "renal",
                        "Obesidade" = "obesidade",
                        "UTI" = "uti",
                        "Suporte ventilatório" = "suport_ven",
                        "Intubação S/N" = "intubacao_SN",
                        "Evolução" = "evolucao",
                        "Vacina Covid"="vacina_cov1"
                      ),
                      selected = "evolucao",
                      width = "220px"
                    ),
                    hr(),
                    h4(strong(
                      "Variantes"
                    )),
                    checkboxGroupInput(
                      inputId = "classivariante3",
                      label = "Tipo de Variante de COVID-19:",
                      choices = c(
                        "Original" = "original",
                        "Gama" = "gama",
                        "Delta" = "delta",
                        "Omicron" = "omicron"
                      ),
                      selected = c("original","gama","delta","omicron")
                    ),
                    hr(),
                    h4(strong(
                      "Características gestantes e/ou puérperas"
                    )),
                    sliderInput(
                      inputId = "idade3",
                      label = "Intervalo de idade:",
                      min = min(dados5$idade_anos),
                      max = max(dados5$idade_anos),
                      value = c(min(dados5$idade_anos), max(dados5$idade_anos))
                    ),
                    checkboxGroupInput(
                      inputId = "GestantePuerpera3",
                      label = "Idade gestacional/puérpera:",
                      choices = c(
                        "1° trimestre" = "1tri",
                        "2° trimestre" = "2tri",
                        "3° trimestre" = "3tri",
                        "Idade gestacional ignorada" = "IG_ig",
                        "Puérpera" = "puerp"
                      ),
                      selected = c("1tri", "2tri", "3tri", "IG_ig", "puerp")
                    ),
                    hr(),
                    h4(strong("Vacina")),
                    checkboxGroupInput(
                      inputId = "vacinacov3",
                      label = "Tomou a vacina?",
                      choices = c(
                        "Sim" = "sim",
                        "Não" = "não",
                        "Não informado" = "nao informado"
                      ),
                      selected = c("sim","não","nao informado")
                    )
                    # h4(strong("Diagnóstico")),
                    # checkboxGroupInput(
                    #   inputId = "classiCovid2",
                    #   label = "Tipo de diagnóstico de COVID:",
                    #   choices = c(
                    #     "PCR" = "pcr",
                    #     "Antigênio" = "antigenio",
                    #     "Sorologia" = "sorologia",
                    #     "Outro" = "outro",
                    #     "Não confirmado ou não COVID-19" = "não"
                    #   ),
                    #   selected = c("pcr", "antigenio", "sorologia", "outro", "não")
                    # ),
                    # h4(strong("Vacina")),
                    # checkboxGroupInput(
                    #   inputId = "vacinacov3",
                    #   label = "Tomou a vacina?:",
                    #   choices = c(
                    #     "Sim" = "sim",
                    #     "Não" = "não",
                    #     "Não informado" = "nao informado"
                    #   ),
                    #   selected = c("sim","não","nao informado")
                    # )
                    # checkboxGroupInput(
                    #   inputId = "dosescov2",
                    #   label = "Tomou quantas doses?:",
                    #   choices = c(
                    #     "Pelo menos uma dose" = "pelo menos uma dose",
                    #     "Duas doses" = "duas doses",
                    #     "Não informado" = "não informado"
                    #   ),
                    #   selected = c("pelo menos uma dose","duas doses","não informado")
                    # )
                  ),
                  box(
                    width = 8,
                    status = "primary",
                    div(tabsetPanel(
                      tabPanel("Tabela Cruzada",
                               highcharter::highchartOutput("plot31"),
                               verbatimTextOutput("table3"),
                               h3(strong("Teste de Fisher")),
                               verbatimTextOutput("print3")
                      ),
                      tabPanel("Teste de Breslow-Day",
                               verbatimTextOutput("print6"))
                    )),
                    h3(strong("Observação")),
                    p(
                      "<NA> na tabela acima indica os casos faltantes ou ignorados (não resposta) das variáveis em questão.
                  No gráfico, essa informação aparece na categoria com número (por exemplo, número 2)."
                    ),
                  p(
                    "Caso queira só analisar os casos válidos (sem considerar os casos faltantes), no canto superior esquerdo em 'Selecionar só casos válidos?',
                selecione o botão 'Excluir casos faltantes?'."
                  )
                  )
                )
                
        ),
        tabItem(tabName = "modelos",
                fluidRow(
                  box(
                    collapsible = TRUE,
                    width = 4,
                    title = "Selecione",
                    status = "primary",
                    solidHeader = FALSE,
                    h4(strong("Variáveis de interesse")),
                    selectInput(
                      inputId = "caracteristicas4",
                      label = "Característica na linha:",
                      choices = c(
                        "Febre" = "febre",
                        "Tosse" = "tosse",
                        "Garganta" = "garganta",
                        "Dispinéia" = "dispneia",
                        "Desconforto respiratório" = "desc_resp",
                        "Saturação" = "saturacao",
                        "Diarréia" = "diarreia",
                        "Vômito" = "vomito",
                        "Dor abdominal" = "dor_abd",
                        "Fadiga" = "fadiga",
                        "Perda olfativa" = "perd_olft",
                        "Perda paladar" = "perd_pala",
                        "UTI" = "uti",
                        "Suporte ventilatório" = "suport_ven",
                        "Intubação S/N" = "intubacao_SN",
                        "Evolução" = "evolucao"
                      ),
                      selected = "evolucao",
                      width = "220px"
                    ),
                    hr(),
                    h4(strong(
                      "Variantes"
                    )),
                    sliderInput(
                      inputId = "idade4",
                      label = "Intervalo de idade:",
                      min = min(dados5$idade_anos),
                      max = max(dados5$idade_anos),
                      value = c(min(dados5$idade_anos), max(dados5$idade_anos))
                    )
                  ),
                  box(
                    width = 8,
                    status = "primary",
                    div(tabsetPanel(
                      tabPanel("Modelo logístico normal",
                               verbatimTextOutput("print7"),
                               h3(strong("Teste de Hosmer-Lemewshow")),
                               verbatimTextOutput("print9"),
                               h3(strong("Gráfico de Envelope")),
                               plotOutput("plot4")),
                      tabPanel("Modelo logístico com vacina*variante",
                               verbatimTextOutput("print8"),
                               h3(strong("Teste de Hosmer-Lemewshow")),
                               verbatimTextOutput("print10"),
                               h3(strong("Gráfico de Envelope")),
                               plotOutput("plot5"))
                      
                      # tabPanel("Análise dados com smote",
                      #          ),
                      # tabPanel("Análise dados com down-sample",
                      #          highcharter::highchartOutput("plot12"),
                      #          p(
                      #            "O gráfico acima indica a porcentagem de causas da variável selecionada (ex: 'óbito' para evolução, 'invasivo' para Intubação, 'sim' para febre) em cada variante no grupo vacinado e não vacinado"
                      #          ))
                    ))
                  )
                ))
      )))

# Server ----
server <- function(input, output, session) {
  
  ## base de dados com filtragem por inputs ----
  
  selectData2 <- reactive({
    dados5 %>%
      dplyr::filter(idade_anos >= input$idade1[1]) %>%
      dplyr::filter(idade_anos <= input$idade1[2]) %>%
      dplyr::filter(classi_gesta_puerp %in% input$GestantePuerpera1) %>%
      dplyr::filter(vacina_cov_sel %in% input$vacinacov) %>%
      dplyr::filter(variante %in% input$classivariante) %>%
      {
        if (input$na1 == TRUE)
          dplyr::filter(., !is.na(get(input$caracteristicas1)))
        else
          dplyr::filter(., (!is.na(get(
            input$caracteristicas1
          )) | is.na(get(
            input$caracteristicas1
          ))))
      }
  })
  
  ### Gráfico e tabela cruzada ----
  dados_hc_aux <- reactive({
    selectData2() %>%
      count(var = .[["grupos"]]) %>%
      mutate(ntot = n) %>%
      select(-n)
  })
  
  dados_hc <- reactive({
    selectData2() %>%
      count(var = .[["grupos"]],
            var2 = .[[input$caracteristicas1]]) %>%
      full_join(dados_hc_aux(), by = "var") %>%
      mutate(porc = round((n / ntot) * 100, 2))
  })
  
  output$plot11 <- highcharter::renderHighchart({
    hchart(dados_hc(), type = "column",
           hcaes(x = var,
                 y = porc, group = var2)) %>%
      hc_xAxis(title = list(text = "Gestante ou Puérpra")) %>%
      hc_yAxis(title = list(text = "%")) %>%
      hc_add_theme(hc_theme_elementary())
  })
  
  
  output$table1 <- renderPrint({
    st_options(headings = FALSE, display.labels = FALSE)
    with(
      selectData2(),
      summarytools::ctable(
        get(input$caracteristicas1),
        grupos,
        prop = "c",
        headings = st_options("headings"),
        display.labels = st_options("display.labels"),
        useNA = "ifany",
        dnn = c(input$caracteristicas1, "Gestante ou Puérpera"),
        OR = TRUE,
        chisq = TRUE
      )
    )
  })
  
  output$print1 <- renderPrint({
    with(selectData2(),
         fisher.test(grupos,get(input$caracteristicas1),simulate.p.value = TRUE))
  })
  
  #selecao para o Teste de Breslow-Day
  selectData22 <- reactive({
    dados5 %>%
      dplyr::filter(idade_anos >= input$idade1[1]) %>%
      dplyr::filter(idade_anos <= input$idade1[2]) %>%
      dplyr::filter(classi_gesta_puerp %in% input$GestantePuerpera1) %>%
      dplyr::filter(vacina_cov_sel %in% input$vacinacov | variante == "original") %>%
      {
        if (input$na1 == TRUE)
          dplyr::filter(., !is.na(get(input$caracteristicas1)))
        else
          dplyr::filter(., (!is.na(get(
            input$caracteristicas1
          )) | is.na(get(
            input$caracteristicas1
          ))))
      }
  })
  
  output$print4 <- renderPrint({
    teste_breslowday(
      selectData22() %>% filter(variante == "original"),
      selectData22() %>% filter(variante == "gama"),
      selectData22() %>% filter(variante == "delta"),
      selectData22() %>% filter(variante == "omicron"),
      input$caracteristicas1
    )
  })
  
  
  ## Modelos 
  
  selectData5 <- reactive({
    dados5 %>%
      dplyr::filter(idade_anos >= input$idade4[1]) %>%
      dplyr::filter(idade_anos <= input$idade4[2])
  })
  
  #Logístico 
  
  output$print7 <- renderPrint({
    summary(glm(data = selectData5(), as.factor(get(input$caracteristicas4)) ~ grupos + vacina_cov1 + variante, family = binomial))
  })
  
  #Logístico com variável vacinacov_variante2 (vacina * variante)
  
  output$print8 <- renderPrint({
    summary(glm(data = selectData5(), as.factor(get(input$caracteristicas4)) ~ grupos + vacinacov_variante2, family = binomial))
  })
  
  output$print9 <- renderPrint({
    hltest(glm(data = selectData5(), as.factor(get(input$caracteristicas4)) ~ grupos + vacina_cov1 + variante, family = binomial))
  })
  
  output$print10 <- renderPrint({
    hltest(glm(data = selectData5(), as.factor(get(input$caracteristicas4)) ~ grupos + vacinacov_variante2, family = binomial))
  })
  
  output$plot4 <- renderPlot({
    hnp::hnp(glm(data = selectData5(), as.factor(get(input$caracteristicas4)) ~ grupos + vacina_cov1 + variante, family = binomial))
  })
  
  output$plot5 <- renderPlot({
    hnp::hnp(glm(data = selectData5(), as.factor(get(input$caracteristicas4)) ~ grupos + vacinacov_variante2, family = binomial))
  })
  
  
  ## base de dados com filtragem por inputs para analise por variante  smote ----
  
  selectData_smote <- reactive({
    dados5 %>%
      dplyr::filter(idade_anos >= input$idade2[1]) %>%
      dplyr::filter(idade_anos <= input$idade2[2]) %>% 
      dplyr::filter(classi_gesta_puerp %in% input$GestantePuerpera2) #%>% 
    # dplyr::filter(vacina_cov_sel %in% input$vacinacov2) 
  })
  
  ### Gráfico e tabela cruzada Original ----
  
  selectData3 <- reactive({
    dados_smote(selectData_smote(),input$caracteristicas2) %>% 
      dplyr::filter(variante %in% input$classivariante2)  %>%
      dplyr::filter(vacina_cov_sel %in% input$vacinacov2) 
  })
  
  dados_hc_aux2 <- reactive({
    selectData3() %>%
      count(var = .[["grupos"]]) %>%
      mutate(ntot = n) %>%
      select(-n)
  })
  
  dados_hc2 <- reactive({
    selectData3() %>%
      count(var = .[["grupos"]],
            var2 = .[[input$caracteristicas2]]) %>%
      full_join(dados_hc_aux2(), by = "var") %>%
      mutate(porc = round((n / ntot) * 100, 2))
  })
  
  output$plot21 <- highcharter::renderHighchart({
    hchart(dados_hc2(), type = "column",
           hcaes(x = var,
                 y = porc, group = var2)) %>%
      hc_xAxis(title = list(text = "Gestante ou Puérpera")) %>%
      hc_yAxis(title = list(text = "%")) %>%
      hc_add_theme(hc_theme_elementary())
  })
  
  
  output$table2 <- renderPrint({
    st_options(headings = FALSE, display.labels = FALSE)
    with(
      selectData3(),
      summarytools::ctable(
        get(input$caracteristicas2),
        grupos,
        prop = "c",
        headings = st_options("headings"),
        display.labels = st_options("display.labels"),
        useNA = "ifany",
        dnn = c(input$caracteristicas2, "Gestante ou Puérpera"),
        OR = TRUE,
        chisq = TRUE
      )
    )
  })
  
  output$print2 <- renderPrint({
    with(selectData3(),
         fisher.test(grupos,get(input$caracteristicas2),simulate.p.value = TRUE))
  })
  
  
  
  ### selecao para o Teste de Breslow-Day
  selectData33 <- reactive({
    dados_smote(selectData_smote(),input$caracteristicas2) %>%  
      dplyr::filter(vacina_cov_sel %in% input$vacinacov2 | variante == "original") 
  })
  
  output$print5 <- renderPrint({
    teste_breslowday(
      selectData33() %>% filter(variante == "original"),
      selectData33() %>% filter(variante == "gama"),
      selectData33() %>% filter(variante == "delta"),
      selectData33() %>% filter(variante == "omicron"),
      input$caracteristicas2
    )
  })
  
  ## base de dados com filtragem por inputs para analise downsample ----
  
  selectData_dowsample <- reactive({
    dados5 %>%
      dplyr::filter(idade_anos >= input$idade3[1]) %>%
      dplyr::filter(idade_anos <= input$idade3[2]) %>% 
      dplyr::filter(classi_gesta_puerp %in% input$GestantePuerpera3) #%>% 
    # dplyr::filter(vacina_cov_sel %in% input$vacinacov3) 
  })
  
  ### Gráfico e tabela cruzada Original ----
  
  selectData4 <- reactive({
    dados_downsample(selectData_dowsample(),input$caracteristicas3) %>% 
      dplyr::filter(variante %in% input$classivariante3) %>%
      dplyr::filter(vacina_cov_sel %in% input$vacinacov3) 
  })
  
  dados_hc_aux3 <- reactive({
    selectData4() %>%
      count(var = .[["grupos"]]) %>%
      mutate(ntot = n) %>%
      select(-n)
  })
  
  dados_hc3 <- reactive({
    selectData4() %>%
      count(var = .[["grupos"]],
            var2 = .[[input$caracteristicas3]]) %>%
      full_join(dados_hc_aux3(), by = "var") %>%
      mutate(porc = round((n / ntot) * 100, 2))
  })
  
  output$plot31 <- highcharter::renderHighchart({
    hchart(dados_hc3(), type = "column",
           hcaes(x = var,
                 y = porc, group = var2)) %>%
      hc_xAxis(title = list(text = "Gestante ou Puérpera")) %>%
      hc_yAxis(title = list(text = "%")) %>%
      hc_add_theme(hc_theme_elementary())
  })
  
  
  output$table3 <- renderPrint({
    st_options(headings = FALSE, display.labels = FALSE)
    with(
      selectData4(),
      summarytools::ctable(
        get(input$caracteristicas3),
        grupos,
        prop = "c",
        headings = st_options("headings"),
        display.labels = st_options("display.labels"),
        useNA = "ifany",
        dnn = c(input$caracteristicas3, "Gestante ou Puérpera"),
        OR = TRUE,
        chisq = TRUE
      )
    )
  })
  
  output$print3 <- renderPrint({
    with(selectData4(),
         fisher.test(grupos,get(input$caracteristicas3),simulate.p.value = TRUE))
  })
  
  ### selecao para o Teste de Breslow-Day
  
  selectData44 <- reactive({
    dados_downsample(selectData_dowsample(),input$caracteristicas3) %>%  
      dplyr::filter(vacina_cov_sel %in% input$vacinacov3 | variante == "original") 
  })
  
  output$print6 <- renderPrint({
    teste_breslowday(
      selectData44() %>% filter(variante == "original"),
      selectData44() %>% filter(variante == "gama"),
      selectData44() %>% filter(variante == "delta"),
      selectData44() %>% filter(variante == "omicron"),
      input$caracteristicas3
    )
  })
  
  
  # Gather all the form inputs (and add timestamp)
  formData <- reactive({
    data <- sapply(fieldsAll, function(x)
      input[[x]])
    data <- c(data, timestamp = humanTime())
    data <- t(data)
    data
  })
  
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit, {
    # User-experience stuff
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    # Save the data (show an error message in case of error)
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error",
                    anim = TRUE,
                    animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  
  # submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })
  
}

shinyApp(ui, server)
