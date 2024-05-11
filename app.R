library(rsconnect)
library(shiny)
library(shinythemes)
library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(DT)
library(png)
library(ggiraph)
library(RColorBrewer)
library(readr)
library(shinyWidgets)
library(shinymanager)
library(formattable)
library(writexl)
library(WriteXLS)
library(rpivotTable)
library(htmlwidgets)
library(shinyjs)
library(clipr)
library(rvest)
library(shinydashboard)
library(ggrepel)
library(readxl)
library(plotly)
library(shinyalert)
options(shiny.host='0.0.0.0')
options(shiny.port=80)
inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

credentials <- data.frame(
  user = c("1", "84066361", "victor", "benoit"),
  password = c("1", "0000", "12345", "azerty"),
  comment = c("alsace", "auvergne", "bretagne","dddd"), 
  stringsAsFactors = FALSE
)
library(readxl)
library(dplyr)
library (plotly)
library(rsconnect)



ui <- 
  secure_app(head_auth = tags$script(inactivity),
  
  dashboardPage(
  dashboardHeader(title = "Sélectivité douanière-Bénin-)"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Télechargement", tabName = "files",
               icon = icon("download"),
               fileInput("file1", "Fichier déclarations XLSX"),
               fileInput("file2", "Fichier AVD XLSX"),
               fileInput("file3", "Fichier droits Compromis XLSX")
               ),
      # Onglet "Informations Générales"
      menuItem("Informations Générales", tabName = "general_info",
               icon = icon("info")),
      # Onglet "Analyse de Sélectivités"
      menuItem("Analyse de Sélectivités", tabName = "selectivity_analysis",
               menuSubItem("Données", tabName = "selectivity_data"),
               menuSubItem("Graphique", tabName = "selectivity_graph")),
      # Onglet "Analyse Opérateur"
      menuItem("Analyse Opérateur", tabName = "operator_analysis",
               menuSubItem("Données", tabName = "operator_data"),
               menuSubItem("Details", tabName = "operator_details")),
      # Onglet "Analyse NomenclAture"
      menuItem("Analyse Nomenclature", tabName = "nomenclature_analysis",
               menuSubItem("Données", tabName = "nomenclature_data"),
               menuSubItem("Details", tabName = "nomenclature_details")),
      # Onglet "Analyse NomenclAture"
      menuItem("Analyse Origine", tabName = "origine_analysis",
               menuSubItem("Données", tabName = "origine_data"),
               menuSubItem("Details", tabName = "origine_details"))
    ),
    
    
    # Filtrage des données
    uiOutput("filter_ui")
    
  ),
  dashboardBody(
    tabItems(
      # Onglet "Informations Générales"
      tabItem(
        tabName = "general_info",
        fluidRow(
          box(
            title = "Informations Générales",solidHeader = TRUE,
            collapsible = TRUE,
            # Insérer ici le graphique Plotly
            plotlyOutput("bur_chart", height = "100vh"),
            width = 12  # Utiliser une largeur de 12 pour occuper toute la largeur de la ligne
          ),
          box(
            title = "Informations Générales",solidHeader = TRUE,
            collapsible = TRUE,
            # Insérer ici le graphique Plotly
            dataTableOutput("Nbre_dec_regime", height = "100vh"),
            width = 12  # Utiliser une largeur de 12 pour occuper toute la largeur de la ligne
          ),
        )
      ),
      # Onglet "Analyse de Sélectivités"
      tabItem(
        tabName = "selectivity_data",
        fluidRow(
          box(title = "Tableau de Données - Analyse de Sélectivités", width = 12,
              dataTableOutput("data_table_selectivity")
          )
        )
      ),
      tabItem(
        tabName = "selectivity_graph",
        fluidRow(
          # A static valueBox
          box(title = "Taux de Détection par Circuit Précédent", width = 12,
              valueBoxOutput("taux_detection_rouge", width = 2),
              valueBoxOutput("taux_detection_vert", width = 2),
              valueBoxOutput("taux_detection_jaune", width = 2),
              valueBoxOutput("taux_detection_bleu", width = 2),
              valueBoxOutput("total_droits_compromis", width = 3),
              valueBoxOutput("total_reajustement_valeur", width = 2),
          ),
          # Dynamic valueBoxes
          valueBoxOutput("progressBox"),
          valueBoxOutput("approvalBox"),
          box(title = "Graphique de l'Analyse de Sélectivités", width = 12,solidHeader = TRUE,
              collapsible = TRUE,
              plotlyOutput("barplot_selectivity"),
          ),
          box(title = "Tableau de l'Analyse de Sélectivités", width = 12,solidHeader = TRUE,
              collapsible = TRUE,
              dataTableOutput("tab_selectivity"),
          )
        )
      ),
      
     # box(
      #  title = "Histogram", status = "primary", solidHeader = TRUE,
       # collapsible = TRUE,
        #plotOutput("plot3", height = 250)
    #  ),
      
      # Onglet "Analyse Opérateur"
      tabItem(
        tabName = "operator_data",
        fluidRow(
          # Add operator analysis data table here
          box(title = "Tableau de Données - Analyse Opérateur", width = 12,
              dataTableOutput("Oper_Evaluation"),
              downloadButton("download_excel", "Télécharger en Excel")
          )
        )
      ),
      tabItem(
        tabName = "operator_details",
        fluidRow(
          # Add operator analysis graph here
          box(title = "Analyse Opérateur", width = 12,
              dataTableOutput("operator_details")
          )
        )
      ),
      # Onglet "Analyse Nomenclature"
      tabItem(
        tabName = "nomenclature_data",
        fluidRow(
          # Add operator analysis data table here
          box(title = "Tableau de Données - Analyse Nomenclature", width = 12,
              dataTableOutput("NOMENCLATURE_Evaluation")
          )
        )
      ),
      tabItem(
        tabName = "nomenclature_details",
        fluidRow(
          # Add operator analysis graph here
          box(title = "Analyse Nomenclature", width = 12,
              dataTableOutput("NOMENCLATURE_details")
          )
        )
      ),
      # Onglet "Analyse Norigine"
      tabItem(
        tabName = "origine_data",
        fluidRow(
          # Add operator analysis data table here
          box(title = "Tableau de Données - Analyse Origine", width = 12,
              dataTableOutput("Origine_Evaluation")
          )
        )
      ),
      tabItem(
        tabName = "origine_details",
        fluidRow(
          # Add operator analysis graph here
          box(title = "Analyse Origine", width = 12,
              dataTableOutput("Origine_details")
          )
        )
      )
    )
  )
))


# Définir la logique du serveur (server)
server <- function(input, output, session) {
  
  # Créer les options pour les filtres
  result_auth <- secure_server(check_credentials = check_credentials(credentials))
  observeEvent(input$file1, {
    data <- reactive({
      data <- read_excel(input$file1$datapath,col_types = c("numeric", "text", "text", 
                                                            "date", "date", "date", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "text", "text", "text", "text", "text", 
                                                            "numeric", "text", "numeric", "numeric", 
                                                            "numeric", "numeric", "text", "numeric", 
                                                            "text", "text", "text", "text", "text", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "numeric", "numeric", 
                                                            "numeric", "text", "numeric", "text"))
      #View(data))
      # Renommer les colonnes du dataframe
      colnames(data) <- c("INSTANCEID", "REFERENCE_DECLARATION", "BUREAU", "DATE_ENREGISTREMENT", "DATE_LIQUIDATION",
                          "DATE_PAYEMENT", "MODELE", "PROCEDURE", "CIRCUIT_PRECEDENT", "CIRCUIT_ACTUEL", 
                          "PAYS_ORIGINE_DECL", "CODE_PAYS_DESTINATION", "NOM_PAYS_DESTINATION", "CODE_PAYS_EXPORT",
                          "NOM_PAYS_EXPORT", "CODE_PAYS_PROV._DECL", "CODE_IMPORTATEUR", "NOM_IMPORTATEUR",
                          "CODE_DECLARANT", "NOM_DECLARANT", "EXPORTATEUR", "Identite_Transport_D/A", 
                          "Identite_Transport_F", "VALEUR_FACTURE_DEVISE", "DEVISE_DECL", "TAUX_DEVISE_DECL",
                          "VALEUR_FACTURE_XOF", "NOMBRE_ARTICLES_DECL", "NOMBRE_COLIS_DECL", "INCOTERM_DECL",
                          "N°_ARTICLE", "CODE_ADDITIONEL", "NOMENCLATURE", "LIBELLE_SH", "DESCRIPTION_COMMERCIALE",
                          "CODE_PAYS_ORIGINE", "VALEUR_ARTICLE_DEVISE", "VALEUR_ARTICLE_XOF", "VALEUR_CAF_ARTICLE_XOF",
                          "TAXES_ARTICLES_XOF", "MASSE_BRUTE_ARTICLE", "MASSE_NETTE_ARTICLE", "QUANTITE", 
                          "UNITE_SUPPLEMENTAIRE", "QUANTITE_UNIT_SUP.", "CONTRE_ÉCRITURE")
      
      data$DATE_ENREGISTREMENT <- as.Date(data$DATE_ENREGISTREMENT)  
      data$DATE_ENREGISTREMENT <- as.Date(data$DATE_ENREGISTREMENT) 
      View(data)
      return(data)
    })
    
  })
  
  observeEvent(input$file1, {
    
  Droits_compromis <- reactive({
   Droits_compromis <- read_excel(input$file3$datapath)
  # colnames(Droits_compromis)[2] <- "NUMDEC"
   Droits_compromis$NUMDEC <- paste0("2023-",Droits_compromis$BUREAU_D,"-",substr(Droits_compromis$NUM_DECL, 1, 1), "-", substr(Droits_compromis$NUM_DECL, 3, 8))
   Droits_compromis_dec<- select(Droits_compromis,NUMDEC,DC) %>% group_by(NUMDEC) %>% summarise(tot_DC=sum(DC))
   Droits_compromis<-left_join(Droits_compromis,Droits_compromis_dec,by=c('NUMDEC'))
  View(Droits_compromis)
   return(Droits_compromis)
})
  })
 
  observeEvent(input$file1, {
   AVD <- reactive({
    AVD <- read_excel(input$file2$datapath)
    AVD<- select(AVD,IFU_IMPORTATEUR,Ref_dec,Diff_Taxes)
    View(AVD)
    return(AVD)
  }) 
  })  
  df <- reactive({ 
    data <- left_join(data(), Droits_compromis(), by = c( 'REFERENCE_DECLARATION'='NUMDEC','N°_ARTICLE'='NUM_ARTICLE' ))
    data <- left_join(data, AVD(), by = c( 'REFERENCE_DECLARATION'='Ref_dec'))
    data <- data %>%
      mutate(
        DC = replace(DC, is.na(DC), 0),
        `tot_DC` = replace(`tot_DC`, is.na(`tot_DC`), 0),
        Diff_Taxes = replace(Diff_Taxes, is.na(Diff_Taxes), 0)
      )
    data$SH2 <- substr(data$NOMENCLATURE, 1, 2)
    data$SH4 <- substr(data$NOMENCLATURE, 1, 4)
    data$SH6 <- substr(data$NOMENCLATURE, 1, 6)
    data$DEC[is.na(data$Diff_Taxes)] <- 0
    data$DEC<-ifelse(data$CONTRE_ÉCRITURE=='OUI','Non conforme','Conforme')
    #data$AVD<- ifelse (data$Diff_Taxes==0.0, "Conforme",data$Diff_Taxes)
    data$AVD<- ifelse (data$Diff_Taxes>10000, "Non conforme","Conforme")
    data$DCO<-ifelse(data$DC>0,'Non Conforme','Conforme')
    # Remplacer les valeurs vides par 0 dans les colonnes DEC, AVD et DCO
    # Pour DEC
    data$DEC <- ifelse(is.na(data$DEC), "Conforme",data$DEC)
    
    # Pour AVD
    data$AVD <- ifelse(is.na(data$AVD), "Conforme",data$AVD)
    
    # Pour DCO
    data$DCO <- ifelse(is.na(data$DCO), "Conforme",data$DCO)
    # Appliquer la logique de conformité
    data$Conformité <- ifelse((data$DEC == 'Conforme' & data$AVD == 'Conforme' & data$DCO == 'Conforme'), "Conforme", "Non Conforme")
    data$Conformité <- ifelse((is.na(data$DEC) & is.na(data$AVD) & is.na(data$DCO)) | (data$DEC == 'Conforme' & data$AVD == 'Conforme' & data$DCO == 'Conforme'), "Conforme", "Non Conforme")
   
    df<-data
    return(df)
    
    
  }) 
  filter_options <- reactive({
    tryCatch({
      if (is.null(df())) {
        return(list(
          message = "En Attente de télécharger les données"
        ))
      }
      df<-df()
      list(
        date_range_selectivity = dateRangeInput("date_range_selectivity", "Sélectionnez une plage de dates", start = "2023-01-01", end = "2023-12-31"),
        bureau_filter_selectivity = selectInput("bureau_filter_selectivity", "BUREAU", choices = unique(df$BUREAU), multiple = TRUE),
        modele_filter_selectivity = selectInput("modele_filter_selectivity", "MODELE", choices = unique(df$MODELE), multiple = TRUE),
        importer_filter_selectivity = selectInput("importer_filter_selectivity", "OPERATEUR", choices = unique(df$CODE_IMPORTATEUR), multiple = TRUE),
        nomenclature_filter_selectivity = selectInput("nomenclature_filter_selectivity", "NOMENCLATURE", choices = unique(df$SH4), multiple = TRUE),
        origine_filter_selectivity = selectInput("origine_filter_selectivity", "CODE_PAYS_ORIGINE", choices = unique(df$CODE_PAYS_ORIGINE), multiple = TRUE),
        circuit_filter_selectivity = selectInput("circuit_filter_selectivity", "CIRCUIT", choices = unique(df$CIRCUIT_PRECEDENT), multiple = TRUE),
        Non_Conformity_filter_selectivity = selectInput("Non_Conformity_filter_selectivity", "CONFORMITE", choices = unique(df$Conformité), multiple = TRUE)
      )
    }, error = function(e) {
      list(
        message = "En Attente de télécharger les données"
      )
    })
  })
  
  # Afficher les filtres
  output$filter_ui <- renderUI({
    tagList(
      lapply(filter_options(), function(filter) {
        column(12, filter)
      })
    )
  })
  
  # Filtrer les données pour l'onglet "Analyse de Sélectivités" - Données
  filtered_data_selectivity <- reactive({
    df<-df()
    req(input$date_range_selectivity)
    df_filtered <- subset(df, DATE_ENREGISTREMENT >= input$date_range_selectivity[1] & DATE_ENREGISTREMENT <= input$date_range_selectivity[2])
    if (!is.null(input$bureau_filter_selectivity) && input$bureau_filter_selectivity != "") {
      df_filtered <- df_filtered[df_filtered$BUREAU %in% input$bureau_filter_selectivity, ]
    }
    if (!is.null(input$modele_filter_selectivity) && input$modele_filter_selectivity != "") {
      df_filtered <- df_filtered[df_filtered$MODELE %in% input$modele_filter_selectivity, ]
    }
    if (!is.null(input$importer_filter_selectivity) && input$importer_filter_selectivity != "") {
      df_filtered <- df_filtered[df_filtered$CODE_IMPORTATEUR %in% input$importer_filter_selectivity, ]
    }
    if (!is.null(input$circuit_filter_selectivity) && input$circuit_filter_selectivity != "") {
      df_filtered <- df_filtered[df_filtered$CIRCUIT_PRECEDENT %in% input$circuit_filter_selectivity, ]
    }
    if (!is.null(input$nomenclature_filter_selectivity) && input$nomenclature_filter_selectivity != "") {
      df_filtered <- df_filtered[df_filtered$SH4 %in% input$nomenclature_filter_selectivity, ]
    }
    if (!is.null(input$origine_filter_selectivity) && input$origine_filter_selectivity != "") {
      df_filtered <- df_filtered[df_filtered$CODE_PAYS_ORIGINE %in% input$origine_filter_selectivity, ]
    }
    if (!is.null(input$Non_Conformity_filter_selectivity) && input$Non_Conformity_filter_selectivity != "") {
      df_filtered <- df_filtered[df_filtered$Conformité %in% input$Non_Conformity_filter_selectivity, ]
    }
    return(df_filtered)
  })#circuit_filter_selectivity
  
  # Afficher les données pour l'onglet "Analyse de Sélectivités" - Données
 
  output$data_table_selectivity <- renderDataTable({
    filtered_data_selectivity()
  }, options = list(
    scrollX = TRUE,  # Enable horizontal scrolling
    paging = TRUE,   # Enable pagination
    searching = TRUE  # Enable search functionality
  ))
  
  # Afficher le graphique pour l'onglet "Analyse de Sélectivités" - Graphique
  output$barplot_selectivity <- renderPlotly({
    
    # Données
    Nbre_dec_circuit <- select(filtered_data_selectivity(), REFERENCE_DECLARATION, CIRCUIT_PRECEDENT) %>%
      group_by(REFERENCE_DECLARATION, CIRCUIT_PRECEDENT) %>%
      summarise() %>%
      group_by(CIRCUIT_PRECEDENT) %>%
      count()
    
    # Calcul du nombre total de déclarations
    Nbre_tot_circuit <- sum(Nbre_dec_circuit$n)
    
    # Ajout des colonnes pour le total et le pourcentage
    Nbre_dec_circuit$total <- Nbre_tot_circuit
    Nbre_dec_circuit$Pourcentage <- round(Nbre_dec_circuit$n / Nbre_dec_circuit$total,2)
    
    # Création d'un tableau de correspondance des couleurs
    couleurs_fr <- c("ROUGE", "VERT", "JAUNE", "BLEU")  # Couleurs en français
    couleurs_en <- c("red", "green", "yellow", "blue")  # Couleurs en anglais
    names(couleurs_en) <- couleurs_fr  # Assigner les noms français comme noms anglais
    
    # Remplacer les couleurs françaises par les couleurs anglaises
    Nbre_dec_circuit$CIRCUIT_PRECEDENT <- couleurs_en[Nbre_dec_circuit$CIRCUIT_PRECEDENT]
    
    # Création du graphique en secteurs avec Plotly
    plot_ly(Nbre_dec_circuit, labels = ~paste(round(Pourcentage * 100, 2), "%"),
            values = ~n, type = 'pie', marker = list(colors = Nbre_dec_circuit$CIRCUIT_PRECEDENT)) %>%
      layout(title = 'Répartition des déclarations par circuit',
             showlegend = FALSE)  # Supprimer la légende
    
  })
  output$tab_selectivity <- renderDataTable({
    
    # Données
    Nbre_dec_circuit <- select(filtered_data_selectivity(), REFERENCE_DECLARATION, CIRCUIT_PRECEDENT) %>%
      group_by(REFERENCE_DECLARATION, CIRCUIT_PRECEDENT) %>%
      summarise() %>%
      group_by(CIRCUIT_PRECEDENT) %>%
      count()
    
    # Calcul du nombre total de déclarations
    Nbre_tot_circuit <- sum(Nbre_dec_circuit$n)
    
    # Ajout des colonnes pour le total et le pourcentage
    Nbre_dec_circuit$total <- Nbre_tot_circuit
    Nbre_dec_circuit$Pourcentage <- Nbre_dec_circuit$n / Nbre_dec_circuit$total
    
    Nbre_dec_circuit
    
  })
  
  Droits_compromis2 <- reactive({ 
    data <- filtered_data_selectivity()
    
    Droits_compromis_dec <- select(data, REFERENCE_DECLARATION, tot_DC) %>%
      group_by(REFERENCE_DECLARATION,tot_DC) %>% slice(1)
    
    Droits_compromis_dec <- select(Droits_compromis_dec,tot_DC) %>% summarise(DCT = sum(tot_DC, na.rm = TRUE))  # Assurez-vous de spécifier na.rm = TRUE pour gérer les valeurs manquantes
    Droits_compromis_dec <- sum(Droits_compromis_dec$DCT)
    Droits_compromis_dec
  })
  
  
  #Calcul réactif des valeurs
  valeurs_reactives <- reactive({
    # Données filtrées pour les déclarations par circuit précédent
    data<-filtered_data_selectivity()
    Nbre_dec_circuit <- select(data, REFERENCE_DECLARATION, CIRCUIT_PRECEDENT) %>%
      group_by(REFERENCE_DECLARATION, CIRCUIT_PRECEDENT) %>%
      summarise() %>%
      group_by(CIRCUIT_PRECEDENT) %>%
      count()
    
    # Calcul du nombre total de déclarations
    Nbre_tot_circuit <- sum(Nbre_dec_circuit$n)
    
    # Calcul des déclarations sans contre-écriture par circuit précédent
    Nbre_dec_nc_circuit <- select(filtered_data_selectivity(), REFERENCE_DECLARATION, CIRCUIT_PRECEDENT, CONTRE_ÉCRITURE) %>%
      group_by(REFERENCE_DECLARATION, CIRCUIT_PRECEDENT, CONTRE_ÉCRITURE) %>%
      summarise() %>%
      filter(CONTRE_ÉCRITURE == 'OUI') %>%
      group_by(CIRCUIT_PRECEDENT) %>%
      count()
    
    # Jointure des deux tableaux
    tb_Assessement <- inner_join(Nbre_dec_circuit, Nbre_dec_nc_circuit, by = 'CIRCUIT_PRECEDENT')
    tb_Assessement <- tb_Assessement %>%
      rename(Nombre_declaration = n.x) %>%
      rename(Nombre_declaration_nc = n.y)
    
    # Calcul du taux de détection
    tb_Assessement$Taux_détection <-(tb_Assessement$Nombre_declaration_nc / tb_Assessement$Nombre_declaration)
    
    # Retourner les valeurs calculées
    list(
      Nbre_dec_circuit = Nbre_dec_circuit,
      Nbre_tot_circuit = Nbre_tot_circuit,
      tb_Assessement = tb_Assessement
    )
  })
  
  # Afficher les Value Boxes pour le Taux de détection par circuit précédent
  # Afficher les Value Boxes pour le Taux de détection par circuit précédent
  output$taux_detection_rouge <- renderValueBox({
  
    valueBox(
      value = sprintf("%.2f", valeurs_reactives()$tb_Assessement$Taux_détection[valeurs_reactives()$tb_Assessement$CIRCUIT_PRECEDENT == 'ROUGE'] * 100),
      subtitle = "Circuit ROUGE",
      icon = icon("check"),
      color = "red",
      width = 1
    )
  })
  
  output$taux_detection_vert <- renderValueBox({
    valueBox(
      value = sprintf("%.2f", valeurs_reactives()$tb_Assessement$Taux_détection[valeurs_reactives()$tb_Assessement$CIRCUIT_PRECEDENT == 'VERT'] * 100),
      subtitle = "Circuit VERT",
      icon = icon("check"),
      color = "green",
      width = 1
    )
  })
  
  output$taux_detection_jaune <- renderValueBox({
    valueBox(
      value = sprintf("%.2f", valeurs_reactives()$tb_Assessement$Taux_détection[valeurs_reactives()$tb_Assessement$CIRCUIT_PRECEDENT == 'JAUNE'] * 100),
      subtitle = "Circuit JAUNE",
      icon = icon("check"),
      color = "yellow",
      width = 1
    )
  })
  
  output$taux_detection_bleu <- renderValueBox({
    valueBox(
      value = sprintf("%.2f", valeurs_reactives()$tb_Assessement$Taux_détection[valeurs_reactives()$tb_Assessement$CIRCUIT_PRECEDENT == 'BLEU'] * 100),
      subtitle = "Circuit BLEU",
      icon = icon("check"),
      color = "blue",
      width = 1
    )
  }) 
  
  output$total_droits_compromis <- renderValueBox({
    value <- Droits_compromis2()
    valueBox(
      value = value,  # Assurez-vous de formater correctement la valeur si nécessaire
      subtitle = "Droits Compromis",
      icon = icon("check"),
      color = "purple",
      width = 4
    )
  })

  output$Oper_Evaluation <- renderDataTable({
    
    data <- filtered_data_selectivity()
    
    nbre_dec_oper<- select(data, REFERENCE_DECLARATION,CIRCUIT_PRECEDENT,CODE_IMPORTATEUR) %>%
      group_by(REFERENCE_DECLARATION,CODE_IMPORTATEUR) %>% summarise()
    nbre_dec_oper<-nbre_dec_oper %>%  group_by(CODE_IMPORTATEUR) %>% count() 
   # View (nbre_dec_oper)
    nbre_dec_oper_nc<-select(data, REFERENCE_DECLARATION,CIRCUIT_PRECEDENT,CODE_IMPORTATEUR,DC) %>%
      group_by(REFERENCE_DECLARATION,CODE_IMPORTATEUR) %>% summarise(DCT=sum(DC))
    
    nbre_dec_oper_nc <- data %>%
      filter(CONTRE_ÉCRITURE == 'OUI') %>%
      group_by(REFERENCE_DECLARATION, CODE_IMPORTATEUR) %>%
      summarise(DCT = sum(DC))
    
    # Regrouper par importateur et compter le nombre de déclarations
    nbre_dec_oper_nc <- data %>%
      filter(CONTRE_ÉCRITURE == 'OUI') %>%
      group_by(CODE_IMPORTATEUR, Diff_Taxes, REFERENCE_DECLARATION) %>%
      summarise(Somme_DC = sum(DC))
    
    # Regrouper par opérateur, diff_TAXES et compter le nombre de déclarations
    nbre_dec_oper_nc <- nbre_dec_oper_nc %>%
      group_by(CODE_IMPORTATEUR) %>%
      summarise(Nombre_Declarations = n(), Somme_DC = sum(Somme_DC),TA=round(sum(Diff_Taxes),0))
    
    
    
      #View(nbre_dec_oper_nc)
    Oper_Evaluation<- inner_join (nbre_dec_oper,nbre_dec_oper_nc,by=c('CODE_IMPORTATEUR'))
    #View (Oper_Evaluation)
    Oper_Evaluation <- Oper_Evaluation %>%
      rename(Nombre_declaration=n ) %>%
      rename(Nombre_declaration_nc=Nombre_Declarations)
    Oper_Evaluation$Pourcentage_NC=round(Oper_Evaluation$Nombre_declaration_nc/Oper_Evaluation$Nombre_declaration,2)
    act_oper<- select(data, REFERENCE_DECLARATION,CIRCUIT_PRECEDENT,CODE_IMPORTATEUR,VALEUR_CAF_ARTICLE_XOF,TAXES_ARTICLES_XOF,MASSE_NETTE_ARTICLE)%>% group_by(REFERENCE_DECLARATION,CODE_IMPORTATEUR) 
    act_oper<-act_oper %>% group_by(CODE_IMPORTATEUR) %>% summarise(Total_valeur=sum(VALEUR_CAF_ARTICLE_XOF),Total_taxes=sum(TAXES_ARTICLES_XOF),Total_poids=sum(MASSE_NETTE_ARTICLE))
    Oper_Evaluation<- inner_join (Oper_Evaluation,act_oper,by=c('CODE_IMPORTATEUR'))
    Oper_Evaluation$Pression_Fiscal<- round(Oper_Evaluation$Total_taxes/Oper_Evaluation$Total_valeur,2)
    Oper_Evaluation$Txes_per_kg<-round(Oper_Evaluation$Total_taxes/Oper_Evaluation$Total_poids,2)
   #View (Oper_Evaluation)
    Oper_Evaluation<-left_join(Oper_Evaluation,lst_operateur,by=c('CODE_IMPORTATEUR'))
    # Retourner le tableau formaté avec les couleurs conditionnelles
    datatable(
      Oper_Evaluation[, c("CODE_IMPORTATEUR","NOM_IMPORTATEUR", 
                                         "Nombre_declaration", "Nombre_declaration_nc", 
                                         "Pourcentage_NC", "Somme_DC", "TA", 
                                         "Total_valeur", "Total_taxes", "Total_poids", 
                                         "Pression_Fiscal", "Txes_per_kg")],
      filter = 'top',
      extensions = c("FixedColumns", "FixedHeader", "Scroller"),
      options = list(
        searching = TRUE,
        autoWidth = TRUE,
        rownames = FALSE,
        scroller = TRUE,
        scrollX = TRUE,
        scrollY = "500px",
        fixedHeader = TRUE,
        class = 'cell-border stripe',
        fixedColumns = list(
          leftColumns = 3,
          heightMatch = 'none'
        )
      )
    ) %>%
      formatStyle(
        columns = c("Pourcentage_NC", "Somme_DC", "TA"),
        valueColumns = c("Pourcentage_NC", "Somme_DC", "TA"),
        backgroundColor = styleInterval(
          c(0.05, 0.3), 
          c("springgreen", "wheat", "tomato")
        ),
        target = 'cell' # Appliquer le style à la cellule
      ) %>%
      formatStyle(
        columns = "Somme_DC",
        valueColumns = "Somme_DC",
        backgroundColor = styleInterval(0, c('transparent', 'tomato')),
        target = 'cell'
      ) %>%
      formatStyle(
        columns = "TA",
        valueColumns = "TA",
        backgroundColor = styleInterval(0, c('transparent', 'tomato')),
        target = 'cell'
      )
    
    
  }
  )
  
  output$NOMENCLATURE_Evaluation <- renderDataTable({
    
    data <- filtered_data_selectivity()
    
    nbre_dec_hscode<- select(data, REFERENCE_DECLARATION,CIRCUIT_PRECEDENT,NOMENCLATURE) %>%
      group_by(REFERENCE_DECLARATION,NOMENCLATURE) %>% summarise()
    nbre_dec_hscode<-nbre_dec_hscode %>%  group_by(NOMENCLATURE) %>% count() 
    # View (nbre_dec_oper)
    nbre_dec_hscode_nc<-select(data, REFERENCE_DECLARATION,CIRCUIT_PRECEDENT,NOMENCLATURE,DC) %>%
      group_by(REFERENCE_DECLARATION,NOMENCLATURE) %>% summarise(DCT=sum(DC))
    
    nbre_dec_hscode_nc <- data %>%
      filter(CONTRE_ÉCRITURE == 'OUI') %>%
      group_by(REFERENCE_DECLARATION, NOMENCLATURE) %>%
      summarise(DCT = sum(DC))
    
    # Regrouper par importateur et compter le nombre de déclarations
    nbre_dec_hscode_nc  <- data %>%
      filter(CONTRE_ÉCRITURE == 'OUI') %>%
      group_by(NOMENCLATURE, Diff_Taxes, REFERENCE_DECLARATION) %>%
      summarise(Somme_DC = sum(DC))
    
    # Regrouper par opérateur, diff_TAXES et compter le nombre de déclarations
    nbre_dec_hscode_nc <- nbre_dec_hscode_nc %>%
      group_by(NOMENCLATURE) %>%
      summarise(Nombre_Declarations = n(), Somme_DC = sum(Somme_DC),TA=round(sum(Diff_Taxes),0))
    
    
    
    #View(nbre_dec_oper_nc)
    NOMENCLATURE_Evaluation<- inner_join (nbre_dec_hscode,nbre_dec_hscode_nc,by=c('NOMENCLATURE'))
    #View (Oper_Evaluation)
    NOMENCLATURE_Evaluation <- NOMENCLATURE_Evaluation %>%
      rename(Nombre_declaration=n ) %>%
      rename(Nombre_declaration_nc=Nombre_Declarations)
    NOMENCLATURE_Evaluation$Pourcentage_NC=round(NOMENCLATURE_Evaluation$Nombre_declaration_nc/NOMENCLATURE_Evaluation$Nombre_declaration,2)
    act_NOMENCLATURE<- select(data, REFERENCE_DECLARATION,CIRCUIT_PRECEDENT,NOMENCLATURE,VALEUR_CAF_ARTICLE_XOF,TAXES_ARTICLES_XOF,MASSE_NETTE_ARTICLE)%>% group_by(REFERENCE_DECLARATION,NOMENCLATURE) 
    act_NOMENCLATURE<-act_NOMENCLATURE %>% group_by(NOMENCLATURE) %>% summarise(Total_valeur=sum(VALEUR_CAF_ARTICLE_XOF),Total_taxes=sum(TAXES_ARTICLES_XOF),Total_poids=sum(MASSE_NETTE_ARTICLE))
    act_NOMENCLATURE<- inner_join (NOMENCLATURE_Evaluation,act_NOMENCLATURE,by=c('NOMENCLATURE'))
    act_NOMENCLATURE$Pression_Fiscal<- round(act_NOMENCLATURE$Total_taxes/act_NOMENCLATURE$Total_valeur,2)
    act_NOMENCLATURE$Txes_per_kg<-round(act_NOMENCLATURE$Total_taxes/act_NOMENCLATURE$Total_poids,2)
    
    # Retourner le tableau formaté avec les couleurs conditionnelles
    datatable(
      act_NOMENCLATURE,
      filter = 'top',
      extensions = c("FixedColumns", "FixedHeader", "Scroller"),
      options = list(
        searching = TRUE,
        autoWidth = TRUE,
        rownames = FALSE,
        scroller = TRUE,
        scrollX = TRUE,
        scrollY = "500px",
        fixedHeader = TRUE,
        class = 'cell-border stripe',
        fixedColumns = list(
          leftColumns = 2,
          heightMatch = 'none'
        )
      )
    ) %>%
      formatStyle(
        columns = c("Pourcentage_NC", "Somme_DC", "TA"),
        valueColumns = c("Pourcentage_NC", "Somme_DC", "TA"),
        backgroundColor = styleInterval(
          c(0.05, 0.3), 
          c("springgreen", "wheat", "tomato")
        ),
        target = 'cell' # Appliquer le style à la cellule
      ) %>%
      formatStyle(
        columns = "Somme_DC",
        valueColumns = "Somme_DC",
        backgroundColor = styleInterval(0, c('transparent', 'tomato')),
        target = 'cell'
      ) %>%
      formatStyle(
        columns = "TA",
        valueColumns = "TA",
        backgroundColor = styleInterval(0, c('transparent', 'tomato')),
        target = 'cell'
      )
    
    
  }
  )
  
  output$Origine_Evaluation <- renderDataTable({
    
    data <- filtered_data_selectivity()
    
    nbre_dec_origine<- select(data, REFERENCE_DECLARATION,CIRCUIT_PRECEDENT,CODE_PAYS_ORIGINE) %>%
      group_by(REFERENCE_DECLARATION,CODE_PAYS_ORIGINE) %>% summarise()
    nbre_dec_origine<-nbre_dec_origine %>%  group_by(CODE_PAYS_ORIGINE) %>% count() 
    # View (nbre_dec_oper)
    nbre_dec_origine_nc<-select(data, REFERENCE_DECLARATION,CIRCUIT_PRECEDENT,CODE_PAYS_ORIGINE,DC) %>%
      group_by(REFERENCE_DECLARATION,CODE_PAYS_ORIGINE) %>% summarise(DCT=sum(DC))
    
    nbre_dec_origine_nc <- data %>%
      filter(CONTRE_ÉCRITURE == 'OUI') %>%
      group_by(REFERENCE_DECLARATION, CODE_PAYS_ORIGINE) %>%
      summarise(DCT = sum(DC))
    
    # Regrouper par importateur et compter le nombre de déclarations
    nbre_dec_origine_nc  <- data %>%
      filter(CONTRE_ÉCRITURE == 'OUI') %>%
      group_by(CODE_PAYS_ORIGINE, Diff_Taxes, REFERENCE_DECLARATION) %>%
      summarise(Somme_DC = sum(DC))
    
    # Regrouper par opérateur, diff_TAXES et compter le nombre de déclarations
    nbre_dec_origine_nc <- nbre_dec_origine_nc %>%
      group_by(CODE_PAYS_ORIGINE) %>%
      summarise(Nombre_Declarations = n(), Somme_DC = sum(Somme_DC),TA=round(sum(Diff_Taxes),0))
    
    
    
    #View(nbre_dec_oper_nc)
    origine_Evaluation<- inner_join (nbre_dec_origine,nbre_dec_origine_nc,by=c('CODE_PAYS_ORIGINE'))
    #View (Oper_Evaluation)
    origine_Evaluation <- origine_Evaluation %>%
      rename(Nombre_declaration=n ) %>%
      rename(Nombre_declaration_nc=Nombre_Declarations)
    origine_Evaluation$Pourcentage_NC=round(origine_Evaluation$Nombre_declaration_nc/origine_Evaluation$Nombre_declaration,2)
    act_origine<- select(data, REFERENCE_DECLARATION,CIRCUIT_PRECEDENT,CODE_PAYS_ORIGINE,VALEUR_CAF_ARTICLE_XOF,TAXES_ARTICLES_XOF,MASSE_NETTE_ARTICLE)%>% group_by(REFERENCE_DECLARATION,CODE_PAYS_ORIGINE) 
    act_origine<-act_origine %>% group_by(CODE_PAYS_ORIGINE) %>% summarise(Total_valeur=sum(VALEUR_CAF_ARTICLE_XOF),Total_taxes=sum(TAXES_ARTICLES_XOF),Total_poids=sum(MASSE_NETTE_ARTICLE))
    act_origine<- inner_join (origine_Evaluation,act_origine,by=c('CODE_PAYS_ORIGINE'))
    act_origine$Pression_Fiscal<- round(act_origine$Total_taxes/act_origine$Total_valeur,2)
    act_origine$Txes_per_kg<-round(act_origine$Total_taxes/act_origine$Total_poids,2)
    
    # Retourner le tableau formaté avec les couleurs conditionnelles
    datatable(
      act_origine,
      filter = 'top',
      extensions = c("FixedColumns", "FixedHeader", "Scroller"),
      options = list(
        searching = TRUE,
        autoWidth = TRUE,
        rownames = FALSE,
        scroller = TRUE,
        scrollX = TRUE,
        scrollY = "500px",
        fixedHeader = TRUE,
        class = 'cell-border stripe',
        fixedColumns = list(
          leftColumns = 2,
          heightMatch = 'none'
        )
      )
    ) %>%
      formatStyle(
        columns = c("Pourcentage_NC", "Somme_DC", "TA"),
        valueColumns = c("Pourcentage_NC", "Somme_DC", "TA"),
        backgroundColor = styleInterval(
          c(0.05, 0.3), 
          c("springgreen", "wheat", "tomato")
        ),
        target = 'cell' # Appliquer le style à la cellule
      ) %>%
      formatStyle(
        columns = "Somme_DC",
        valueColumns = "Somme_DC",
        backgroundColor = styleInterval(0, c('transparent', 'tomato')),
        target = 'cell'
      ) %>%
      formatStyle(
        columns = "TA",
        valueColumns = "TA",
        backgroundColor = styleInterval(0, c('transparent', 'tomato')),
        target = 'cell'
      )
    
    
  }
  )
  output$bur_chart <- renderPlotly({
    data <- filtered_data_selectivity()
    nbre_dec_bur <- select(data, REFERENCE_DECLARATION, CIRCUIT_PRECEDENT, BUREAU) %>%
      group_by(REFERENCE_DECLARATION, CIRCUIT_PRECEDENT) %>%
      summarise() %>%
      count()
    
    nbre_dec_bur <- select(data, REFERENCE_DECLARATION, CIRCUIT_PRECEDENT, BUREAU) %>%
      group_by(REFERENCE_DECLARATION, CIRCUIT_PRECEDENT, BUREAU) %>%
      slice(1)
    
    nbre_dec_bur <- nbre_dec_bur %>%
      group_by(BUREAU, CIRCUIT_PRECEDENT) %>%
      count()
    
    nbre_dec_bur <- nbre_dec_bur %>%
      rename(Nombre_declaration = n)
    
    # Créer une palette de couleurs
    colors <- c("red", "green", "blue", "yellow")
    
    # Créer le graphique Plotly
   
    
    # Créer le graphique avec Plotly
    # Définir les couleurs
    colors <- c("blue","yellow","red", "green")
    
    # Créer le graphique avec ggplot
    ggplot(nbre_dec_bur, aes(x = CIRCUIT_PRECEDENT, y = Nombre_declaration, fill = CIRCUIT_PRECEDENT)) +
      geom_col() +
      scale_fill_manual(values = colors) +
      facet_wrap(vars(BUREAU)) +
      theme_minimal() +
      theme(axis.text.x = element_blank()) +
      labs(title = "Nombre de Déclarations par Circuit Précédent et Bureau", x = NULL, y = "Nombre de Déclarations")
  })
  
  # Autres fonctions du serveur...
  output$operator_graph <- renderPlotly({
    data <- filtered_data_selectivity()
    
    importer_stat<- data %>%
      select(REFERENCE_DECLARATION, CIRCUIT_PRECEDENT) %>%
      group_by(REFERENCE_DECLARATION, CIRCUIT_PRECEDENT) %>%
      summarise() %>%
      group_by(CIRCUIT_PRECEDENT) %>%
      count()
    plot_ly(importer_stat, x = ~CIRCUIT_PRECEDENT, y = ~n, type = 'bar', color = ~CIRCUIT_PRECEDENT,
            colors = colors, # Utiliser les couleurs spécifiées
            marker = list(line = list(color = 'black', width = 0.5))) %>%
      layout(title = "Distribution des Couleurs", xaxis = list(title = "Circuit Précédent"), yaxis = list(title = "Nombre de Déclarations"))
  }) 
  output$operator_details <- renderDataTable({
    
    data <- filtered_data_selectivity()
    
    operator_details <- select(data, CODE_IMPORTATEUR, NOM_IMPORTATEUR, BUREAU, CODE_DECLARANT, NOM_DECLARANT, NOMENCLATURE, LIBELLE_SH, CODE_PAYS_ORIGINE, CODE_PAYS_DESTINATION,CONTRE_ÉCRITURE,DC,Diff_Taxes)
    
    # Return the final dataset
    datatable(operator_details, filter = 'top',
              extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
              options = list(
                searching = TRUE,
                autoWidth = TRUE,
                rownames = FALSE,
                fixedHeader = TRUE,
                scrollX = TRUE,  # Add lateral scrolling
                class = 'cell-border stripe',
                fixedColumns = list(
                  leftColumns = 2,
                  heightMatch = 'none'
                )
              )
    )
    
  }
  )
  output$NOMENCLATURE_details <- renderDataTable({
    
    data <- filtered_data_selectivity()
    
    nomenclature_details <- select(data,  NOMENCLATURE, LIBELLE_SH,DESCRIPTION_COMMERCIALE, CODE_IMPORTATEUR, NOM_IMPORTATEUR, BUREAU, CODE_DECLARANT, NOM_DECLARANT, CODE_PAYS_ORIGINE, CODE_PAYS_DESTINATION,CONTRE_ÉCRITURE,DC,Diff_Taxes)
    
    # Return the final dataset
    datatable(nomenclature_details, filter = 'top',
              extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
              options = list(
                searching = TRUE,
                autoWidth = TRUE,
                rownames = FALSE,
                fixedHeader = TRUE,
                scrollX = TRUE,  # Add lateral scrolling
                class = 'cell-border stripe',
                fixedColumns = list(
                  leftColumns = 2,
                  heightMatch = 'none'
                )
              )
    )
    
  })
  output$Origine_details <- renderDataTable({
    
    data <- filtered_data_selectivity()
    
    nomenclature_details <- select(data,CODE_PAYS_ORIGINE,  NOMENCLATURE, LIBELLE_SH,DESCRIPTION_COMMERCIALE, CODE_IMPORTATEUR, NOM_IMPORTATEUR, BUREAU, CODE_DECLARANT, NOM_DECLARANT, CODE_PAYS_ORIGINE, CODE_PAYS_DESTINATION,CONTRE_ÉCRITURE,DC,Diff_Taxes)
    
    # Return the final dataset
    datatable(nomenclature_details, filter = 'top',
              extensions = c("FixedColumns", "FixedHeader", "Scroller"), 
              options = list(
                searching = TRUE,
                autoWidth = TRUE,
                rownames = FALSE,
                fixedHeader = TRUE,
                scrollX = TRUE,  # Add lateral scrolling
                class = 'cell-border stripe',
                fixedColumns = list(
                  leftColumns = 2,
                  heightMatch = 'none'
                )
              )
    )
    
  }
  )
  output$download_excel <- downloadHandler(
    filename = function() {
      "operateur_data.xlsx" # Nom du fichier de sortie
    },
    content = function(file) {
      # Écrire les données du tableau dans un fichier Excel
      writexl::write_xlsx(Oper_Evaluation_download(), file) # Opérateur_evaluation est le nom de votre dataframe
    }
  )  
  
  
  output$Nbre_dec_regime <- renderDataTable({
    
    data <- filtered_data_selectivity()
    
    nbre_dec_regime<- select(data, REFERENCE_DECLARATION,CIRCUIT_PRECEDENT,MODELE) %>%
      group_by(REFERENCE_DECLARATION,MODELE) %>% summarise()
    nbre_dec_regime<-nbre_dec_regime %>%  group_by(MODELE) %>% count() 
    # View (nbre_dec_oper)
    
    # Retourner le tableau formaté avec les couleurs conditionnelles
    datatable(
      nbre_dec_regime,
      filter = 'top',
      extensions = c("FixedColumns", "FixedHeader", "Scroller"),
      options = list(
        searching = TRUE,
        autoWidth = TRUE,
        rownames = FALSE,
        scroller = TRUE,
        scrollX = TRUE,
        scrollY = "500px",
        fixedHeader = TRUE,
        class = 'cell-border stripe',
        fixedColumns = list(
          leftColumns = 2,
          heightMatch = 'none'
        )
      )
    )
    
  }
  )
  Oper_Evaluation_download <- reactive({
    
    data <- filtered_data_selectivity()
    
    nbre_dec_oper<- select(data, REFERENCE_DECLARATION,CIRCUIT_PRECEDENT,CODE_IMPORTATEUR) %>%
      group_by(REFERENCE_DECLARATION,CODE_IMPORTATEUR) %>% summarise()
    nbre_dec_oper<-nbre_dec_oper %>%  group_by(CODE_IMPORTATEUR) %>% count() 
    # View (nbre_dec_oper)
    nbre_dec_oper_nc<-select(data, REFERENCE_DECLARATION,CIRCUIT_PRECEDENT,CODE_IMPORTATEUR,DC) %>%
      group_by(REFERENCE_DECLARATION,CODE_IMPORTATEUR) %>% summarise(DCT=sum(DC))
    
    nbre_dec_oper_nc <- data %>%
      filter(CONTRE_ÉCRITURE == 'OUI') %>%
      group_by(REFERENCE_DECLARATION, CODE_IMPORTATEUR) %>%
      summarise(DCT = sum(DC))
    
    # Regrouper par importateur et compter le nombre de déclarations
    nbre_dec_oper_nc <- data %>%
      filter(CONTRE_ÉCRITURE == 'OUI') %>%
      group_by(CODE_IMPORTATEUR, Diff_Taxes, REFERENCE_DECLARATION) %>%
      summarise(Somme_DC = sum(DC))
    
    # Regrouper par opérateur, diff_TAXES et compter le nombre de déclarations
    nbre_dec_oper_nc <- nbre_dec_oper_nc %>%
      group_by(CODE_IMPORTATEUR) %>%
      summarise(Nombre_Declarations = n(), Somme_DC = sum(Somme_DC),TA=round(sum(Diff_Taxes),0))
    
    
    
    #View(nbre_dec_oper_nc)
    Oper_Evaluation<- inner_join (nbre_dec_oper,nbre_dec_oper_nc,by=c('CODE_IMPORTATEUR'))
    #View (Oper_Evaluation)
    Oper_Evaluation <- Oper_Evaluation %>%
      rename(Nombre_declaration=n ) %>%
      rename(Nombre_declaration_nc=Nombre_Declarations)
    Oper_Evaluation$Pourcentage_NC=round(Oper_Evaluation$Nombre_declaration_nc/Oper_Evaluation$Nombre_declaration,2)
    act_oper<- select(data, REFERENCE_DECLARATION,CIRCUIT_PRECEDENT,CODE_IMPORTATEUR,VALEUR_CAF_ARTICLE_XOF,TAXES_ARTICLES_XOF,MASSE_NETTE_ARTICLE)%>% group_by(REFERENCE_DECLARATION,CODE_IMPORTATEUR) 
    act_oper<-act_oper %>% group_by(CODE_IMPORTATEUR) %>% summarise(Total_valeur=sum(VALEUR_CAF_ARTICLE_XOF),Total_taxes=sum(TAXES_ARTICLES_XOF),Total_poids=sum(MASSE_NETTE_ARTICLE))
    Oper_Evaluation<- inner_join (Oper_Evaluation,act_oper,by=c('CODE_IMPORTATEUR'))
    Oper_Evaluation$Pression_Fiscal<- round(Oper_Evaluation$Total_taxes/Oper_Evaluation$Total_valeur,2)
    Oper_Evaluation$Txes_per_kg<-round(Oper_Evaluation$Total_taxes/Oper_Evaluation$Total_poids,2)
    Oper_Evaluation<-left_join(Oper_Evaluation,lst_operateur,by=c('CODE_IMPORTATEUR'))
    # Retourner le tableau formaté avec les couleurs conditionnelles
    
    
    
    # Retourner le tableau formaté avec les couleurs conditionnelles
    Oper_Evaluation
    
    
  })
  
  
} 
options(shiny.maxRequestSize = 300*1024^2) # Set max upload size to 300 MB
# Exécuter l'application
shinyApp(ui = ui, server = server)

