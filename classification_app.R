library(shiny)
library(ggplot2)

# Lade die Daten
all.data.mgrad.validation <- readRDS("all_data_mgrad_validation.rds") |> 
  select(ID, Class,  EP.y1:EP.y300)

# Lade die Liste der unauffälligen IDs
sub_norm <- readRDS("sub_norm.rds")

# Extrahiere die Chromatogramme der unauffälligen IDs
norm_data <- all.data.mgrad.validation[all.data.mgrad.validation$ID %in% sub_norm$ID, ]


# Datei für gespeicherte Klassifikationen
classified_file <- "classified_chromatograms.csv"

# Falls bereits gespeicherte Klassifikationen existieren, laden
if (file.exists(classified_file)) {
  classified_data <- read.csv(classified_file, stringsAsFactors = FALSE)
} else {
  classified_data <- data.frame(ID = character(), Classification = character(), stringsAsFactors = FALSE)
}

# Bestimme den nächsten unklassifizierten Index
get_next_index <- function() {
  classified_ids <- classified_data$ID
  unclassified_rows <- which(!(all.data.mgrad.validation$ID %in% classified_ids))
  if (length(unclassified_rows) > 0) {
    return(unclassified_rows[1])  # Erste nicht klassifizierte Zeile
  } else {
    return(NA)  # Alle sind klassifiziert
  }
}

# UI der App
ui <- fluidPage(
    titlePanel("Chromatogramm Klassifikation"),
    
    tabsetPanel(
        tabPanel("Klassifikation", 
                 sidebarLayout(
                     sidebarPanel(
                         h3("Klassifikation wählen:"),
                         div(style = "display: flex; gap: 10px;",
                             actionButton("btn_unauffaellig", "Unauffällig", 
                                          style = "flex: 1; height:50px; font-size:18px; background-color:#4CAF50; color:white;"),
                             actionButton("btn_suspicious", "Suspicious", 
                                          style = "flex: 1; height:50px; font-size:18px; background-color:#FFC107; color:black;"),
                             actionButton("btn_mgradient", "M-Gradient", 
                                          style = "flex: 1; height:50px; font-size:18px; background-color:#F44336; color:white;")
                         ),
                         br(),
                         textOutput("progress")
                     ),
                     mainPanel(
                         plotOutput("chromatogramPlot")
                     )
                 )
        ),
        
        tabPanel("Normale Chromatogramme",
                 sidebarLayout(
                     sidebarPanel(
                         actionButton("prev_norm", "Zurück"),
                         actionButton("next_norm", "Weiter"),
                         br(),
                         textOutput("norm_progress")
                     ),
                     mainPanel(
                         plotOutput("normalChromatogramPlot")
                     )
                 )
        )
    )
)


# Server der App
server <- function(input, output, session) {
  # Datenindex zum Durchlaufen der Chromatogramme
  current_index <- reactiveVal(get_next_index())
  
  # DataFrame zur Speicherung der Klassifikationen
  classified_data <- reactiveVal(data.frame(ID = character(), Classification = character(), stringsAsFactors = FALSE))
  
  # Funktion zum Extrahieren des aktuellen Chromatogramms
  get_current_chromatogram <- reactive({
    if (current_index() > nrow(all.data.mgrad.validation)) return(NULL)
    row_data <- all.data.mgrad.validation[current_index(), ]
    x_values <- 1:(ncol(all.data.mgrad.validation) - 1)  # EP.y1 bis EP.y300
    y_values <- as.numeric(row_data[-1])  # Alle Spalten außer ID
    
    data.frame(x = x_values, y = y_values, ID = row_data$ID)
  })
  
  # Zeichne das aktuelle Chromatogramm
  output$chromatogramPlot <- renderPlot({
    chrom_data <- get_current_chromatogram()
    if (is.null(chrom_data)) return(NULL)
    
    ggplot(chrom_data, aes(x = x, y = y)) +
      geom_line(color = "blue") +
      labs(title = paste("Chromatogramm ID:", chrom_data$ID[1]),
           x = "x (Messpunkt)", y = "y (Intensität)") +
      theme_minimal()
  })
  
  # Fortschrittsanzeige
  output$progress <- renderText({
    paste("Chromatogramm", current_index(), "von", nrow(all.data.mgrad.validation))
  })
  
  # Funktion zur Speicherung und zum Laden des nächsten Chromatogramms
  classify_and_next <- function(classification) {
    if (current_index() > nrow(all.data.mgrad.validation)) return()
    
    # Speichern der Klassifikation in der reaktiven Variable
    new_entry <- data.frame(ID = all.data.mgrad.validation$ID[current_index()], 
                            Classification = classification, 
                            stringsAsFactors = FALSE)
    
    classified_data(rbind(classified_data(), new_entry))
    
    # Automatisches Zwischenspeichern nach jedem 10. Eintrag
    if (nrow(classified_data()) %% 10 == 0) {
      write.csv(classified_data(), "classified_chromatograms.csv", row.names = FALSE)
      cat("✅ Zwischenspeicherung nach", nrow(classified_data()), "Einträgen.\n")
    }
    
    # Fortschreiten zum nächsten Chromatogramm
    if (current_index() < nrow(all.data.mgrad.validation)) {
      current_index(current_index() + 1)
    } else {
      # Endgültiges Speichern am Schluss
      write.csv(classified_data(), "classified_chromatograms.csv", row.names = FALSE)
      showModal(modalDialog(
        title = "Fertig!",
        "Alle Chromatogramme wurden klassifiziert. Die Datei wurde gespeichert.",
        easyClose = TRUE
      ))
    }
  }
  
  # Event-Listener für die Buttons
  observeEvent(input$btn_unauffaellig, { classify_and_next("unauffällig") })
  observeEvent(input$btn_mgradient, { classify_and_next("M-Gradient") })
  observeEvent(input$btn_suspicious, { classify_and_next("suspicious") })
  
  # Index für die unauffälligen Chromatogramme
  normal_index <- reactiveVal(1)
  
  # Funktion zum Extrahieren eines normalen Chromatogramms
  get_normal_chromatogram <- reactive({
    if (normal_index() > nrow(norm_data)) return(NULL)
    normal_row <- norm_data[normal_index(), ]
    
    x_values <- 1:300
    y_values <- as.numeric(normal_row[grep("^EP\\.y", names(normal_row))])
    
    data.frame(x = x_values, y = y_values, ID = normal_row$ID)
  })
  
  
  # Zeichne das normale Chromatogramm
  output$normalChromatogramPlot <- renderPlot({
    norm_data <- get_normal_chromatogram()
    if (is.null(norm_data)) return(NULL)
    
    ggplot(norm_data, aes(x = x, y = y)) +
      geom_line(color = "darkgreen") +
      labs(title = paste("Normales Chromatogramm ID:", norm_data$ID[1]),
           x = "x (Messpunkt)", y = "y (Intensität)") +
      theme_minimal()
  })
  
  # Navigation durch normale Chromatogramme
  observeEvent(input$prev_norm, {
    if (normal_index() > 1) {
      normal_index(normal_index() - 1)
    }
  })
  
  observeEvent(input$next_norm, {
    if (normal_index() < nrow(norm_data)) {
      normal_index(normal_index() + 1)
    }
  })
   
}

# Starte die App
shinyApp(ui = ui, server = server)