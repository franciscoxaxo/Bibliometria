# ==============================================================================
# APLICACIÓN SHINY: EXPLORADOR BIBLIOMÉTRICO, CURACIÓN Y NLP
# Arquitectura Basada en Proyectos (PRISMA) - Versión Multiusuario & Consenso
# ==============================================================================

# 0. AUTO-INSTALADOR DE PAQUETES
# ------------------------------------------------------------------------------
paquetes_requeridos <- c("shiny", "bslib", "DT", "dplyr", "tidytext", 
                         "ggplot2", "stringr", "httr", "jsonlite", "shinyFiles",
                         "plotly", "wordcloud2", "tidyr", "visNetwork", 
                         "countrycode", "igraph")

paquetes_faltantes <- paquetes_requeridos[!(paquetes_requeridos %in% installed.packages()[,"Package"])]
if(length(paquetes_faltantes)) {
  message("Instalando paquetes necesarios. Esto puede tomar unos minutos...")
  install.packages(paquetes_faltantes, dependencies = TRUE)
}

library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(tidytext)
library(ggplot2)
library(stringr)
library(httr)
library(jsonlite)
library(shinyFiles)
library(plotly)
library(wordcloud2)
library(tidyr)
library(visNetwork)
library(countrycode)
library(igraph)

# 1. CAPA DE BACK-END (Conexión Directa y Extracción Blindada)
# ------------------------------------------------------------------------------
fetch_openalex_literature <- function(query, email, start_year, end_year) {
  url <- "https://api.openalex.org/works"
  filtros_fecha <- paste0("from_publication_date:", start_year, "-01-01",
                          ",to_publication_date:", end_year, "-12-31")
  all_results <- list()
  cursor <- "*"
  page_count <- 1
  max_pages <- 50 
  
  while (page_count <= max_pages) {
    res <- httr::GET(
      url,
      query = list(
        search = query, filter = filtros_fecha,
        select = "id,doi,title,publication_year,primary_location,authorships,concepts,keywords,type,cited_by_count,abstract_inverted_index,referenced_works",
        per_page = 200, mailto = email, cursor = cursor
      )
    )
    if (httr::status_code(res) != 200) break
    parsed <- jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    if (is.null(parsed$results) || length(parsed$results) == 0) break
    
    all_results <- c(all_results, parsed$results)
    cursor <- parsed$meta$next_cursor
    if (is.null(cursor)) break
    page_count <- page_count + 1
  }
  
  if (length(all_results) == 0) return(NULL)
  
  df_final <- data.frame(
    id = vapply(all_results, function(x) if(!is.null(x$id)) gsub("https://openalex.org/", "", as.character(x$id)) else NA_character_, character(1)),
    doi = vapply(all_results, function(x) if(!is.null(x$doi)) gsub("https://doi.org/", "", as.character(x$doi)) else NA_character_, character(1)),
    title = vapply(all_results, function(x) if(!is.null(x$title)) as.character(x$title) else NA_character_, character(1)),
    journal = vapply(all_results, function(x) if(!is.null(x$primary_location$source$display_name)) as.character(x$primary_location$source$display_name) else NA_character_, character(1)),
    publication_year = vapply(all_results, function(x) if(!is.null(x$publication_year)) as.integer(x$publication_year) else NA_integer_, integer(1)),
    authors = vapply(all_results, function(x) {
      if (is.null(x$authorships) || length(x$authorships) == 0) return(NA_character_)
      names <- unlist(lapply(x$authorships, function(a) a$author$display_name))
      if (length(names) == 0) return(NA_character_)
      paste(names, collapse = "; ")
    }, character(1)),
    affiliations = vapply(all_results, function(x) {
      if (is.null(x$authorships) || length(x$authorships) == 0) return(NA_character_)
      affils <- unlist(lapply(x$authorships, function(a) {
        if(length(a$institutions) > 0) sapply(a$institutions, function(inst) inst$display_name) else NA
      }))
      affils <- unique(affils[!is.na(affils) & affils != ""])
      if (length(affils) == 0) return(NA_character_)
      paste(affils, collapse = "; ")
    }, character(1)),
    countries = vapply(all_results, function(x) {
      if (is.null(x$authorships) || length(x$authorships) == 0) return(NA_character_)
      cc <- unlist(lapply(x$authorships, function(a) { lapply(a$institutions, function(inst) inst$country_code) }))
      cc <- unique(cc[!is.na(cc) & cc != ""])
      if (length(cc) == 0) return(NA_character_)
      paste(cc, collapse = "; ")
    }, character(1)),
    concepts = vapply(all_results, function(x) {
      if (!is.null(x$concepts) && length(x$concepts) > 0) {
        cons <- unlist(lapply(x$concepts, function(c) c$display_name))
        return(paste(head(cons, 8), collapse = "; "))
      } else if (!is.null(x$keywords) && length(x$keywords) > 0) {
        kws <- sapply(x$keywords, function(k) if(!is.null(k$display_name)) k$display_name else k$keyword)
        return(paste(unique(kws[!is.na(kws) & kws != ""]), collapse = "; "))
      }
      return(NA_character_)
    }, character(1)),
    type = vapply(all_results, function(x) if(!is.null(x$type)) as.character(x$type) else NA_character_, character(1)),
    cited_by_count = vapply(all_results, function(x) if(!is.null(x$cited_by_count)) as.integer(x$cited_by_count) else NA_integer_, integer(1)),
    referenced_works = vapply(all_results, function(x) {
      if (is.null(x$referenced_works) || length(x$referenced_works) == 0) return(NA_character_)
      ids <- gsub("https://openalex.org/", "", as.character(x$referenced_works))
      paste(ids, collapse = "; ")
    }, character(1)),
    stringsAsFactors = FALSE
  )
  
  df_final$abstract_text <- vapply(all_results, function(x) {
    inv_index <- x[["abstract_inverted_index"]]
    if (is.null(inv_index) || length(inv_index) == 0) return(NA_character_)
    words <- names(inv_index)
    if (is.null(words) || length(words) == 0) return(NA_character_)
    text_vec <- c() 
    for (w in words) {
      pos <- unlist(inv_index[[w]]) + 1
      if (max(pos) > length(text_vec)) length(text_vec) <- max(pos)
      text_vec[pos] <- w
    }
    text_vec[is.na(text_vec)] <- ""
    return(paste(text_vec, collapse = " "))
  }, character(1))
  
  return(df_final)
}

# Funciones de Reglas de Consenso Matemáticas
resolve_status <- function(st_vec, rule) {
  st_clean <- tolower(trimws(st_vec))
  n_total <- length(st_clean[st_clean != ""])
  if(n_total == 0) return("Excluido")
  
  n_inc <- sum(grepl("inclu", st_clean), na.rm=TRUE)
  n_exc <- sum(grepl("exclu", st_clean), na.rm=TRUE)
  
  if(rule == "split") {
    # Tomar el primero que no esté vacío (asume que no se pisan)
    res <- st_clean[st_clean != ""][1]
    if(is.na(res)) return("Excluido")
    return(tools::toTitleCase(res))
  } else if(rule == "majority") {
    if(n_inc > n_total/2) return("Incluido")
    else if(n_exc > n_total/2) return("Excluido")
    else return("CONFLICTO")
  } else if(rule == "absolute") {
    if(n_inc == n_total) return("Incluido")
    else if(n_exc == n_total) return("Excluido")
    else return("CONFLICTO")
  } else if(rule == "judge") {
    if(n_inc == n_total) return("Incluido")
    else if(n_exc == n_total) return("Excluido")
    else return("EN REVISIÓN (JUEZ)")
  } else {
    # Individual: Toma el primero que encuentre
    return(tools::toTitleCase(st_clean[1]))
  }
}

resolve_metadata <- function(vals, rule) {
  v_clean <- unique(trimws(vals[!is.na(vals) & trimws(vals) != ""]))
  if(length(v_clean) == 0) return(NA_character_)
  if(length(v_clean) == 1) return(v_clean)
  
  if(rule == "union") {
    # Fusiona ambos y quita duplicados internos separados por ";"
    all_terms <- unlist(strsplit(v_clean, ";\\s*"))
    return(paste(unique(trimws(all_terms)), collapse = "; "))
  } else { 
    # Conflicto marcado
    return(paste0("[CONFLICTO: ", paste(v_clean, collapse = " vs "), "]"))
  }
}

# 2. INTERFAZ DE USUARIO (Front-end)
# ------------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Explorador Bibliométrico y Consenso PRISMA",
  theme = bs_theme(version = 5, bootswatch = "flatly"), 
  
  tags$head(tags$style(HTML("
    .jump-box .form-group { margin-bottom: 0px !important; }
    .jump-box input { height: 30px; text-align: center; padding: 2px 5px; }
  "))),
  
  sidebar = sidebar(
    title = "Gestor de Proyectos",
    width = 370,
    
    h6("Opción 1: Crear Nuevo Proyecto", class = "text-primary"),
    textInput("proj_name_new", "Nombre (Carpeta a crear):", value = "Mi_Tesis"),
    shinyDirButton("create_dir_btn", "Elegir Ubicación", "Selecciona dónde crear tu proyecto", class="btn-outline-primary btn-sm"),
    verbatimTextOutput("create_path_show"), 
    
    radioButtons("data_source", "Fuente de Datos Inicial:", 
                 choices = c("API OpenAlex" = "api", "Importar CSV" = "csv")),
    
    conditionalPanel(
      condition = "input.data_source == 'api'",
      textInput("user_email", "Correo Institucional:", value = "tunombre@tuinstitucion.cl"),
      sliderInput("year_range", "Años:", min = 1990, max = as.integer(format(Sys.Date(), "%Y")), value = c(2015, as.integer(format(Sys.Date(), "%Y"))), sep = ""),
      textAreaInput("search_query", "Ecuación Booleana:", value = '("renewable energy") AND ("merit-order effect")', rows = 3)
    ),
    conditionalPanel(
      condition = "input.data_source == 'csv'",
      fileInput("upload_csv", "Subir CSV:", accept = c(".csv"))
    ),
    
    actionButton("create_proj_btn", "Crear Proyecto y Datos", class = "btn-primary", icon = icon("rocket")),
    
    hr(style="border-top: 2px dashed #bbb;"),
    
    h6("Opción 2: Cargar Proyecto", class = "text-info"),
    shinyDirButton("load_dir_btn", "Seleccionar Proyecto", "Busca la carpeta", class="btn-outline-info btn-sm"),
    verbatimTextOutput("load_path_show"),
    actionButton("load_proj_btn", "Cargar Proyecto", class = "btn-info text-white", icon = icon("folder-open")),
    
    hr(style="border-top: 2px dashed #bbb;"),
    
    h6("Herramientas Avanzadas", class = "text-warning"),
    helpText(icon("exclamation-triangle"), "Minería profunda: Extrae autores citados (~3 min)."),
    actionButton("enrich_authors_btn", "Minería: Autores Citados", class = "btn-warning text-dark", icon = icon("cogs"), width = "100%")
  ),
  
  navset_card_underline(
    title = "Flujo de Trabajo Colaborativo",
    
    nav_panel("Capa 1: Base de Datos", icon = icon("table"), DTOutput("results_table")),
    
    nav_panel("Capa 1.5: Setear Preguntas", icon = icon("list-check"), br(),
              fluidRow(
                column(5, card(card_header("1. Diseñar Pregunta", class = "bg-primary text-white"), card_body(
                  textInput("q_name", "Nombre de la Pregunta"), textAreaInput("q_desc", "Descripción"), selectInput("q_type", "Tipo de Pregunta:", choices = c("Text Field", "Single Choice (Button)", "Multiple Choice")),
                  conditionalPanel(condition = "input.q_type != 'Text Field'", hr(), h6("Opciones (Choices)"), textInput("c_name", "Nombre Choice"), textInput("c_acronym", "Acrónimo"), textInput("c_desc", "Descripción Breve"), textInput("c_examples", "Ejemplos"), actionButton("add_choice_btn", "Agregar Choice", icon = icon("plus"), class = "btn-light btn-sm mt-2"), br(), br(), DTOutput("current_choices_tbl")), hr(), actionButton("add_question_btn", "Agregar Pregunta", class = "btn-dark", width = "100%", icon = icon("check-circle"))))),
                column(7, card(card_header("2. Set de Preguntas Actual", class = "bg-success text-white"), card_body(
                  uiOutput("unsaved_warning_ui"), DTOutput("questions_set_tbl"), hr(), fluidRow(column(6, actionButton("delete_last_q_btn", "Eliminar Última", class = "btn-danger", icon = icon("trash"), width = "100%")), column(6, actionButton("save_set_btn", "Guardar Configuración", class = "btn-success", icon = icon("save"), width = "100%"))))))
              )
    ),
    
    # --------------------------------------------------------------------------
    # CAPA 2 MODIFICADA: CON ROLES
    # --------------------------------------------------------------------------
    nav_panel("Capa 2: Curación (Revisores)", icon = icon("user-edit"), br(), 
              div(class="alert alert-secondary", style="display:flex; align-items:center; gap:15px;",
                  strong("Identificación de Revisor:"),
                  textInput("reviewer_name", label=NULL, value="Revisor_A", placeholder="Ej: Revisor_A", width="200px"),
                  span("Tus avances se guardarán en un archivo CSV propio para garantizar el ciego.")
              ),
              uiOutput("curation_ui")),
    
    # --------------------------------------------------------------------------
    # NUEVA CAPA 2.5: SALA DE CONSENSO
    # --------------------------------------------------------------------------
    nav_panel("Capa 2.5: Sala de Consenso", icon = icon("handshake"), br(),
              fluidRow(
                column(4, 
                       card(
                         card_header("1. Motor de Fusión PRISMA", class = "bg-primary text-white"),
                         card_body(
                           fileInput("consensus_files", "Sube los CSV de los revisores (Puedes seleccionar varios a la vez):", multiple = TRUE, accept = ".csv"),
                           hr(),
                           radioButtons("cons_rule", "Regla de Inclusión/Exclusión (Status):",
                                        choices = c("1. Investigador Individual (Toma el primer dato)" = "individual",
                                                    "2. Consenso y Juez (Marca empates para dirimir)" = "judge",
                                                    "3. Mayoría Simple (>50% de votos)" = "majority",
                                                    "4. Consenso Absoluto (Unanimidad exigida)" = "absolute",
                                                    "5. División del Trabajo (Sin superposición)" = "split"), selected = "judge"),
                           hr(),
                           radioButtons("meta_rule", "Regla ante Disenso en Metadatos (Tus preguntas):",
                                        choices = c("A. Unión Inclusiva (Fusionar opciones divergentes)" = "union",
                                                    "B. Intervención (Marcar con etiqueta [CONFLICTO])" = "conflict"), selected = "union"),
                           hr(),
                           actionButton("run_consensus_btn", "Cruzar Datos y Generar Consenso", class="btn-success", width="100%", icon=icon("cogs"))
                         )
                       )
                ),
                column(8,
                       card(
                         card_header("2. Dataset Maestro Consolidado", class = "bg-info text-white"),
                         card_body(
                           helpText("Revisa el resultado matemático de la fusión. Los conflictos pueden resolverse cargando este dataset maestro en la Capa 2 usando el rol 'Juez'."),
                           DTOutput("consensus_table"),
                           hr(),
                           actionButton("export_consensus_btn", "Exportar Dataset Maestro y Enviar a Capa 3", class="btn-dark text-white", width="100%", icon=icon("save"))
                         )
                       )
                )
              )
    ),
    
    # --------------------------------------------------------------------------
    # CAPA 3: ANALÍTICAS
    # --------------------------------------------------------------------------
    nav_panel("Capa 3: Análisis Visual", icon = icon("chart-pie"), br(),
              layout_sidebar(
                sidebar = sidebar(
                  title = "Dashboard Interactivo",
                  selectInput("viz_mode", "Seleccionar Gráfico a Mostrar:", 
                              choices = c("1. Flujo PRISMA (Sankey)" = "sankey",
                                          "2. Grafo de Conceptos Temáticos" = "concepts",
                                          "3. Nube de Palabras (Abstracts)" = "wordcloud",
                                          "4. Grafo de Raíces Intelectuales (ACA)" = "authors",
                                          "5. Geografía e Instituciones" = "geo")),
                  hr(),
                  radioButtons("viz_filter", "Filtro de Estado:", 
                               choices = c("Solo Incluidos" = "Incluido", 
                                           "Solo Excluidos" = "Excluido", 
                                           "Solo Conflictos / Empates" = "Conflicto",
                                           "Todos los Papers" = "Todos"), 
                               selected = "Incluido"),
                  uiOutput("viz_paper_count"), # Contador Robusto
                  hr(),
                  
                  conditionalPanel(condition = "input.viz_mode == 'concepts' || input.viz_mode == 'wordcloud'",
                                   h6("Filtros NLP"), textInput("custom_stopwords", "Excluir palabras:", value = "data, method, approach")
                  ),
                  conditionalPanel(condition = "input.viz_mode == 'sankey'",
                                   h6("Ejes Sankey"), selectInput("sankey_origen", "Origen:", choices = NULL), selectInput("sankey_destino", "Destino:", choices = NULL)
                  )
                ),
                
                # Paneles Perezosos (Lazy Loading para cuidar la RAM)
                conditionalPanel(condition = "input.viz_mode == 'sankey'", fluidRow(column(12, card(card_header("Flujo Relacional de Variables Curadas"), plotlyOutput("sankey_plot", height = "650px"))))),
                conditionalPanel(condition = "input.viz_mode == 'concepts'", fluidRow(column(12, card(card_header("Red de Co-ocurrencia de Conceptos Oficiales"), visNetworkOutput("concept_network_plot", height = "650px"))))),
                conditionalPanel(condition = "input.viz_mode == 'wordcloud'", fluidRow(column(12, card(card_header("Nube de Análisis de Texto (Abstracts)"), wordcloud2Output("nlp_wordcloud", height = "650px"))))),
                conditionalPanel(condition = "input.viz_mode == 'authors'", fluidRow(column(12, card(card_header("Análisis de Co-citación de Autores (ACA)"), visNetworkOutput("author_network_plot", height = "650px"))))),
                conditionalPanel(condition = "input.viz_mode == 'geo'", fluidRow(
                  column(12, card(card_header("Mapa de Cooperación Institucional"), visNetworkOutput("inst_network_plot", height = "450px"))),
                  column(12, card(card_header("Geografía Mundial de la Producción"), plotlyOutput("world_map_plot", height = "450px")))
                ))
              )
    )
  )
)

# 3. LÓGICA DEL SERVIDOR
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  home_path <- normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE)
  safe_roots <- c(Documentos = home_path, shinyFiles::getVolumes()())
  
  shinyDirChoose(input, "create_dir_btn", roots = safe_roots, session = session)
  shinyDirChoose(input, "load_dir_btn", roots = safe_roots, session = session)
  
  active_project_path <- reactiveVal(NULL) 
  raw_data <- reactiveVal(NULL)
  curated_data <- reactiveVal(NULL)
  consensus_data <- reactiveVal(NULL) # Para la Capa 2.5
  current_row <- reactiveVal(1)
  
  current_choices <- reactiveVal(data.frame(Choice=character(), Acronimo=character(), Descripcion=character(), Ejemplos=character(), stringsAsFactors=FALSE))
  questions_set <- reactiveVal(data.frame(Pregunta=character(), Descripcion=character(), Tipo=character(), Opciones_Acronimos=character(), stringsAsFactors=FALSE))
  choices_list_master <- reactiveVal(list())
  
  unsaved_questions <- reactiveVal(FALSE) 
  
  aplicar_filtro_doi <- function(df) {
    if(!"status" %in% names(df)) df$status <- "Incluido"
    condicion_vacia <- is.na(df$doi) | trimws(df$doi) == "" | toupper(trimws(df$doi)) %in% c("NA", "NULL", "NONE")
    df$status[condicion_vacia] <- "Excluido"
    return(df)
  }
  
  create_base_path <- reactive({
    if (is.integer(input$create_dir_btn)) return("Ninguna carpeta seleccionada")
    parseDirPath(safe_roots, input$create_dir_btn)
  })
  output$create_path_show <- renderText({
    if (create_base_path() == "Ninguna carpeta seleccionada") return("Destino: [Pendiente]")
    paste("Destino:", file.path(create_base_path(), input$proj_name_new))
  })
  
  load_proj_path <- reactive({
    if (is.integer(input$load_dir_btn)) return("Ninguna carpeta seleccionada")
    parseDirPath(safe_roots, input$load_dir_btn)
  })
  output$load_path_show <- renderText({
    if (load_proj_path() == "Ninguna carpeta seleccionada") return("Carpeta Proyecto: [Pendiente]")
    paste("Proyecto:", load_proj_path())
  })
  
  # === CREACIÓN Y CARGA DE PROYECTOS ===
  observeEvent(input$create_proj_btn, {
    req(input$proj_name_new)
    if (create_base_path() == "Ninguna carpeta seleccionada") {
      showNotification("Por favor, selecciona Dónde Guardar el proyecto primero.", type = "error")
      return()
    }
    proj_dir <- file.path(create_base_path(), input$proj_name_new)
    dir_brutos <- file.path(proj_dir, "datos", "brutos")
    dir_config <- file.path(proj_dir, "datos", "configuraciones")
    dir_procesados <- file.path(proj_dir, "datos", "procesados")
    
    dir.create(dir_brutos, recursive = TRUE, showWarnings = FALSE)
    dir.create(dir_config, recursive = TRUE, showWarnings = FALSE)
    dir.create(dir_procesados, recursive = TRUE, showWarnings = FALSE)
    
    if (input$data_source == "api") {
      req(input$search_query, input$user_email)
      id_notif <- showNotification("Consultando API y creando estructura...", duration = NULL, type = "message")
      res <- tryCatch({ fetch_openalex_literature(input$search_query, input$user_email, input$year_range[1], input$year_range[2]) }, error = function(e) { return(NULL) })
      removeNotification(id_notif)
      
      if (!is.null(res) && nrow(res) > 0) {
        res <- aplicar_filtro_doi(res) 
        write.csv2(res, file.path(dir_brutos, "dataset_crudo.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        active_project_path(proj_dir)
        raw_data(res)
        curated_data(res)
        current_row(1)
        showNotification("Proyecto API creado exitosamente.", type = "default", duration = 8)
      } else { showNotification("No se encontraron resultados o falló la conexión.", type = "warning") }
      
    } else if (input$data_source == "csv") {
      if(is.null(input$upload_csv)) { showNotification("Selecciona un CSV.", type = "error"); return() }
      id_notif <- showNotification("Importando archivo...", duration = NULL, type = "message")
      tryCatch({
        df <- read.csv(input$upload_csv$datapath, stringsAsFactors = FALSE)
        if (ncol(df) == 1) df <- read.csv2(input$upload_csv$datapath, stringsAsFactors = FALSE)
        df <- aplicar_filtro_doi(df) 
        write.csv2(df, file.path(dir_brutos, "dataset_crudo.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        active_project_path(proj_dir)
        raw_data(df)
        curated_data(df)
        current_row(1)
        removeNotification(id_notif)
        showNotification("Proyecto importado exitosamente.", type = "default", duration = 8)
      }, error = function(e) { removeNotification(id_notif); showNotification(paste("Error:", e$message), type = "error") })
    }
  })
  
  observeEvent(input$load_proj_btn, {
    if (load_proj_path() == "Ninguna carpeta seleccionada") { showNotification("Selecciona carpeta.", type = "error"); return() }
    proj_dir <- load_proj_path()
    dir_brutos <- file.path(proj_dir, "datos", "brutos")
    dir_procesados <- file.path(proj_dir, "datos", "procesados")
    dir_config <- file.path(proj_dir, "datos", "configuraciones")
    
    if (!dir.exists(file.path(proj_dir, "datos"))) { showNotification("Proyecto inválido.", type = "error"); return() }
    
    file_to_load <- NULL
    
    # 1. Prioridad: Dataset Maestro (Consenso)
    if (file.exists(file.path(dir_procesados, "dataset_curado_MAESTRO.csv"))) {
      file_to_load <- file.path(dir_procesados, "dataset_curado_MAESTRO.csv")
    } # 2. Prioridad: Dataset del Revisor Actual
    else if (!is.null(input$reviewer_name) && file.exists(file.path(dir_procesados, paste0("dataset_curado_", gsub(" ", "_", input$reviewer_name), ".csv")))) {
      file_to_load <- file.path(dir_procesados, paste0("dataset_curado_", gsub(" ", "_", input$reviewer_name), ".csv"))
    } # 3. Prioridad: Archivo genérico curado
    else if (file.exists(file.path(dir_procesados, "dataset_curado.csv"))) {
      file_to_load <- file.path(dir_procesados, "dataset_curado.csv")
    } # 4. Prioridad: Archivos sueltos viejos
    else if (length(list.files(dir_procesados, pattern = "\\.csv$")) > 0) {
      files <- list.files(dir_procesados, pattern = "\\.csv$", full.names = TRUE)
      details <- file.info(files)
      file_to_load <- rownames(details)[order(details$mtime, decreasing = TRUE)[1]]
    } # 5. Respaldo crudo
    else if (file.exists(file.path(dir_brutos, "dataset_crudo.csv"))) {
      file_to_load <- file.path(dir_brutos, "dataset_crudo.csv")
    }
    
    if (!is.null(file_to_load)) {
      tryCatch({
        df <- read.csv2(file_to_load, stringsAsFactors = FALSE)
        if (ncol(df) == 1) df <- read.csv(file_to_load, stringsAsFactors = FALSE)
        df <- aplicar_filtro_doi(df) 
        active_project_path(proj_dir)
        raw_data(df)
        curated_data(df)
      }, error = function(e) { showNotification(paste("Error leyendo datos:", e$message), type = "error") })
    }
    
    first_empty_row <- 1
    if (dir.exists(dir_config)) {
      if(file.exists(file.path(dir_config, "Set_Preguntas.csv"))) { latest_preg_file <- file.path(dir_config, "Set_Preguntas.csv")
      } else {
        preguntas_files <- list.files(dir_config, pattern = "^Set_Preguntas.*\\.csv$", full.names = TRUE)
        if(length(preguntas_files) > 0) { det_preg <- file.info(preguntas_files); latest_preg_file <- rownames(det_preg)[order(det_preg$mtime, decreasing = TRUE)[1]]
        } else { latest_preg_file <- NULL }
      }
      
      if (!is.null(latest_preg_file)) {
        tryCatch({
          df_preg <- read.csv2(latest_preg_file, stringsAsFactors = FALSE)
          if(ncol(df_preg) == 1) df_preg <- read.csv(latest_preg_file, stringsAsFactors = FALSE)
          questions_set(df_preg)
          
          if(nrow(df_preg) > 0 && !is.null(curated_data())) {
            first_q_col <- gsub("[^A-Za-z0-9]", "_", df_preg$Pregunta[1])
            temp_df <- curated_data()
            if (first_q_col %in% names(temp_df)) {
              empty_idx <- which(is.na(temp_df[[first_q_col]]) | trimws(temp_df[[first_q_col]]) == "")
              if (length(empty_idx) > 0) first_empty_row <- empty_idx[1]
              else first_empty_row <- nrow(temp_df)
            }
          }
          
          choices_files <- list.files(dir_config, pattern = "^Choices_.*\\.csv$", full.names = TRUE)
          c_list <- list()
          for (cf in choices_files) {
            base_cf <- basename(cf)
            q_name_part <- sub("^Choices_(.*?)_?\\d*\\.?csv$", "\\1", base_cf) 
            q_name_part <- gsub(".csv", "", q_name_part, fixed = TRUE)
            df_c <- read.csv2(cf, stringsAsFactors = FALSE)
            if(ncol(df_c) == 1) df_c <- read.csv(cf, stringsAsFactors = FALSE)
            c_list[[q_name_part]] <- df_c
          }
          choices_list_master(c_list)
          unsaved_questions(FALSE) 
        }, error = function(e) { showNotification("Error de configuración.", type = "error") })
      }
    }
    current_row(first_empty_row)
    showNotification("Proyecto cargado.", type = "message")
  })
  
  observeEvent(input$enrich_authors_btn, {
    req(active_project_path(), curated_data())
    df <- curated_data()
    if(!"referenced_works" %in% names(df)) { showNotification("No hay columna de referencias.", type="error"); return() }
    all_refs <- unlist(strsplit(na.omit(df$referenced_works), ";\\s*")); all_refs <- unique(all_refs[all_refs != ""])
    if(length(all_refs) == 0) return()
    
    batches <- split(all_refs, ceiling(seq_along(all_refs)/50))
    ref_authors <- list()
    
    withProgress(message = 'Extrayendo Autores Citados', detail = "Esto tomará unos minutos...", value = 0, {
      for(i in seq_along(batches)) {
        b <- batches[[i]]; query_ids <- paste(b, collapse="|"); url <- "https://api.openalex.org/works"
        mail <- if(input$user_email == "") "mineria@bot.cl" else input$user_email
        res <- tryCatch(httr::GET(url, query = list(filter = paste0("openalex:", query_ids), select = "id,authorships", per_page = 50, mailto=mail)), error=function(e) NULL)
        if(!is.null(res) && httr::status_code(res) == 200) {
          parsed <- jsonlite::fromJSON(httr::content(res, "text", encoding="UTF-8"), simplifyVector=FALSE)
          if(length(parsed$results) > 0) {
            for(w in parsed$results) {
              w_id <- gsub("https://openalex.org/", "", w$id)
              if(length(w$authorships) > 0) {
                auths <- unlist(lapply(w$authorships, function(a) a$author$display_name))
                ref_authors[[w_id]] <- paste(auths, collapse=", ")
              }
            }
          }
        }
        incProgress(1/length(batches), detail = paste("Lote", i, "de", length(batches)))
      }
    })
    
    df$cited_authors <- sapply(df$referenced_works, function(rw) {
      if(is.na(rw) || rw == "") return(NA_character_)
      ids <- unlist(strsplit(rw, ";\\s*"))
      auths <- unlist(lapply(ids, function(id) ref_authors[[id]]))
      auths <- unique(auths[!is.na(auths) & auths != ""])
      if(length(auths) == 0) return(NA_character_)
      paste(auths, collapse="; ")
    })
    curated_data(df)
    target_dir <- file.path(active_project_path(), "datos", "procesados")
    rev_name <- gsub(" ", "_", input$reviewer_name)
    write.csv2(df, file.path(target_dir, paste0("dataset_curado_", rev_name, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
    showNotification("Minería completada.", type="message", duration = 8)
  })
  
  # === CAPA 1.5 ===
  output$unsaved_warning_ui <- renderUI({
    if (unsaved_questions()) tags$div(class = "alert alert-warning", icon("exclamation-triangle"), " Tienes cambios no guardados.")
    else tags$div(class = "alert alert-success", icon("check-circle"), " Configuraciones guardadas.")
  })
  observeEvent(input$add_choice_btn, {
    if (input$c_name == "" || input$c_acronym == "") return()
    new_choice <- data.frame(Choice = input$c_name, Acronimo = input$c_acronym, Descripcion = input$c_desc, Ejemplos = input$c_examples, stringsAsFactors = FALSE)
    current_choices(bind_rows(current_choices(), new_choice))
    updateTextInput(session, "c_name", value = ""); updateTextInput(session, "c_acronym", value = "")
  })
  output$current_choices_tbl <- renderDT({ datatable(current_choices(), options = list(dom = 't', scrollX = TRUE), rownames = FALSE) })
  observeEvent(input$add_question_btn, {
    req(input$q_name)
    acronimos_str <- "N/A (Texto Libre)"
    if(input$q_type != "Text Field" && nrow(current_choices()) > 0) acronimos_str <- paste(current_choices()$Acronimo, collapse = " | ")
    new_q <- data.frame(Pregunta = input$q_name, Descripcion = input$q_desc, Tipo = input$q_type, Opciones_Acronimos = acronimos_str, stringsAsFactors = FALSE)
    questions_set(bind_rows(questions_set(), new_q))
    if(input$q_type != "Text Field" && nrow(current_choices()) > 0) {
      temp_list <- choices_list_master()
      safe_name <- gsub("[^A-Za-z0-9]", "_", input$q_name)
      temp_list[[safe_name]] <- current_choices()
      choices_list_master(temp_list)
    }
    updateTextInput(session, "q_name", value = ""); current_choices(data.frame(Choice=character(), Acronimo=character(), Descripcion=character(), Ejemplos=character(), stringsAsFactors=FALSE)); unsaved_questions(TRUE) 
  })
  observeEvent(input$delete_last_q_btn, {
    qs <- questions_set()
    if (nrow(qs) > 0) {
      last_q_name <- qs$Pregunta[nrow(qs)]; qs <- qs[-nrow(qs), ]; questions_set(qs)
      c_list <- choices_list_master(); safe_name <- gsub("[^A-Za-z0-9]", "_", last_q_name)
      if (safe_name %in% names(c_list)) { c_list[[safe_name]] <- NULL; choices_list_master(c_list) }
      unsaved_questions(TRUE) 
    }
  })
  output$questions_set_tbl <- renderDT({ datatable(questions_set(), options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE) })
  observeEvent(input$save_set_btn, {
    req(nrow(questions_set()) > 0); if (is.null(active_project_path())) return()
    target_dir <- file.path(active_project_path(), "datos", "configuraciones")
    if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    tryCatch({
      write.csv2(questions_set(), file.path(target_dir, "Set_Preguntas.csv"), row.names = FALSE, fileEncoding = "UTF-8")
      c_list <- choices_list_master()
      if(length(c_list) > 0) { for(q_name in names(c_list)) write.csv2(c_list[[q_name]], file.path(target_dir, paste0("Choices_", q_name, ".csv")), row.names = FALSE, fileEncoding = "UTF-8") }
      unsaved_questions(FALSE); showNotification("Configuraciones guardadas.", type = "message")
    }, error = function(e) { showNotification(paste("Error:", e$message), type = "error") })
  })
  
  # === CAPA 1 Y 2 ===
  output$results_table <- renderDT({
    req(curated_data()) 
    df_vista <- curated_data() %>% select(-any_of(c("referenced_works", "cited_authors")))
    datatable(df_vista, extensions = 'Buttons', options = list(pageLength = 10, scrollX = TRUE, dom = 'Bfrtip', buttons = list('copy', 'csv', 'excel'), columnDefs = list(list(targets = "_all", render = JS("function(data, type, row, meta) { if (type === 'display' && typeof data === 'string' && data.length > 150) { var safe_data = data.replace(/['\"]/g, ''); return '<span title=\"' + safe_data + '\">' + data.substr(0, 150) + '...</span>'; } else { return data; } }")))), rownames = FALSE, selection = "single")
  }, server = TRUE)
  
  observeEvent(input$jump_btn, { val <- isolate(input$jump_row_val); if(is.numeric(val) && val >= 1 && val <= nrow(curated_data())) current_row(val) })
  observeEvent(input$jump_next_btn, { if (current_row() < nrow(curated_data())) current_row(current_row() + 1) })
  
  output$curation_ui <- renderUI({
    df <- curated_data()
    if (is.null(df) || nrow(df) == 0) return(h5("Carga o crea un proyecto primero."))
    idx <- current_row()
    row_data <- df[idx, ]
    
    dynamic_ui <- tagList()
    q_set <- questions_set()
    c_master <- choices_list_master()
    
    if (nrow(q_set) > 0) {
      dynamic_ui <- tagAppendChild(dynamic_ui, hr())
      dynamic_ui <- tagAppendChild(dynamic_ui, h5("📖 Rúbrica de Extracción", class = "text-info"))
      for (i in 1:nrow(q_set)) {
        q_name <- q_set$Pregunta[i]
        q_type <- q_set$Tipo[i]
        safe_name <- gsub("[^A-Za-z0-9]", "_", q_name) 
        input_id <- paste0("dyn_q_", safe_name)
        current_val <- if (safe_name %in% names(row_data)) row_data[[safe_name]] else ""
        
        if (q_type == "Text Field") {
          dynamic_ui <- tagAppendChild(dynamic_ui, textInput(input_id, q_name, value = current_val, width = "100%"))
        } else {
          choices_df <- c_master[[safe_name]]
          if (!is.null(choices_df) && nrow(choices_df) > 0) {
            choice_opts <- setNames(choices_df$Acronimo, paste0(choices_df$Choice, " (", choices_df$Acronimo, ")"))
            choice_opts <- c("Seleccionar..." = "", choice_opts)
            if (q_type == "Single Choice (Button)") { dynamic_ui <- tagAppendChild(dynamic_ui, selectInput(input_id, q_name, choices = choice_opts, selected = current_val, width = "100%"))
            } else if (q_type == "Multiple Choice") {
              sel_vals <- if(is.na(current_val) || current_val == "") character(0) else strsplit(as.character(current_val), "; ")[[1]]
              dynamic_ui <- tagAppendChild(dynamic_ui, selectizeInput(input_id, q_name, choices = choice_opts, selected = sel_vals, multiple = TRUE, width = "100%"))
            }
          }
        }
      }
    }
    
    safe_authors <- if("authors" %in% names(row_data)) row_data$authors else "Sin información"
    safe_affil <- if("affiliations" %in% names(row_data)) row_data$affiliations else "Sin información"
    safe_cited <- if("cited_authors" %in% names(row_data) && !is.na(row_data$cited_authors)) row_data$cited_authors else "Sin minería"
    
    # NUEVO: Permitir al Juez seleccionar estados de conflicto
    status_choices <- c("Incluido", "Excluido", "CONFLICTO", "EN REVISIÓN (JUEZ)")
    
    card(
      card_header(class = "bg-primary text-white", div(style = "display: flex; justify-content: space-between; align-items: center;", span(paste("Proyecto:", basename(active_project_path())), style="font-size: 1.1em; font-weight: bold;"), div(class = "jump-box", style = "display: flex; align-items: center; gap: 8px; background: rgba(0,0,0,0.1); padding: 4px 10px; border-radius: 20px;", span("Ir a evaluación:"), numericInput("jump_row_val", label = NULL, value = idx, min = 1, max = nrow(df), width = "70px"), span(paste("de", nrow(df))), actionButton("jump_btn", "Ir", class = "btn-light btn-sm"), actionButton("jump_next_btn", ">>", class = "btn-secondary btn-sm", title = "Avanzar sin guardar")))),
      card_body(
        div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px; border: 1px solid #dee2e6;", p(strong("DOI: "), a(href = paste0("https://doi.org/", row_data$doi), target = "_blank", row_data$doi, class = "text-primary"), style="margin-bottom: 5px;"), p(strong("Autores: "), safe_authors, style="margin-bottom: 5px;"), p(strong("Afiliaciones: "), safe_affil, style="margin-bottom: 5px; font-size: 0.9em; color: #555;"), p(strong("Autores Citados en Ref: "), safe_cited, style="margin-bottom: 0px; font-size: 0.85em; color: #888;")),
        textInput("cur_title", "Título del Paper:", value = row_data$title, width = "100%"),
        textAreaInput("cur_abstract", "Abstract / Resumen:", value = row_data$abstract_text, rows = 6, width = "100%"),
        textInput("cur_concepts", "Conceptos Temáticos:", value = if("concepts" %in% names(row_data)) row_data$concepts else "", width = "100%"),
        selectInput("cur_status", "Estado en la Revisión (PRISMA):", choices = status_choices, selected = trimws(row_data$status)),
        dynamic_ui
      ),
      card_footer(fluidRow(column(4, actionButton("prev_btn", "Anterior", icon = icon("arrow-left"), width = "100%")), column(8, actionButton("save_next_btn", "Guardar y Siguiente", class = "btn-success", icon = icon("save"), width = "100%"))))
    )
  })
  
  observeEvent(input$save_next_btn, {
    df <- curated_data(); idx <- current_row()
    df$title[idx] <- input$cur_title; df$abstract_text[idx] <- input$cur_abstract; if("concepts" %in% names(df)) df$concepts[idx] <- input$cur_concepts; df$status[idx] <- input$cur_status
    
    q_set <- questions_set()
    if (nrow(q_set) > 0) {
      for (i in 1:nrow(q_set)) {
        safe_name <- gsub("[^A-Za-z0-9]", "_", q_set$Pregunta[i]); input_id <- paste0("dyn_q_", safe_name); val <- input[[input_id]]
        if (is.null(val)) val <- ""; if (length(val) > 1) val <- paste(val, collapse = "; ") 
        if (!safe_name %in% names(df)) df[[safe_name]] <- NA_character_
        df[idx, safe_name] <- val
      }
    }
    curated_data(df)
    
    # GUARDADO ROBUSTO POR ROL
    if (!is.null(active_project_path())) {
      target_dir <- file.path(active_project_path(), "datos", "procesados")
      if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
      
      rev_name <- gsub(" ", "_", input$reviewer_name)
      if(is.null(rev_name) || rev_name == "") rev_name <- "Revisor"
      
      full_path <- file.path(target_dir, paste0("dataset_curado_", rev_name, ".csv"))
      tryCatch({ write.csv2(df, full_path, row.names = FALSE, fileEncoding = "UTF-8") }, error = function(e) {})
    }
    
    if (idx < nrow(df)) { current_row(idx + 1); showNotification("Guardado exitoso. Avanzando...", type = "message", duration = 2)
    } else { showNotification("¡Has evaluado el último paper!", type = "warning", duration = 5) }
  })
  
  # ============================================================================
  # NUEVA CAPA 2.5: MOTOR DE FUSIÓN Y CONSENSO
  # ============================================================================
  observeEvent(input$run_consensus_btn, {
    req(input$consensus_files)
    
    withProgress(message = 'Fusionando Revisiones...', value = 0, {
      # 1. Leer todos los CSV
      df_list <- lapply(input$consensus_files$datapath, function(path) {
        d <- read.csv(path, stringsAsFactors=FALSE)
        if(ncol(d) == 1) d <- read.csv2(path, stringsAsFactors=FALSE)
        return(d)
      })
      
      incProgress(0.3, detail = "Cruzando datos...")
      df_all <- bind_rows(df_list)
      
      # 2. Identificar columnas base y columnas dinámicas (rúbrica)
      base_cols <- c("id", "doi", "title", "journal", "publication_year", "authors", "affiliations", 
                     "countries", "concepts", "keywords", "type", "cited_by_count", 
                     "referenced_works", "abstract_text", "cited_authors")
      
      dyn_cols <- setdiff(names(df_all), c(base_cols, "status", "revisor_name"))
      
      # 3. Aplicar Matemáticas de Consenso agrupando por ID del Paper
      master_df <- df_all %>%
        group_by(id) %>%
        summarise(
          # Conservar la metadata bibliométrica intacta (tomamos la del primer revisor)
          across(any_of(base_cols), ~ first(na.omit(.))),
          
          # Resolver Status con la Regla elegida
          status = resolve_status(status, rule = input$cons_rule),
          
          # Resolver Metadatos Teóricos con la Regla elegida
          across(any_of(dyn_cols), ~ resolve_metadata(., rule = input$meta_rule)),
          
          .groups = "drop"
        )
      
      consensus_data(master_df)
      incProgress(1.0, detail = "Fusión completada.")
    })
    
    showNotification("Fusión Matemática Completada. Revisa el resultado.", type="message")
  })
  
  output$consensus_table <- renderDT({
    req(consensus_data())
    datatable(consensus_data() %>% select(-any_of(c("referenced_works", "cited_authors"))), 
              options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE)
  }, server = TRUE)
  
  observeEvent(input$export_consensus_btn, {
    req(consensus_data(), active_project_path())
    target_dir <- file.path(active_project_path(), "datos", "procesados")
    full_path <- file.path(target_dir, "dataset_curado_MAESTRO.csv")
    
    tryCatch({
      write.csv2(consensus_data(), full_path, row.names = FALSE, fileEncoding = "UTF-8")
      # Actualizar la memoria global para que la Capa 3 lo lea inmediatamente
      curated_data(consensus_data()) 
      showNotification("Dataset Maestro Exportado Exitosamente. La Capa 3 ya está actualizada.", type="message", duration = 8)
    }, error = function(e) {
      showNotification(paste("Error exportando Maestro:", e$message), type="error")
    })
  })
  
  # ============================================================================
  # CAPA 3: VISUALIZACIONES INTERACTIVAS DASHBOARD
  # ============================================================================
  viz_data <- reactive({
    req(curated_data())
    df <- curated_data()
    if(!"status" %in% names(df)) df$status <- "Incluido"
    
    val_filtro <- input$viz_filter
    if(is.null(val_filtro)) val_filtro <- "Incluido"
    
    if(val_filtro == "Conflicto") {
      df <- df[toupper(trimws(df$status)) %in% c("CONFLICTO", "EN REVISIÓN (JUEZ)"), ]
    } else if(val_filtro != "Todos") {
      df <- df[tolower(trimws(df$status)) == tolower(trimws(val_filtro)), ]
    }
    
    df$temp_id <- seq_len(nrow(df))
    return(df)
  })
  
  output$viz_paper_count <- renderUI({
    df_tot <- curated_data()
    if(is.null(df_tot) || nrow(df_tot) == 0) return(HTML("<div class='alert alert-warning text-center mt-3 mb-0' style='padding:8px;'>Sin datos</div>"))
    
    tot <- nrow(df_tot)
    inc <- if("status" %in% names(df_tot)) sum(grepl("inclu", tolower(df_tot$status)), na.rm = TRUE) else tot
    exc <- if("status" %in% names(df_tot)) sum(grepl("exclu", tolower(df_tot$status)), na.rm = TRUE) else 0
    conf <- if("status" %in% names(df_tot)) sum(toupper(trimws(df_tot$status)) %in% c("CONFLICTO", "EN REVISIÓN (JUEZ)"), na.rm = TRUE) else 0
    
    HTML(paste0(
      "<div style='padding: 10px; border-radius: 5px; background-color: #f1f3f5; border: 1px solid #ced4da; text-align: center; margin-top: 15px; font-size: 0.95em;'>",
      "<span style='color: #2c3e50; font-size: 1.1em;'><strong>Total Muestra: </strong>", tot, "</span><br>",
      "<div style='margin-top: 5px;'>",
      "<span style='color: #27ae60; margin-right: 15px;'><strong>✔ Incluidos: </strong>", inc, "</span>",
      "<span style='color: #e74c3c; margin-right: 15px;'><strong>✖ Excluidos: </strong>", exc, "</span><br>",
      "<span style='color: #d35400;'><strong>⚠️ Conflictos: </strong>", conf, "</span>",
      "</div></div>"
    ))
  })
  
  # 1. RED DE CONCEPTOS
  output$concept_network_plot <- renderVisNetwork({
    df <- viz_data()
    if (is.null(df) || nrow(df) == 0) return(visNetwork(data.frame(id=1, label="Sin datos"), data.frame(from=character(), to=character())))
    
    target_col <- if("concepts" %in% names(df)) "concepts" else if("keywords" %in% names(df)) "keywords" else NULL
    if(is.null(target_col)) return(NULL)
    
    kw_df <- df %>% select(temp_id, all_of(target_col)) %>% rename(word_list = !!sym(target_col)) %>% filter(!is.na(word_list) & word_list != "") %>% tidyr::separate_rows(word_list, sep = ";\\s*") %>% mutate(word = tolower(trimws(word_list))) %>% filter(word != "")
    user_stops <- isolate(input$custom_stopwords) %>% str_split(",") %>% unlist() %>% str_trim() %>% tolower()
    if(length(user_stops) > 0 && user_stops[1] != "") kw_df <- kw_df %>% filter(!word %in% user_stops)
    if(nrow(kw_df) == 0) return(NULL)
    
    concepts_df <- kw_df %>% select(temp_id, word) %>% distinct()
    top_concepts <- concepts_df %>% count(word, sort = TRUE) %>% head(30)
    co_concepts <- concepts_df %>% filter(word %in% top_concepts$word) %>% rename(from = word) %>% inner_join(concepts_df %>% filter(word %in% top_concepts$word) %>% rename(to = word), by = "temp_id", relationship = "many-to-many") %>% filter(from < to) %>% count(from, to, name = "value") 
    
    if(nrow(co_concepts) > 0) {
      umbral <- max(2, quantile(co_concepts$value, 0.40)); co_concepts <- co_concepts %>% filter(value >= umbral); co_concepts$title <- paste("Co-ocurrencias:", co_concepts$value)
    }
    
    if(nrow(co_concepts) == 0) return(NULL)
    g <- graph_from_data_frame(d = co_concepts, vertices = top_concepts, directed = FALSE)
    cl <- cluster_louvain(g)
    top_concepts$group <- as.character(membership(cl)[top_concepts$word])
    
    nodes <- data.frame(id = top_concepts$word, label = str_to_title(top_concepts$word), value = top_concepts$n, group = top_concepts$group, title = paste("Tema:", str_to_title(top_concepts$word), "<br>Apariciones:", top_concepts$n), stringsAsFactors = FALSE)
    visNetwork(nodes, co_concepts, width = "100%") %>% visNodes(shape = "dot", shadow = TRUE, font = list(size = 14, face = "Arial", color = "#2C3E50")) %>% visEdges(color = list(color = "#BDC3C7", opacity = 0.5), smooth = list(enabled = TRUE, type = "continuous"), scaling = list(min = 1, max = 15)) %>% visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) %>% visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -80, centralGravity = 0.01, springLength = 150), stabilization = list(enabled = TRUE, iterations = 300))
  })
  
  # 2. WORDCLOUD
  output$nlp_wordcloud <- renderWordcloud2({
    df <- viz_data()
    if(is.null(df) || nrow(df) == 0 || !"abstract_text" %in% names(df)) return(NULL)
    data("stop_words", package = "tidytext")
    base_stops <- c("study", "paper", "results", "analysis", "article", "model", "show", "can", "research", "based", "using", "also")
    user_stops <- isolate(input$custom_stopwords) %>% str_split(",") %>% unlist() %>% str_trim() %>% tolower()
    custom_stops <- data.frame(word = unique(c(base_stops, user_stops)))
    tokens <- df %>% select(temp_id, abstract_text) %>% filter(!is.na(abstract_text) & abstract_text != "") %>% unnest_tokens(word, abstract_text) %>% anti_join(stop_words, by = "word") %>% anti_join(custom_stops, by = "word") %>% filter(!str_detect(word, "^[0-9]+$")) 
    if(nrow(tokens) == 0) return(NULL)
    wordcloud2(tokens %>% count(word, sort = TRUE) %>% head(100), size = 0.6, color = "random-dark", shape = "circle")
  })
  
  # 3. SANKEY
  observe({
    q_set <- questions_set()
    if(nrow(q_set) > 0) {
      safe_names <- gsub("[^A-Za-z0-9]", "_", q_set$Pregunta); names(safe_names) <- q_set$Pregunta
      updateSelectInput(session, "sankey_origen", choices = safe_names, selected = safe_names[1])
      if(length(safe_names) > 1) updateSelectInput(session, "sankey_destino", choices = safe_names, selected = safe_names[2])
    }
  })
  
  output$sankey_plot <- renderPlotly({
    df <- viz_data()
    v_orig <- input$sankey_origen; v_dest <- input$sankey_destino
    if(is.null(df) || nrow(df) == 0 || is.null(v_orig) || is.null(v_dest)) return(NULL)
    if(!(v_orig %in% names(df) && v_dest %in% names(df))) return(NULL)
    
    sankey_df <- df %>% select(all_of(c(v_orig, v_dest))) %>% tidyr::drop_na() %>% tidyr::separate_rows(!!sym(v_orig), sep = ";\\s*") %>% tidyr::separate_rows(!!sym(v_dest), sep = ";\\s*") %>% filter(!!sym(v_orig) != "", !!sym(v_dest) != "") %>% count(!!sym(v_orig), !!sym(v_dest), name = "value")
    if(nrow(sankey_df) == 0) return(NULL)
    
    orig_nodes <- paste0(sankey_df[[v_orig]], " (Orig)"); dest_nodes <- paste0(sankey_df[[v_dest]], " (Dest)"); all_nodes <- unique(c(orig_nodes, dest_nodes))
    sankey_df$source <- match(orig_nodes, all_nodes) - 1; sankey_df$target <- match(dest_nodes, all_nodes) - 1; clean_nodes <- gsub(" \\(Orig\\)| \\(Dest\\)", "", all_nodes)
    plot_ly(type = "sankey", orientation = "h", node = list(label = clean_nodes, pad = 15, thickness = 20, line = list(color = "black", width = 0.5)), link = list(source = sankey_df$source, target = sankey_df$target, value = sankey_df$value)) %>% layout(font = list(size = 12))
  })
  
  # 4. ACA
  output$author_network_plot <- renderVisNetwork({
    df <- viz_data()
    if(is.null(df) || nrow(df) == 0 || !"cited_authors" %in% names(df)) return(NULL)
    df_valid <- df %>% filter(!is.na(cited_authors) & cited_authors != "")
    if(nrow(df_valid) == 0) return(NULL)
    
    authors_df <- df_valid %>% select(temp_id, cited_authors) %>% tidyr::separate_rows(cited_authors, sep = "[;,]\\s*") %>% filter(cited_authors != "") %>% distinct()
    top_authors <- authors_df %>% count(cited_authors, sort = TRUE) %>% head(30)
    if(nrow(top_authors) == 0) return(NULL)
    
    co_citations <- authors_df %>% filter(cited_authors %in% top_authors$cited_authors) %>% rename(from = cited_authors) %>% inner_join(authors_df %>% filter(cited_authors %in% top_authors$cited_authors) %>% rename(to = cited_authors), by = "temp_id", relationship = "many-to-many") %>% filter(from < to) %>% count(from, to, name = "value")
    if(nrow(co_citations) > 0) { umbral_fuerza <- max(2, quantile(co_citations$value, 0.40)); co_citations <- co_citations %>% filter(value >= umbral_fuerza); co_citations$title <- paste("Co-citas:", co_citations$value) } else { co_citations <- data.frame(from=character(), to=character(), value=numeric()) }
    
    nodes <- data.frame(id = top_authors$cited_authors, label = top_authors$cited_authors, value = top_authors$n, title = paste("Autor:", top_authors$cited_authors, "<br>Citas:", top_authors$n), stringsAsFactors = FALSE)
    visNetwork(nodes, co_citations, width = "100%") %>% visNodes(shape = "dot", shadow = TRUE, color = list(background = "#E67E22", border = "#D35400"), font = list(size=16, color="#2C3E50")) %>% visEdges(color = list(color = "#BDC3C7", opacity = 0.8), smooth = list(enabled = TRUE, type = "continuous"), scaling = list(min = 1, max = 15)) %>% visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) %>% visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -120, centralGravity = 0.005, springLength = 200), stabilization = list(enabled = TRUE, iterations = 500))
  })
  
  # 5. MAPA
  output$world_map_plot <- renderPlotly({
    df <- viz_data()
    if(is.null(df) || nrow(df) == 0 || !"countries" %in% names(df)) return(NULL)
    map_df <- df %>% select(temp_id, countries) %>% tidyr::drop_na() %>% tidyr::separate_rows(countries, sep = ";\\s*") %>% filter(countries != "") %>% count(countries, name = "papers")
    if(nrow(map_df) == 0) return(NULL)
    
    es_codigo_corto <- all(nchar(map_df$countries) <= 3); origen_codigo <- ifelse(es_codigo_corto, "iso2c", "country.name")
    map_df$iso3 <- countrycode::countrycode(map_df$countries, origin = origen_codigo, destination = "iso3c", warn = FALSE)
    map_df$country_name <- countrycode::countrycode(map_df$countries, origin = origen_codigo, destination = "country.name.es", warn = FALSE)
    map_df <- map_df %>% filter(!is.na(iso3))
    if(nrow(map_df) == 0) return(NULL)
    
    plot_geo(map_df) %>% add_trace(z = ~papers, locations = ~iso3, color = ~papers, colors = "YlOrRd", text = ~paste("País:", country_name, "<br>Papers:", papers), hoverinfo = "text", marker = list(line = list(color = toRGB("grey"), width = 0.5))) %>% colorbar(title = "Papers") %>% layout(geo = list(showframe = FALSE, showcoastlines = TRUE, projection = list(type = 'equirectangular')), margin = list(l = 0, r = 0, t = 0, b = 0)) %>% config(displayModeBar = FALSE)
  })
  
  # 6. INSTITUCIONES
  output$inst_network_plot <- renderVisNetwork({
    df <- viz_data()
    if(is.null(df) || nrow(df) == 0 || !"affiliations" %in% names(df)) return(NULL)
    df_valid <- df %>% filter(!is.na(affiliations) & affiliations != "")
    if(nrow(df_valid) == 0) return(NULL)
    
    affil_df <- df_valid %>% select(temp_id, affiliations) %>% tidyr::separate_rows(affiliations, sep = ";\\s*") %>% filter(affiliations != "") %>% mutate(affiliations = str_to_title(trimws(affiliations))) %>% distinct()
    top_inst <- affil_df %>% count(affiliations, sort = TRUE) %>% head(35)
    if(nrow(top_inst) == 0) return(NULL)
    
    co_affil <- affil_df %>% filter(affiliations %in% top_inst$affiliations) %>% rename(from = affiliations) %>% inner_join(affil_df %>% filter(affiliations %in% top_inst$affiliations) %>% rename(to = affiliations), by = "temp_id", relationship = "many-to-many") %>% filter(from < to) %>% count(from, to, name = "value")
    if(nrow(co_affil) > 0) {
      co_affil$title <- paste("Colaboraciones:", co_affil$value)
      g <- graph_from_data_frame(d = co_affil, vertices = top_inst, directed = FALSE); cl <- cluster_louvain(g); top_inst$group <- as.character(membership(cl)[top_inst$affiliations])
    } else { top_inst$group <- "1"; co_affil <- data.frame(from=character(), to=character(), value=numeric()) }
    
    nodes <- data.frame(id = top_inst$affiliations, label = top_inst$affiliations, value = top_inst$n, group = top_inst$group, title = paste("Institución:", top_inst$affiliations, "<br>Papers:", top_inst$n), stringsAsFactors = FALSE)
    visNetwork(nodes, co_affil, width = "100%") %>% visNodes(shape = "dot", shadow = TRUE, font = list(size = 14)) %>% visEdges(color = list(color = "#BDC3C7", opacity = 0.5), smooth = list(enabled = TRUE, type = "continuous"), scaling = list(min = 1, max = 15)) %>% visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) %>% visPhysics(solver = "forceAtlas2Based", forceAtlas2Based = list(gravitationalConstant = -80, centralGravity = 0.01, springLength = 150), stabilization = list(enabled = TRUE, iterations = 300))
  })
}

shinyApp(ui = ui, server = server)