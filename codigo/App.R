# ==============================================================================
# APLICACIÓN SHINY: EXPLORADOR BIBLIOMÉTRICO, CURACIÓN Y NLP
# Arquitectura Basada en Proyectos - Rutas Seguras
# ==============================================================================

paquetes_requeridos <- c("shiny", "bslib", "DT", "dplyr", "tidytext", 
                         "ggplot2", "stringr", "httr", "jsonlite", "shinyFiles")
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

# 1. CAPA DE BACK-END
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
        select = "id,doi,title,publication_year,primary_location,authorships,concepts,type,cited_by_count,abstract_inverted_index",
        per_page = 200, mailto = email, cursor = cursor
      )
    )
    if (httr::status_code(res) != 200) {
      error_msg <- httr::content(res, "text", encoding = "UTF-8")
      warning(sprintf("\n¡Fallo en la API! (Página %s)\nCódigo HTTP: %s\nDetalle: %s\n", 
                      page_count, httr::status_code(res), error_msg))
      break
    }
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
    countries = vapply(all_results, function(x) {
      if (is.null(x$authorships) || length(x$authorships) == 0) return(NA_character_)
      cc <- unlist(lapply(x$authorships, function(a) { lapply(a$institutions, function(inst) inst$country_code) }))
      cc <- unique(cc[!is.na(cc) & cc != ""])
      if (length(cc) == 0) return(NA_character_)
      paste(cc, collapse = "; ")
    }, character(1)),
    concepts = vapply(all_results, function(x) {
      if (is.null(x$concepts) || length(x$concepts) == 0) return(NA_character_)
      cons <- unlist(lapply(x$concepts, function(c) c$display_name))
      if (length(cons) == 0) return(NA_character_)
      paste(head(cons, 6), collapse = "; ")
    }, character(1)),
    type = vapply(all_results, function(x) if(!is.null(x$type)) as.character(x$type) else NA_character_, character(1)),
    cited_by_count = vapply(all_results, function(x) if(!is.null(x$cited_by_count)) as.integer(x$cited_by_count) else NA_integer_, integer(1)),
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

# 2. INTERFAZ DE USUARIO (Front-end)
# ------------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Explorador y Curador Bibliométrico",
  theme = bs_theme(version = 5, bootswatch = "flatly"), 
  
  sidebar = sidebar(
    title = "Gestor de Proyectos",
    width = 370,
    
    # ---------------------------------------------------------
    # UI: CREAR PROYECTO (Integrado API y CSV)
    # ---------------------------------------------------------
    h6("Opción 1: Crear Nuevo Proyecto", class = "text-primary"),
    textInput("proj_name_new", "Nombre (Carpeta a crear):", value = "Mi_Tesis"),
    shinyDirButton("create_dir_btn", "Elegir Ubicación de Guardado", "Selecciona dónde quieres crear tu proyecto", class="btn-outline-primary btn-sm"),
    verbatimTextOutput("create_path_show"), 
    
    hr(style="border-top: 1px dotted #ccc;"),
    
    radioButtons("data_source", "Fuente de Datos Inicial:", 
                 choices = c("Extraer desde API OpenAlex" = "api", 
                             "Importar CSV Externo" = "csv")),
    
    conditionalPanel(
      condition = "input.data_source == 'api'",
      textInput("user_email", "Correo Institucional:", value = "tunombre@tuinstitucion.cl"),
      helpText(icon("info-circle"), "OpenAlex requiere un correo real (Polite Pool) para asignarte cuota gratuita.", class="text-muted", style="font-size: 0.85em; margin-top: -10px; margin-bottom: 15px;"),
      sliderInput("year_range", "Rango de Años:", min = 1990, max = as.integer(format(Sys.Date(), "%Y")), value = c(2015, as.integer(format(Sys.Date(), "%Y"))), sep = ""),
      textAreaInput("search_query", "Ecuación Booleana:", value = '("renewable energy") AND ("merit-order effect")', rows = 3)
    ),
    
    conditionalPanel(
      condition = "input.data_source == 'csv'",
      fileInput("upload_csv", "Subir archivo crudo o curado (.csv):", accept = c(".csv"), buttonLabel = "Explorar...")
    ),
    
    actionButton("create_proj_btn", "Crear Proyecto y Cargar Datos", class = "btn-primary", icon = icon("rocket")),
    
    hr(style="border-top: 2px dashed #bbb;"),
    
    # ---------------------------------------------------------
    # UI: CARGAR PROYECTO EXISTENTE
    # ---------------------------------------------------------
    h6("Opción 2: Cargar Proyecto Existente", class = "text-info"),
    shinyDirButton("load_dir_btn", "Seleccionar Carpeta del Proyecto", "Busca la carpeta de tu proyecto previamente creado", class="btn-outline-info btn-sm"),
    verbatimTextOutput("load_path_show"),
    actionButton("load_proj_btn", "Cargar Proyecto", class = "btn-info text-white", icon = icon("folder-open")),
    
    hr(),
    h6("Filtros de Texto (Capa 3)"),
    textInput("custom_stopwords", "Excluir palabras del gráfico:", value = "data, method, approach")
  ),
  
  navset_card_underline(
    title = "Flujo de Trabajo",
    
    nav_panel("Capa 1: Base de Datos", icon = icon("table"), DTOutput("results_table")),
    
    nav_panel("Capa 1.5: Setear Preguntas", icon = icon("list-check"), br(),
              fluidRow(
                column(5,
                       card(
                         card_header("1. Diseñar Pregunta", class = "bg-primary text-white"),
                         card_body(
                           textInput("q_name", "Nombre de la Pregunta"),
                           textAreaInput("q_desc", "Descripción de la pregunta"),
                           selectInput("q_type", "Tipo de Pregunta:", choices = c("Text Field", "Single Choice (Button)", "Multiple Choice")),
                           
                           conditionalPanel(
                             condition = "input.q_type != 'Text Field'",
                             hr(),
                             h6("Opciones (Choices)", class = "text-secondary"),
                             textInput("c_name", "Nombre Choice"),
                             textInput("c_acronym", "Acrónimo (Breve)"),
                             textInput("c_desc", "Descripción Breve"),
                             textInput("c_examples", "Ejemplos"),
                             actionButton("add_choice_btn", "Agregar Choice", icon = icon("plus"), class = "btn-light btn-sm mt-2"),
                             br(), br(),
                             DTOutput("current_choices_tbl")
                           ),
                           hr(),
                           actionButton("add_question_btn", "Agregar Pregunta al Set", class = "btn-dark", width = "100%", icon = icon("check-circle"))
                         )
                       )
                ),
                column(7,
                       card(
                         card_header("2. Set de Preguntas Actual", class = "bg-success text-white"),
                         card_body(
                           uiOutput("unsaved_warning_ui"), 
                           DTOutput("questions_set_tbl"),
                           hr(),
                           fluidRow(
                             column(6, actionButton("delete_last_q_btn", "Eliminar Última", class = "btn-danger", icon = icon("trash"), width = "100%")),
                             column(6, actionButton("save_set_btn", "Guardar Configuración", class = "btn-success", icon = icon("save"), width = "100%"))
                           )
                         )
                       )
                )
              )
    ),
    
    nav_panel("Capa 2: Curación Manual", icon = icon("edit"), br(), uiOutput("curation_ui")),
    nav_panel("Capa 3: Análisis NLP", icon = icon("chart-bar"), br(), plotOutput("nlp_freq_plot", height = "600px"))
  )
)

# 3. LÓGICA DEL SERVIDOR
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # ============================================================================
  # SOLUCIÓN DE SEGURIDAD (RUTAS DINÁMICAS PARA SHINYFILES)
  # ============================================================================
  # Detectamos la ruta de Documentos/Home de forma segura (evita colapsos de permisos)
  home_path <- normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE)
  
  # Creamos los accesos directos poniendo 'Documentos' como prioridad número 1
  safe_roots <- c(Documentos = home_path, shinyFiles::getVolumes()())
  
  shinyDirChoose(input, "create_dir_btn", roots = safe_roots, session = session)
  shinyDirChoose(input, "load_dir_btn", roots = safe_roots, session = session)
  
  # ============================================================================
  
  active_project_path <- reactiveVal(NULL) 
  raw_data <- reactiveVal(NULL)
  curated_data <- reactiveVal(NULL)
  current_row <- reactiveVal(1)
  
  current_choices <- reactiveVal(data.frame(Choice=character(), Acronimo=character(), Descripcion=character(), Ejemplos=character(), stringsAsFactors=FALSE))
  questions_set <- reactiveVal(data.frame(Pregunta=character(), Descripcion=character(), Tipo=character(), Opciones_Acronimos=character(), stringsAsFactors=FALSE))
  choices_list_master <- reactiveVal(list())
  
  unsaved_questions <- reactiveVal(FALSE) 
  
  # ============================================================================
  # GESTIÓN DE DIRECTORIOS DE SHINYFILES
  # ============================================================================
  create_base_path <- reactive({
    if (is.integer(input$create_dir_btn)) {
      return("Ninguna carpeta seleccionada")
    } else {
      return(parseDirPath(safe_roots, input$create_dir_btn))
    }
  })
  
  output$create_path_show <- renderText({
    if (create_base_path() == "Ninguna carpeta seleccionada") return("Destino: [Pendiente]")
    paste("Destino:", file.path(create_base_path(), input$proj_name_new))
  })
  
  load_proj_path <- reactive({
    if (is.integer(input$load_dir_btn)) {
      return("Ninguna carpeta seleccionada")
    } else {
      return(parseDirPath(safe_roots, input$load_dir_btn))
    }
  })
  
  output$load_path_show <- renderText({
    if (load_proj_path() == "Ninguna carpeta seleccionada") return("Carpeta Proyecto: [Pendiente]")
    paste("Proyecto:", load_proj_path())
  })
  
  # ============================================================================
  # GESTIÓN DE PROYECTOS (Integrada y Centralizada)
  # ============================================================================
  
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
    
    # RAMA A: Extraer desde API
    if (input$data_source == "api") {
      req(input$search_query, input$user_email)
      if(input$user_email == "tunombre@tuinstitucion.cl") {
        showNotification("Ingresa un correo válido antes de buscar.", type = "error")
        return()
      }
      
      id_notif <- showNotification("Consultando API y creando estructura...", duration = NULL, type = "message")
      
      res <- tryCatch({
        fetch_openalex_literature(input$search_query, input$user_email, input$year_range[1], input$year_range[2])
      }, error = function(e) { return(NULL) })
      
      removeNotification(id_notif)
      
      if (!is.null(res) && nrow(res) > 0) {
        res$status <- "Incluido" 
        write.csv2(res, file.path(dir_brutos, "dataset_crudo.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        
        active_project_path(proj_dir)
        raw_data(res)
        curated_data(res)
        current_row(1)
        showNotification(paste("Proyecto API creado exitosamente en:", proj_dir), type = "default", duration = 8)
      } else {
        showNotification("No se encontraron resultados o falló la conexión.", type = "warning")
      }
      
      # RAMA B: Importar desde CSV Externo
    } else if (input$data_source == "csv") {
      if(is.null(input$upload_csv)) {
        showNotification("Por favor selecciona un archivo CSV para importar.", type = "error")
        return()
      }
      
      id_notif <- showNotification("Importando archivo y creando estructura...", duration = NULL, type = "message")
      
      tryCatch({
        df <- read.csv(input$upload_csv$datapath, stringsAsFactors = FALSE)
        if (ncol(df) == 1) df <- read.csv2(input$upload_csv$datapath, stringsAsFactors = FALSE)
        
        if (!"abstract_text" %in% names(df)) {
          removeNotification(id_notif)
          showNotification("Error: Falta la columna 'abstract_text'.", type = "error")
          return()
        }
        if(!"status" %in% names(df)) df$status <- "Incluido"
        
        write.csv2(df, file.path(dir_brutos, "dataset_crudo.csv"), row.names = FALSE, fileEncoding = "UTF-8")
        
        active_project_path(proj_dir)
        raw_data(df)
        curated_data(df)
        current_row(1)
        removeNotification(id_notif)
        showNotification(paste("Proyecto importado exitosamente en:", proj_dir), type = "default", duration = 8)
      }, error = function(e) {
        removeNotification(id_notif)
        showNotification(paste("Error leyendo archivo:", e$message), type = "error")
      })
    }
  })
  
  observeEvent(input$load_proj_btn, {
    if (load_proj_path() == "Ninguna carpeta seleccionada") {
      showNotification("Por favor, selecciona la carpeta del proyecto a cargar.", type = "error")
      return()
    }
    
    proj_dir <- load_proj_path()
    dir_brutos <- file.path(proj_dir, "datos", "brutos")
    dir_procesados <- file.path(proj_dir, "datos", "procesados")
    dir_config <- file.path(proj_dir, "datos", "configuraciones")
    
    if (!dir.exists(file.path(proj_dir, "datos"))) {
      showNotification("La carpeta seleccionada no parece ser un proyecto válido (Falta carpeta 'datos').", type = "error")
      return()
    }
    
    file_to_load <- NULL
    status_msg <- "Sin datos de lectura."
    
    if (dir.exists(dir_procesados) && length(list.files(dir_procesados, pattern = "\\.csv$")) > 0) {
      files <- list.files(dir_procesados, pattern = "\\.csv$", full.names = TRUE)
      details <- file.info(files)
      file_to_load <- rownames(details)[order(details$mtime, decreasing = TRUE)[1]]
      status_msg <- "Datos: Progreso de curación cargado."
    } else if (dir.exists(dir_brutos) && file.exists(file.path(dir_brutos, "dataset_crudo.csv"))) {
      file_to_load <- file.path(dir_brutos, "dataset_crudo.csv")
      status_msg <- "Datos: Dataset original (bruto) cargado."
    }
    
    if (!is.null(file_to_load)) {
      tryCatch({
        df <- read.csv2(file_to_load, stringsAsFactors = FALSE)
        if (ncol(df) == 1) df <- read.csv(file_to_load, stringsAsFactors = FALSE)
        
        if (!"abstract_text" %in% names(df)) {
          showNotification("El archivo de datos está corrupto.", type = "error")
          return()
        }
        
        if(!"status" %in% names(df)) df$status <- "Incluido"
        
        active_project_path(proj_dir)
        raw_data(df)
        curated_data(df)
        current_row(1)
      }, error = function(e) {
        showNotification(paste("Error leyendo datos:", e$message), type = "error")
      })
    }
    
    config_msg <- "Config: Sin preguntas previas."
    
    if (dir.exists(dir_config)) {
      preguntas_files <- list.files(dir_config, pattern = "^Set_Preguntas_.*\\.csv$", full.names = TRUE)
      if (length(preguntas_files) > 0) {
        tryCatch({
          det_preg <- file.info(preguntas_files)
          latest_preg_file <- rownames(det_preg)[order(det_preg$mtime, decreasing = TRUE)[1]]
          
          df_preg <- read.csv2(latest_preg_file, stringsAsFactors = FALSE)
          if(ncol(df_preg) == 1) df_preg <- read.csv(latest_preg_file, stringsAsFactors = FALSE)
          questions_set(df_preg)
          
          choices_files <- list.files(dir_config, pattern = "^Choices_.*\\.csv$", full.names = TRUE)
          c_list <- list()
          for (cf in choices_files) {
            base_cf <- basename(cf)
            q_name_part <- sub("^Choices_(.*)_\\d{8}_\\d{4}\\.csv$", "\\1", base_cf)
            df_c <- read.csv2(cf, stringsAsFactors = FALSE)
            if(ncol(df_c) == 1) df_c <- read.csv(cf, stringsAsFactors = FALSE)
            c_list[[q_name_part]] <- df_c
          }
          choices_list_master(c_list)
          config_msg <- "Config: Libro de códigos restaurado."
          unsaved_questions(FALSE) 
        }, error = function(e) {
          showNotification(paste("Error restaurando preguntas:", e$message), type = "error")
        })
      }
    }
    showNotification(paste("Proyecto cargado desde:\n", proj_dir, "\n", status_msg, "\n", config_msg), type = "message", duration = 8)
  })
  
  # ============================================================================
  # LÓGICA DE CAPA 1.5: SETEAR PREGUNTAS Y ALERTAS
  # ============================================================================
  
  output$unsaved_warning_ui <- renderUI({
    if (unsaved_questions()) {
      tags$div(class = "alert alert-warning", icon("exclamation-triangle"), " Tienes cambios en las preguntas que NO han sido guardados.")
    } else {
      tags$div(class = "alert alert-success", icon("check-circle"), " Todas tus configuraciones están guardadas.")
    }
  })
  
  observeEvent(input$add_choice_btn, {
    if (input$c_name == "" || input$c_acronym == "") {
      showNotification("Debes ingresar al menos el 'Nombre Choice' y el 'Acrónimo'.", type = "warning")
      return()
    }
    new_choice <- data.frame(Choice = input$c_name, Acronimo = input$c_acronym, Descripcion = input$c_desc, Ejemplos = input$c_examples, stringsAsFactors = FALSE)
    current_choices(bind_rows(current_choices(), new_choice))
    
    updateTextInput(session, "c_name", value = "")
    updateTextInput(session, "c_acronym", value = "")
    updateTextInput(session, "c_desc", value = "")
    updateTextInput(session, "c_examples", value = "")
  })
  
  output$current_choices_tbl <- renderDT({ datatable(current_choices(), options = list(dom = 't', scrollX = TRUE), rownames = FALSE) })
  
  observeEvent(input$add_question_btn, {
    req(input$q_name)
    acronimos_str <- "N/A (Texto Libre)"
    if(input$q_type != "Text Field" && nrow(current_choices()) > 0) {
      acronimos_str <- paste(current_choices()$Acronimo, collapse = " | ")
    }
    
    new_q <- data.frame(Pregunta = input$q_name, Descripcion = input$q_desc, Tipo = input$q_type, Opciones_Acronimos = acronimos_str, stringsAsFactors = FALSE)
    questions_set(bind_rows(questions_set(), new_q))
    
    if(input$q_type != "Text Field" && nrow(current_choices()) > 0) {
      temp_list <- choices_list_master()
      safe_name <- gsub("[^A-Za-z0-9]", "_", input$q_name)
      temp_list[[safe_name]] <- current_choices()
      choices_list_master(temp_list)
    }
    
    updateTextInput(session, "q_name", value = "")
    updateTextAreaInput(session, "q_desc", value = "")
    current_choices(data.frame(Choice=character(), Acronimo=character(), Descripcion=character(), Ejemplos=character(), stringsAsFactors=FALSE))
    
    unsaved_questions(TRUE) 
    showNotification("Pregunta añadida al Set temporalmente.", type = "message")
  })
  
  observeEvent(input$delete_last_q_btn, {
    qs <- questions_set()
    if (nrow(qs) > 0) {
      last_q_name <- qs$Pregunta[nrow(qs)]
      qs <- qs[-nrow(qs), ]
      questions_set(qs)
      
      c_list <- choices_list_master()
      safe_name <- gsub("[^A-Za-z0-9]", "_", last_q_name)
      if (safe_name %in% names(c_list)) {
        c_list[[safe_name]] <- NULL
        choices_list_master(c_list)
      }
      
      unsaved_questions(TRUE) 
      showNotification("Última pregunta eliminada.", type = "warning")
    } else {
      showNotification("No hay preguntas para eliminar.", type = "warning")
    }
  })
  
  output$questions_set_tbl <- renderDT({ datatable(questions_set(), options = list(pageLength = 5, scrollX = TRUE), rownames = FALSE) })
  
  observeEvent(input$save_set_btn, {
    req(nrow(questions_set()) > 0)
    
    if (is.null(active_project_path())) {
      showNotification("Debes crear o cargar un proyecto formal para guardar la configuración.", type = "error")
      return()
    }
    
    target_dir <- file.path(active_project_path(), "datos", "configuraciones")
    if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M")
    
    tryCatch({
      write.csv2(questions_set(), file.path(target_dir, paste0("Set_Preguntas_", timestamp, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
      c_list <- choices_list_master()
      if(length(c_list) > 0) {
        for(q_name in names(c_list)) {
          write.csv2(c_list[[q_name]], file.path(target_dir, paste0("Choices_", q_name, "_", timestamp, ".csv")), row.names = FALSE, fileEncoding = "UTF-8")
        }
      }
      unsaved_questions(FALSE) 
      showNotification("¡Set de preguntas guardado exitosamente!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # ============================================================================
  # TABLAS Y CURACIÓN MANUAL
  # ============================================================================
  output$results_table <- renderDT({
    req(curated_data()) 
    datatable(
      curated_data(), extensions = 'Buttons',
      options = list(
        pageLength = 10, scrollX = TRUE, dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', exportOptions = list(orthogonal = 'export')),
          list(extend = 'csv', exportOptions = list(orthogonal = 'export')),
          list(extend = 'excel', exportOptions = list(orthogonal = 'export'))
        ),
        columnDefs = list(list(targets = 10, render = JS(
          "function(data, type, row, meta) {
             if (type === 'display' && data != null && data.length > 100) {
               return '<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>';
             } else { return data; }
           }"))),
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')
      ), rownames = FALSE, selection = "single"
    )
  }, server = FALSE)
  
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
            
            if (q_type == "Single Choice (Button)") {
              dynamic_ui <- tagAppendChild(dynamic_ui, selectInput(input_id, q_name, choices = choice_opts, selected = current_val, width = "100%"))
            } else if (q_type == "Multiple Choice") {
              sel_vals <- if(is.na(current_val) || current_val == "") character(0) else strsplit(as.character(current_val), "; ")[[1]]
              dynamic_ui <- tagAppendChild(dynamic_ui, selectizeInput(input_id, q_name, choices = choice_opts, selected = sel_vals, multiple = TRUE, width = "100%"))
            }
          }
        }
      }
    }
    
    card(
      card_header(class = "bg-primary text-white", paste("Evaluación:", idx, "de", nrow(df), "| Proyecto:", basename(active_project_path()))),
      card_body(
        p(strong("DOI: "), a(href = paste0("https://doi.org/", row_data$doi), target = "_blank", row_data$doi, class = "text-primary")),
        textInput("cur_title", "Título del Paper:", value = row_data$title, width = "100%"),
        textAreaInput("cur_abstract", "Abstract / Resumen:", value = row_data$abstract_text, rows = 6, width = "100%"),
        textInput("cur_concepts", "Conceptos Detectados:", value = row_data$concepts, width = "100%"),
        selectInput("cur_status", "Estado en la Revisión:", choices = c("Incluido", "Excluido"), selected = row_data$status),
        dynamic_ui
      ),
      card_footer(
        fluidRow(
          column(3, actionButton("prev_btn", "Anterior", icon = icon("arrow-left"), width = "100%")),
          column(3, actionButton("next_btn", "Siguiente", icon = icon("arrow-right"), width = "100%")),
          column(3, actionButton("save_row_btn", "Guardar Fila", class = "btn-warning", icon = icon("check"), width = "100%")),
          column(3, actionButton("export_btn", "Exportar Progreso", class = "btn-success", icon = icon("save"), width = "100%"))
        )
      )
    )
  })
  
  observeEvent(input$prev_btn, { if (current_row() > 1) current_row(current_row() - 1) })
  observeEvent(input$next_btn, { if (current_row() < nrow(curated_data())) current_row(current_row() + 1) })
  
  observeEvent(input$save_row_btn, {
    df <- curated_data()
    idx <- current_row()
    
    df$title[idx] <- input$cur_title
    df$abstract_text[idx] <- input$cur_abstract
    df$concepts[idx] <- input$cur_concepts
    df$status[idx] <- input$cur_status
    
    q_set <- questions_set()
    if (nrow(q_set) > 0) {
      for (i in 1:nrow(q_set)) {
        safe_name <- gsub("[^A-Za-z0-9]", "_", q_set$Pregunta[i])
        input_id <- paste0("dyn_q_", safe_name)
        val <- input[[input_id]]
        
        if (is.null(val)) val <- ""
        if (length(val) > 1) val <- paste(val, collapse = "; ") 
        
        if (!safe_name %in% names(df)) {
          df[[safe_name]] <- NA_character_
        }
        df[idx, safe_name] <- val
      }
    }
    
    curated_data(df)
    showNotification("Fila actualizada en memoria. Recuerda exportar tu progreso.", type = "message")
  })
  
  observeEvent(input$export_btn, {
    if (is.null(active_project_path())) return()
    
    df <- curated_data()
    target_dir <- file.path(active_project_path(), "datos", "procesados")
    if (!dir.exists(target_dir)) dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    
    file_name <- paste0("dataset_curado_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
    full_path <- file.path(target_dir, file_name)
    
    tryCatch({
      write.csv2(df, full_path, row.names = FALSE, fileEncoding = "UTF-8")
      showNotification(paste("Progreso exportado en:", full_path), type = "message", duration = 8)
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  # ============================================================================
  # CAPA 3: NLP
  # ============================================================================
  nlp_data <- reactive({
    req(curated_data())
    df <- curated_data() %>% filter(status == "Incluido")
    if(nrow(df) == 0) return(NULL)
    data("stop_words", package = "tidytext")
    base_stops <- c("study", "paper", "results", "analysis", "article", "model", "show", "can", "research", "based", "using", "also")
    user_stops <- input$custom_stopwords %>% str_split(",") %>% unlist() %>% str_trim() %>% tolower()
    custom_stops <- data.frame(word = unique(c(base_stops, user_stops)))
    tokens_clean <- df %>%
      select(id, abstract_text) %>%
      filter(!is.na(abstract_text) & abstract_text != "") %>% 
      unnest_tokens(word, abstract_text) %>%
      anti_join(stop_words, by = "word") %>%
      anti_join(custom_stops, by = "word") %>%
      filter(!str_detect(word, "^[0-9]+$")) 
    return(tokens_clean)
  })
  
  output$nlp_freq_plot <- renderPlot({
    req(nlp_data())
    top_words <- nlp_data() %>% count(word, sort = TRUE) %>% head(20)
    ggplot(top_words, aes(x = reorder(word, n), y = n)) +
      geom_col(fill = "#2C3E50", alpha = 0.9) +
      coord_flip() + theme_minimal(base_size = 14) +
      labs(title = "Conceptos más frecuentes en los Abstracts", subtitle = paste("Proyecto:", basename(active_project_path()), "- Filtro PRISMA: 'Incluido'"), x = "Término (Unigrama)", y = "Frecuencia absoluta") +
      theme(plot.title = element_text(face = "bold"), panel.grid.major.y = element_blank())
  })
}

shinyApp(ui = ui, server = server)