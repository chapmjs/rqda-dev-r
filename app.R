# Qualitative Analysis Tool
# R Shiny application for coding qualitative data

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(RMySQL)
library(DBI)
library(dplyr)
library(shinyjs)
library(colourpicker)

# Database connection function using environment variables
get_db_connection <- function() {
  tryCatch({
    dbConnect(
      MySQL(),
      host = Sys.getenv("DB_HOST"),
      dbname = Sys.getenv("DB_NAME"),
      user = Sys.getenv("DB_USER"),
      password = Sys.getenv("DB_PASS")
    )
  }, error = function(e) {
    cat("Database connection error:", e$message, "\n")
    return(NULL)
  })
}

# Initialize database tables if they don't exist
init_database <- function() {
  con <- get_db_connection()
  if (!is.null(con)) {
    # Create texts table
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS texts (
        id INT AUTO_INCREMENT PRIMARY KEY,
        title VARCHAR(255),
        content TEXT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")
    
    # Create codes table
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS codes (
        id INT AUTO_INCREMENT PRIMARY KEY,
        name VARCHAR(255) UNIQUE,
        description TEXT,
        color VARCHAR(7) DEFAULT '#3498db',
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")
    
    # Create coded_segments table
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS coded_segments (
        id INT AUTO_INCREMENT PRIMARY KEY,
        text_id INT,
        code_id INT,
        selected_text TEXT,
        start_pos INT,
        end_pos INT,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        FOREIGN KEY (text_id) REFERENCES texts(id),
        FOREIGN KEY (code_id) REFERENCES codes(id)
      )
    ")
    
    dbDisconnect(con)
  }
}

# Initialize database on app start
init_database()

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Qualitative Analysis Tool"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Text Analysis", tabName = "analysis", icon = icon("file-text")),
      menuItem("Manage Codes", tabName = "codes", icon = icon("tags")),
      menuItem("View Results", tabName = "results", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tags$head(
      tags$style(HTML("
        .selected-text {
          background-color: yellow !important;
          cursor: pointer;
        }
        .coded-segment {
          border-radius: 3px;
          padding: 2px;
          margin: 1px;
          cursor: pointer;
        }
        #text_display {
          border: 1px solid #ddd;
          padding: 15px;
          min-height: 400px;
          background-color: white;
          line-height: 1.6;
          font-family: Arial, sans-serif;
        }
      "))
    ),
    
    tabItems(
      # Text Analysis Tab
      tabItem(tabName = "analysis",
        fluidRow(
          box(
            title = "Text Input", status = "primary", solidHeader = TRUE, width = 12,
            textInput("text_title", "Title for this text:", placeholder = "Enter a title..."),
            textAreaInput("text_input", "Paste your text here:", 
                         height = "200px", 
                         placeholder = "Paste the text you want to analyze..."),
            br(),
            actionButton("load_text", "Load Text for Analysis", class = "btn-primary")
          )
        ),
        
        fluidRow(
          box(
            title = "Text Display", status = "info", solidHeader = TRUE, width = 8,
            div(id = "text_display", "Load text above to begin analysis...")
          ),
          
          box(
            title = "Coding Panel", status = "warning", solidHeader = TRUE, width = 4,
            h4("Selected Text:"),
            div(id = "selected_text_display", style = "border: 1px solid #ddd; padding: 10px; min-height: 60px; background-color: #f9f9f9;"),
            br(),
            selectInput("code_select", "Apply Code:", choices = NULL),
            actionButton("apply_code", "Apply Code", class = "btn-success"),
            br(), br(),
            h5("Quick Actions:"),
            actionButton("clear_selection", "Clear Selection", class = "btn-warning btn-sm"),
            actionButton("save_text", "Save Text", class = "btn-info btn-sm")
          )
        )
      ),
      
      # Manage Codes Tab
      tabItem(tabName = "codes",
        fluidRow(
          box(
            title = "Create New Code", status = "primary", solidHeader = TRUE, width = 6,
            textInput("new_code_name", "Code Name:", placeholder = "Enter code name..."),
            textAreaInput("new_code_description", "Description:", height = "100px"),
            colourInput("new_code_color", "Color:", value = "#3498db"),
            br(),
            actionButton("create_code", "Create Code", class = "btn-primary")
          ),
          
          box(
            title = "Existing Codes", status = "info", solidHeader = TRUE, width = 6,
            DT::dataTableOutput("codes_table")
          )
        )
      ),
      
      # Results Tab
      tabItem(tabName = "results",
        fluidRow(
          box(
            title = "Coded Segments", status = "success", solidHeader = TRUE, width = 12,
            DT::dataTableOutput("coded_segments_table")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values
  values <- reactiveValues(
    current_text_id = NULL,
    selected_text = "",
    codes = data.frame()
  )
  
  # Load codes on startup
  observe({
    con <- get_db_connection()
    if (!is.null(con)) {
      codes <- dbReadTable(con, "codes")
      dbDisconnect(con)
      
      if (nrow(codes) > 0) {
        values$codes <- codes
        choices <- setNames(codes$id, codes$name)
        updateSelectInput(session, "code_select", choices = choices)
      }
    }
  })
  
  # Load text for analysis
  observeEvent(input$load_text, {
    if (input$text_input != "" && input$text_title != "") {
      # Save text to database
      con <- get_db_connection()
      if (!is.null(con)) {
        result <- dbExecute(con, 
          "INSERT INTO texts (title, content) VALUES (?, ?)",
          params = list(input$text_title, input$text_input)
        )
        values$current_text_id <- dbGetQuery(con, "SELECT LAST_INSERT_ID() as id")$id[1]
        dbDisconnect(con)
      }
      
      # Display text with selection capability
      output$text_display <- renderUI({
        div(
          id = "text_display",
          HTML(paste0('<div id="selectable_text" onmouseup="getSelection()">', 
                     gsub("\n", "<br>", input$text_input), 
                     '</div>')),
          tags$script('
            function getSelection() {
              var selection = window.getSelection();
              if (selection.toString().length > 0) {
                Shiny.setInputValue("selected_text", selection.toString());
              }
            }
          ')
        )
      })
    }
  })
  
  # Handle text selection
  observe({
    if (!is.null(input$selected_text) && input$selected_text != "") {
      values$selected_text <- input$selected_text
      output$selected_text_display <- renderText({
        paste0('"', input$selected_text, '"')
      })
    }
  })
  
  # Create new code
  observeEvent(input$create_code, {
    if (input$new_code_name != "") {
      con <- get_db_connection()
      if (!is.null(con)) {
        tryCatch({
          dbExecute(con, 
            "INSERT INTO codes (name, description, color) VALUES (?, ?, ?)",
            params = list(input$new_code_name, input$new_code_description, input$new_code_color)
          )
          
          # Refresh codes
          codes <- dbReadTable(con, "codes")
          values$codes <- codes
          choices <- setNames(codes$id, codes$name)
          updateSelectInput(session, "code_select", choices = choices)
          
          # Clear inputs
          updateTextInput(session, "new_code_name", value = "")
          updateTextAreaInput(session, "new_code_description", value = "")
          
          showNotification("Code created successfully!", type = "success")
        }, error = function(e) {
          showNotification(paste("Error creating code:", e$message), type = "error")
        })
        dbDisconnect(con)
      }
    }
  })
  
  # Apply code to selected text
  observeEvent(input$apply_code, {
    if (!is.null(values$current_text_id) && values$selected_text != "" && !is.null(input$code_select)) {
      con <- get_db_connection()
      if (!is.null(con)) {
        dbExecute(con, 
          "INSERT INTO coded_segments (text_id, code_id, selected_text, start_pos, end_pos) VALUES (?, ?, ?, ?, ?)",
          params = list(values$current_text_id, input$code_select, values$selected_text, 0, nchar(values$selected_text))
        )
        dbDisconnect(con)
        
        showNotification("Code applied successfully!", type = "success")
        values$selected_text <- ""
        output$selected_text_display <- renderText("")
      }
    }
  })
  
  # Display codes table
  output$codes_table <- DT::renderDataTable({
    if (nrow(values$codes) > 0) {
      values$codes %>%
        select(name, description, color) %>%
        datatable(options = list(pageLength = 10, scrollX = TRUE))
    }
  })
  
  # Display coded segments
  output$coded_segments_table <- DT::renderDataTable({
    con <- get_db_connection()
    if (!is.null(con)) {
      segments <- dbGetQuery(con, "
        SELECT 
          t.title as text_title,
          c.name as code_name,
          cs.selected_text,
          cs.created_at
        FROM coded_segments cs
        JOIN texts t ON cs.text_id = t.id
        JOIN codes c ON cs.code_id = c.id
        ORDER BY cs.created_at DESC
      ")
      dbDisconnect(con)
      
      if (nrow(segments) > 0) {
        datatable(segments, options = list(pageLength = 15, scrollX = TRUE))
      }
    }
  })
  
  # Clear selection
  observeEvent(input$clear_selection, {
    values$selected_text <- ""
    output$selected_text_display <- renderText("")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
