# ====== PLACEMENT INSTRUCTIONS ======
# This code should be added at the beginning of your server function in app.R file
# Specifically, add it right after the 'server <- function(input, output, session) {' line
# It runs once when the server starts and adds diagnostic capabilities to your app

# Function to check if all needed files for the report are present
check_report_dependencies <- function() {
  message("Checking report dependencies...")
  
  # Check for main template file
  if (!file.exists("report_template.Rmd")) {
    message("WARNING: report_template.Rmd not found in current directory")
    message("Current working directory: ", getwd())
    message("Files in current directory: ", paste(list.files(), collapse = ", "))
  } else {
    message("Found report_template.Rmd")
  }
  
  # Check for logo file
  logo_path <- "www/mbslogo.jpg"
  if (!file.exists(logo_path)) {
    message("WARNING: Logo file not found at ", logo_path)
    
    # Check if www directory exists
    if (!dir.exists("www")) {
      message("WARNING: www directory does not exist")
      dir.create("www", showWarnings = FALSE)
      message("Created www directory")
    } else {
      message("www directory exists")
      message("Files in www directory: ", paste(list.files("www"), collapse = ", "))
    }
  } else {
    message("Found logo file at ", logo_path)
  }
  
  # Check if svo.js file exists
  if (!file.exists("www/svo.js")) {
    message("WARNING: svo.js not found in www directory")
  } else {
    message("Found svo.js file")
  }
  
  # Check R packages needed for report rendering
  required_packages <- c("knitr", "rmarkdown", "dplyr", "ggplot2", "ggpubr", 
                         "stringr", "cowplot", "tidyr", "tools", "xtable", "grid")
  
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    message("WARNING: The following packages required for report rendering are missing: ", 
            paste(missing_packages, collapse = ", "))
  } else {
    message("All required R packages for report rendering are available")
  }
  
  # Check if the server can write to temp directory
  temp_test_file <- file.path(tempdir(), "write_test.txt")
  tryCatch({
    writeLines("test", temp_test_file)
    if (file.exists(temp_test_file)) {
      message("Successfully wrote to temporary directory at ", tempdir())
      file.remove(temp_test_file)
    }
  }, error = function(e) {
    message("WARNING: Cannot write to temporary directory: ", e$message)
  })
  
  # Check if rmarkdown can render HTML
  can_render_html <- tryCatch({
    if (requireNamespace("rmarkdown", quietly = TRUE)) {
      # Test if we can render a minimal Rmd to HTML
      test_rmd <- file.path(tempdir(), "test.Rmd")
      writeLines("---\ntitle: \"Test\"\noutput: html_document\n---\n\nTest", test_rmd)
      test_html <- tempfile(fileext = ".html")
      rmarkdown::render(test_rmd, output_file = test_html, quiet = TRUE)
      file.exists(test_html)
    } else {
      FALSE
    }
  }, error = function(e) {
    message("WARNING: Error testing rmarkdown HTML rendering: ", e$message)
    FALSE
  })
  
  if (can_render_html) {
    message("Successfully tested rmarkdown HTML rendering")
  } else {
    message("WARNING: Could not test rmarkdown HTML rendering")
  }
  
  message("Dependency check complete")
}

# Now add this code right after the server function definition:
server <- function(input, output, session) {
  
  # Run dependency check when the server starts (add this line)
  check_report_dependencies()
  
  # Create reactive values to store debugging information
  debug_mode <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    !is.null(query$debug) && query$debug == "true"
  })
  
  # Create debugging outputs
  output$dependencyWarnings <- renderUI({
    req(debug_mode())
    
    # Run checks and capture output
    log_output <- capture.output(check_report_dependencies())
    
    # Count warnings
    warning_count <- sum(grepl("WARNING", log_output))
    
    if (warning_count > 0) {
      div(
        style = "background-color: #ffe6e6; border-left: 4px solid #ff6666; padding: 15px; margin: 20px 0;",
        h4("Report Dependency Warnings"),
        p(paste("Found", warning_count, "potential issues with report generation.")),
        pre(paste(log_output, collapse = "\n")),
        p("These issues may affect report generation. Please address them to ensure reports work properly.")
      )
    } else {
      div(
        style = "background-color: #e6ffe6; border-left: 4px solid #66ff66; padding: 15px; margin: 20px 0;",
        h4("Report Dependencies OK"),
        p("All required dependencies for report generation appear to be available.")
      )
    }
  })
  
  output$debugInfo <- renderPrint({
    req(debug_mode())
    
    # System information
    cat("System Information:\n")
    cat("R version:", R.version.string, "\n")
    cat("Working directory:", getwd(), "\n")
    cat("Temp directory:", tempdir(), "\n")
    cat("User directory:", path.expand("~"), "\n\n")
    
    # Check if we're running in Posit Connect
    is_posit <- any(grepl("posit|rstudio|connect", tolower(names(Sys.getenv()))))
    cat("Running in Posit Connect:", is_posit, "\n\n")
    
    # List installed packages
    cat("Key packages:\n")
    for (pkg in c("rmarkdown", "knitr", "shiny", "googlesheets4", "dplyr", "ggplot2")) {
      if (requireNamespace(pkg, quietly = TRUE)) {
        pkg_version <- as.character(packageVersion(pkg))
        cat(pkg, "version:", pkg_version, "\n")
      } else {
        cat(pkg, "is NOT installed\n")
      }
    }
    cat("\n")
    
    # Check if pandoc is available (needed for Rmd rendering)
    cat("Pandoc availability:\n")
    if (rmarkdown::pandoc_available()) {
      cat("Pandoc is available, version:", rmarkdown::pandoc_version(), "\n")
    } else {
      cat("Pandoc is NOT available\n")
    }
    cat("\n")
    
    # Check environment variables
    cat("Environment variables of interest:\n")
    env_vars <- Sys.getenv()
    for (name in names(env_vars)) {
      if (grepl("r_|shiny|rstudio|posit|connect|path|home|user|google", tolower(name))) {
        value <- env_vars[name]
        if (grepl("key|token|secret|password", tolower(name))) {
          value <- "[REDACTED]"
        }
        cat(name, "=", value, "\n")
      }
    }
    cat("\n")
    
    # List files in important directories
    cat("Files in working directory:\n")
    print(list.files(getwd(), recursive = FALSE))
    cat("\n")
    
    cat("Files in temp directory:\n")
    print(list.files(tempdir(), recursive = FALSE))
  })
  
  # The rest of your server code goes here...
}

# ====== UI ADDITIONS ======
# Add this to your UI definition, at the end of your mainPanel section:

conditionalPanel(
  condition = "URLSearchParams && new URLSearchParams(window.location.search).has('debug')",
  div(
    style = "margin-top: 50px; border-top: 2px dashed #ccc; padding-top: 20px;",
    h3("Admin Debug Panel"),
    uiOutput("dependencyWarnings"),
    verbatimTextOutput("debugInfo")
  )
)