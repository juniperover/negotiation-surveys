# Add this to your app before submission to check for file dependencies

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

# Call this function when your app starts
check_report_dependencies()

# Add this to your UI to show dependency warnings to admins
output$dependencyWarnings <- renderUI({
  req(input$debug == TRUE)
  
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

# Add a parameter to your UI to enable debugging
# Add this to your UI where appropriate
conditionalPanel(
  condition = "URLSearchParams && new URLSearchParams(window.location.search).has('debug')",
  div(
    style = "margin-top: 50px; border-top: 2px dashed #ccc; padding-top: 20px;",
    h3("Admin Debug Panel"),
    uiOutput("dependencyWarnings"),
    verbatimTextOutput("debugInfo")
  )
)