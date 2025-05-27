library(shiny)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(this.path)
library(shinyjs)  
library(knitr)
library(rmarkdown)
library(tools)
library(openxlsx)
library(xtable)
library(grid)
library(ggpubr)
library(stringr)
library(cowplot)
library(tidyr)

setwd(this.path::here())

# More robust Google Sheets authentication
# First check if the package is loaded
if (!requireNamespace("googlesheets4", quietly = TRUE)) {
  stop("The googlesheets4 package is not available. Please install it.")
}

# Log authentication attempt
message("Starting Google Sheets authentication process...")

# Try different authentication methods
auth_successful <- FALSE

# Method 1: Try environment variable with key path
key_path <- Sys.getenv("new_google_key", "")
if (key_path != "") {
  message("Found google_sheets_key environment variable")
  
  # Check if it's a file path or JSON content
  if (file.exists(key_path)) {
    message("Key appears to be a file path")
    tryCatch({
      googlesheets4::gs4_auth(path = key_path)
      message("Successfully authenticated with key file")
      auth_successful <- TRUE
    }, error = function(e) {
      message("Error authenticating with key file: ", e$message)
    })
  } else if (grepl("^\\s*\\{", key_path)) {
    # Looks like JSON content
    message("Key appears to be JSON content")
    tryCatch({
      temp_key_file <- tempfile(fileext = ".json")
      writeLines(key_path, temp_key_file)
      googlesheets4::gs4_auth(path = temp_key_file)
      message("Successfully authenticated with JSON content")
      auth_successful <- TRUE
    }, error = function(e) {
      message("Error authenticating with JSON content: ", e$message)
    })
  } else {
    message("Key environment variable exists but doesn't appear to be a file or JSON content")
  }
}

# Method 2: Try token-based authentication if available
if (!auth_successful) {
  message("Trying token-based authentication...")
  tryCatch({
    googlesheets4::gs4_auth()
    message("Successfully authenticated with token")
    auth_successful <- TRUE
  }, error = function(e) {
    message("Error authenticating with token: ", e$message)
  })
}

# Method 3: Try deauthentication (for public sheets)
if (!auth_successful) {
  message("Trying to deauthenticate (for public sheets)...")
  tryCatch({
    googlesheets4::gs4_deauth()
    message("Successfully deauthenticated (for public sheets)")
    auth_successful <- TRUE
  }, error = function(e) {
    message("Error deauthenticating: ", e$message)
  })
}

# If all authentication methods fail, stop the app
if (!auth_successful) {
  stop("Failed to authenticate with Google Sheets. Please check your credentials.")
}

# Test the authentication by listing sheets (optional)
tryCatch({
  sheet_id <- "19cbvKPdMlg7vr9XBNZsVdkbsrQl0nETpatpcJW2_9nU"
  sheet_properties <- googlesheets4::gs4_get(sheet_id)
  message("Successfully connected to sheet: ", sheet_properties$name)
}, error = function(e) {
  message("Error connecting to sheet: ", e$message)
  # Don't stop here, as the sheet might not exist yet
})

message("Google Sheets authentication process completed")

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  
  # Add custom CSS
  tags$head(
    tags$style(HTML("
    /* General font size increase */
    body {
      font-size: 16px; /* Base font size */
    }
    
    /* Increase font size for headings */
    h4 {
      font-size: 20px;
    }
    
    h5 {
      font-size: 18px;
    }
    
    /* Increase font size for paragraphs */
    p {
      font-size: 16px;
    }
    
    /* Increase font size for rating items */
    .rating-item {
      font-size: 14px;
    }
    
    /* Increase font size for buttons */
    .btn {
      font-size: 16px;
    }
    
    /* Increase font size for input labels */
    .control-label {
      font-size: 16px;
    }
    
    /* Increase font size for text inputs */
    .form-control {
      font-size: 16px;
    }
    
    /* Increase font size for well panels */
    .well {
      font-size: 16px;
    }
    
    /* Custom styles for other elements */
    .rating-container {
      display: flex;
      justify-content: space-between;
      margin-bottom: 20px;
    }
    .rating-item {
      text-align: center;
      width: 14%;
      cursor: pointer;
      padding: 5px;
      border-radius: 5px;
      font-size: 14px; /* Adjusted font size */
    }
    .rating-item:hover {
      background-color: #e0e0e0;
    }
    .selected {
      background-color: #007bff;
      color: white;
    }
    .error-message {
      color: red;
      font-weight: bold;
    }
    #logo {
      position: absolute;
      top: 10px;
      right: 10px;
      max-width: 150px;
      height: auto;
    }
    .button-container {
      display: flex;
      align-items: center;
      gap: 10px;
    }
    .disabled-button {
      background-color: #cccccc !important;
      cursor: not-allowed;
    }
    /* SVO slider styles */
    .svo-container {
      margin: 30px 0;
      position: relative;
    }
    .svo-slider-container {
      display: flex;
      flex-direction: column;
      align-items: center;
      margin-bottom: 40px;
    }
    .svo-labels {
      display: flex;
      justify-content: center;
      width: 100%;
      margin: 10px 0;
      font-weight: bold;
      font-size: 16px;
    }
    .svo-top-label {
      margin-top: 10px;
    }
    .svo-bottom-label {
      margin-bottom: 10px;
    }
    .svo-values {
      width: 100%;
      margin-bottom: 10px;
    }
    .svo-values table {
      width: 100%;
    }
    .svo-instructions {
      background-color: #f8f9fa;
      border-left: 4px solid #007bff;
      padding: 15px;
      margin-bottom: 20px;
    }
/* Mobile-specific styles */
    @media (max-width: 768px) {
      body {
        font-size: 14px;
      }
      h4 {
        font-size: 18px;
      }
      h5 {
        font-size: 16px;
      }
      .rating-container {
        flex-direction: column;
        align-items: center;
      }
      .rating-item {
        width: 80%;
        margin-bottom: 10px;
      }
      .form-control {
        height: 40px;
        font-size: 16px;
      }
      .btn {
        padding: 10px 20px;
        font-size: 16px;
      }
      .sidebarLayout > .sidebarPanel {
        width: 100%;
        margin-top: 20px;
      }
      .sidebarLayout > .mainPanel {
        width: 100%;
      }
      .svo-container {
        overflow-x: auto;
      }
    }
    ")),
    
    # Add the reference to external JavaScript file here
    tags$script(src = "svo.js")
  ),
  
  fluidRow(
    column(8, titlePanel("Negotiation Behaviour Inventory")),
    column(4, tags$img(src = "mbslogo.jpg", height = "100px", style = "float:right;"))
  ),
  
  # Add the preamble
  wellPanel(
    HTML("<p>We ask you to reflect on your behaviours during negotiations in which you were personally involved. We define the term 'negotiation' in a broad sense. In addition to events commonly thought to involve negotiation, such as negotiating a job offer or making a purchase, negotiation can involve attempts to obtain cooperation from others, settle a dispute, resolve legal issues, or convince someone to follow your plan of action.</p>")
  ),
  
sidebarLayout(
  sidebarPanel(
    textInput("name", "Name (this name will appear on your feedback report):"),
    div(id = "name_error", class = "error-message"),
    textInput("email", "Email:"),
    div(id = "email_error", class = "error-message"),
    numericInput("age", "Age:", value = 18, min = 1, max = 120),
    
    # Modified gender question with self-identify option
    radioButtons("gender", "Gender:", 
                choices = c("Man", "Woman", "Prefer to self-identify")),
    conditionalPanel(
      condition = "input.gender == 'Prefer to self-identify'",
      textInput("gender_self", "Please specify:")
    )
  ),
  
  mainPanel(
    h4("Reflecting on how you negotiated in the past, to what extent would you do the following behaviours before a negotiation?"),
    h4("Each of the behaviours below is something people can do to prepare for a negotiation."),
    
    # Survey items: NPI
    lapply(1:21, function(i) {
              tagList(
          h5(switch(i,
                    "1. Spend time thinking about your counterpart's goals.",
                    "2. Spend time studying your counterpart's tactics and patterns of behaviour in similar situations.",
                    "3. Spend time studying the counterpart's emotional state as they enter into negotiations.",
                    "4. Spend time researching if your counterpart might value things differently than you.",
                    "5. Spend time researching the counterpart's point of view on the situation.",
                    "6. Exhaust every source of information at your disposal (including, but not limited to family, friends, co-workers, institutions, the library, and the Internet).",
                    "7. Research alternative outcomes that satisfy the need of the parties, (i.e., find alternative arrangements that meet you and your counterpart's needs).",
                    "8. Communicate in advance your intention to negotiate.",
                    "9. Make sure all relevant parties will be included in the negotiation.",
                    "10. Establish a shared perception of the situation that requires resolution.",
                    "11. Attempt to remove or minimise distractions that could draw attention away from the negotiation.",
                    "12. Spend time researching the events leading to the negotiation.",
                    "13. Spend time thinking about your goals.",
                    "14. Prioritise the goals for the negotiation.",
                    "15. Create a table or list of what you value most to what you value least.",
                    "16. Ensure that you have the necessary resources to follow through with the deal that you reach.",
                    "17. Maintain competence in the skills needed to analyze the deal (e.g., technical evaluation, accounting, developing relationships, etc.).",
                    "18. Understand your no-deal options.",
                    "19. Understand your counterpart's no-deal options.",
                    "20. Research the obstacles to an agreement that can result in a no-deal.",
                    "21. Spend time thinking about what you would do if the negotiation ended in a no-deal."
          )),
          div(
            class = "rating-container",
            lapply(1:7, function(j) {
              div(
                id = paste0("plan", i, "_", j),
                class = "rating-item",
                HTML(switch(j,
                            "1" = "1<br>Would not do<br>this at all",
                            "2" = "2",
                            "3" = "3",
                            "4" = "4<br>Would do this<br>somewhat",
                            "5" = "5",
                            "6" = "6",
                            "7" = "7<br>Would do this<br>a great deal"
                )),
                onclick = sprintf("Shiny.setInputValue('plan%d', %d, {priority: 'event'})", i, j)
              )
            })
          ),
          br()
        )
    }),
    
    h4("Reflecting on how you negotiated in the past, to what extent would you do the following behaviours during a negotiation?"),
    h4("Each of the behaviours is something people can do during a negotiation."),
    
    # During negotiation survey items
    lapply(1:26, function(i) {
      tagList(
          h5(switch(i,
                    "1. Have plans in advance to deal with counterproductive (e.g., negative, extreme, irrational) behaviour from your counterpart.",
                    "2. Ask questions to learn about your counterpart.",
                    "3. Test your understanding of what your counterpart was trying to say by repeating what they said.",
                    "4. Try to focus on satisfying your underlying needs rather than a specific list of requirements.",
                    "5. Spend time inventing options for mutual benefit.",
                    "6. Keep track of how much value is created by systematically evaluating each offer against previous offers.",
                    "7. Try to reach a deal that touches on all of the issues involved.",
                    "8. Review the final terms with your counterpart to confirm mutual agreement about their interpretation.",
                    "9. Make sure that you were fully understood.",
                    "10. Try to ensure all your interests were met in the agreement.",
                    "11. Check if your arguments are persuasive.",
                    "12. Check if your counterpart is telling the truth.",
                    "13. Check if the counterpart is committed to making a deal with you.",
                    "14. Make sure the help you are getting is appropriate for the task at hand.",
                    "15. Justify each offer with a reason.",
                    "16. Provide reasons that the counterpart should find plausible.",
                    "17. Present facts that support your offer.",
                    "18. Support your offer with facts.",
                    "19. Refuse to make concessions.",
                    "20. Insist on getting something every time you make a concession.",
                    "21. Ask for a lot in your offers.",
                    "22. Avoid making concessions.",
                    "23. Use your power to make the counterpart concede",
                    "24. Always try to get something in return for an accommodation you make",
                    "25. Make ambitious offers.",
                    "26. Downplay threats made by the counterpart"
          )),
          div(
            class = "rating-container",
            lapply(1:7, function(j) {
              div(
                id = paste0("barg", i, "_", j),
                class = "rating-item",
                HTML(switch(j,
                            "1" = "1<br>Would not do<br>this at all",
                            "2" = "2",
                            "3" = "3",
                            "4" = "4<br>Would do this<br>somewhat",
                            "5" = "5",
                            "6" = "6",
                            "7" = "7<br>Would do this<br>a great deal"
                )),
                onclick = sprintf("Shiny.setInputValue('barg%d', %d, {priority: 'event'})", i, j)
              )
            })
          ),
          br()
        )
    }),
    
    # New section for after negotiation behaviours
    h4("Reflecting on how you negotiated in the past, to what extent would you do the following behaviours following a negotiation?"),
    h4("Each of the behaviours is something people can do after reaching an agreement."),
    
    # After negotiation survey items
    lapply(1:8, function(i) {
      tagList(
          h5(switch(i,
                    "1. Express your commitment to the agreement.",
                    "2. Check that you have addressed your counterpart's key concerns.",
                    "3. Agree on a plan to implement every aspect of the agreement.",
                    "4. Monitor the counterpart's actions to check that they are working to implement the agreement.",
                    "5. Periodically seek advice about negotiating from a more experienced negotiator.",
                    "6. Periodically seek advice about negotiating from peers.",
                    "7. Check with your counterpart about their feelings about the negotiation.",
                    "8. Check with your counterpart's social network (e.g., their friends, family, colleagues, etc…) about the negotiation."
          )),
          div(
            class = "rating-container",
            lapply(1:7, function(j) {
              div(
                id = paste0("post", i, "_", j),
                class = "rating-item",
                HTML(switch(j,
                            "1" = "1<br>Would not do<br>this at all",
                            "2" = "2",
                            "3" = "3",
                            "4" = "4<br>Would do this<br>somewhat",
                            "5" = "5",
                            "6" = "6",
                            "7" = "7<br>Would do this<br>a great deal"
                )),
                onclick = sprintf("Shiny.setInputValue('post%d', %d, {priority: 'event'})", i, j)
              )
            })
          ),
          br()
        )
    }),
    
    # Need for Closure Scale
    h4("Need for Closure Scale"),
    h4("Please indicate the extent to which you agree or disagree with each of the following statements as descriptions of your preferences or personality."),
    
    # Survey items for Need for Closure
    lapply(1:15, function(i) {
      tagList(
          h5(switch(i,
                    "1. I don't like situations that are uncertain.",
                    "2. I dislike questions which could be answered in many different ways.",
                    "3. I find that a well ordered life with regular hours suits my temperament.",
                    "4. I feel uncomfortable when I don't understand the reason why an event occurred in my life.",
                    "5. I feel irritated when one person disagrees with what everyone else in a group believes.",
                    "6. I don't like to go into a situation without knowing what I can expect from it.",
                    "7. When I have made a decision, I feel relieved.",
                    "8. When I am confronted with a problem, I'm dying to reach a solution very quickly.",
                    "9. I would quickly become impatient and irritated if I would not find a solution to a problem immediately.",
                    "10. I don't like to be with people who are capable of unexpected actions.",
                    "11. I dislike it when a person's statement could mean many different things.",
                    "12. I find that establishing a consistent routine enables me to enjoy life more.",
                    "13. I enjoy having a clear and structured mode of life.",
                    "14. I do not usually consult many different opinions before forming my own view.",
                    "15. I dislike unpredictable situations."
          )),
          div(
            class = "rating-container",
            lapply(1:6, function(j) {
              div(
                id = paste0("nfc", i, "_", j),
                class = "rating-item",
                HTML(switch(j,
                            "1" = "1<br>Strongly<br>disagree",
                            "2" = "2<br>Moderately<br>disagree",
                            "3" = "3<br>Slightly<br>disagree",
                            "4" = "4<br>Slightly<br>agree",
                            "5" = "5<br>Moderately<br>agree",
                            "6" = "6<br>Strongly<br>agree"
                )),
                onclick = sprintf("Shiny.setInputValue('nfc%d', %d, {priority: 'event'})", i, j)
              )
            })
          ),
          br()
        )
    }),
    
    # Need for Cognition Scale
    h4("Need for Cognition Scale"),
    h4("Please indicate the extent to which each statement is characteristic of you."),
    
    # Survey items for Need for Cognition
    lapply(1:18, function(i) {
      tagList(
          h5(switch(i,
                    "1. I would prefer complex to simple problems.",
                    "2. I like to have the responsibility of handling a situation that requires a lot of thinking.",
                    "3. Thinking is not my idea of fun.",
                    "4. I would rather do something that requires little thought than something that is sure to challenge my thinking abilities.",
                    "5. I try to anticipate and avoid situations where there is likely chance I will have to think in depth about something.",
                    "6. I find satisfaction in deliberating hard and for long hours.",
                    "7. I only think as hard as I have to.",
                    "8. I prefer to think about small, daily projects to long-term ones.",
                    "9. I like tasks that require little thought once I've learned them.",
                    "10. The idea of relying on thought to make my way to the top appeals to me.",
                    "11. I really enjoy a task that involves coming up with new solutions to problems.",
                    "12. Learning new ways to think doesn't excite me very much.",
                    "13. I prefer my life to be filled with puzzles that I must solve.",
                    "14. The notion of thinking abstractly is appealing to me.",
                    "15. I would prefer a task that is intellectual, difficult, and important to one that is somewhat important but does not require much thought.",
                    "16. I feel relief rather than satisfaction after completing a task that required a lot of mental effort.",
                    "17. It's enough for me that something gets the job done; I don't care how or why it works.",
                    "18. I usually end up deliberating about issues even when they do not affect me personally."
          )),
          div(
            class = "rating-container",
            lapply(1:5, function(j) {
              div(
                id = paste0("ncs", i, "_", j),
                class = "rating-item",
                HTML(switch(j,
                            "1" = "1<br>Extremely<br>Uncharacteristic",
                            "2" = "2<br>Somewhat<br>Uncharacteristic",
                            "3" = "3<br>Uncertain",
                            "4" = "4<br>Somewhat<br>Characteristic",
                            "5" = "5<br>Extremely<br>Characteristic"
                )),
                onclick = sprintf("Shiny.setInputValue('ncs%d', %d, {priority: 'event'})", i, j)
              )
            })
          ),
          br()
        )
    }),
    
    # SVO Scale
    h4("Social Value Orientation"),
    div(class = "svo-instructions",
        HTML("<p>For this section, imagine you have been randomly paired with another person, whom we will refer to as the Other. 
              This Other person is someone you do not know and will remain mutually anonymous. All of your choices are completely confidential.</p>
              <p>You will be making a series of decisions about allocating resources between you and the Other. 
              For each of the following questions, please indicate the distribution you prefer most by moving the slider to your preferred position.</p>
              <p>Assume that your decisions would yield money for both yourself and the Other (not really, for course purposes, but answer as if it would). There are no right or wrong answers; 
              this is all about personal preferences. </p>")
    ),
    
    # SVO Slider items with corrected structure
    lapply(1:6, function(i) {
      tagList(
        h5(paste("SVO Item", i)),
        
        # Define the self/other values for each slider position
        tags$div(class = "svo-slider-container",
          # First the top label
          tags$div(class = "svo-labels svo-top-label",
                  tags$div(class = "svo-label", "You Receive")
          ),
          
          # Then the values display
          tags$div(id = paste0("svo_values_", i), class = "svo-values"),
          
          # Then the slider
          sliderInput(
            inputId = paste0("svo", i),
            label = NULL,
            min = 1,
            max = 9,
            value = 5,
            step = 1,
            width = "100%",
            ticks = FALSE
          ),
          
          # Then the bottom label
          tags$div(class = "svo-labels svo-bottom-label",
                  tags$div(class = "svo-label", "Other Receives")
          ),
          
          # Hidden inputs to store the actual values
          tags$div(style = "display: none;",
                  textInput(paste0("svo", i, "_self_value"), NULL, ""),
                  textInput(paste0("svo", i, "_other_value"), NULL, "")
          )
        )
      )
    }),
    
    # Add the submit button and download section
    tags$p("Click the 'Submit' button below. It will save your responses and after a few seconds, you will get a download link to your individualised feedback report below."),
    
    div(class = "button-container",
        actionButton("submit", "Submit"),
        uiOutput("downloadButton")
    ),
    
    tags$div(id = "loading", style = "display: none;",
             tags$p("Processing your responses and generating report. Please wait...")
    )
  )  # This closes mainPanel
)  # This closes sidebarLayout
)  # This closes fluidPage

# Server logic
server <- function(input, output, session) {
  
  is_valid_email <- function(x) {
    grepl("^[[:alnum:]._%-]+@[[:alnum:].-]+\\.[[:alpha:]]{2,}$", x)
  }
  
  # Reactive value to store the generated report
  report <- reactiveVal(NULL)
  
  # Add this observer to handle the styling of selected items for all question sets
  observe({
    # For 'item' questions
    lapply(1:21, function(i) {
      selected_value <- input[[paste0("plan", i)]]
      lapply(1:7, function(j) {
        if (!is.null(selected_value) && selected_value == j) {
          shinyjs::addClass(id = paste0("plan", i, "_", j), class = "selected")
        } else {
          shinyjs::removeClass(id = paste0("plan", i, "_", j), class = "selected")
        }
      })
    })
    
    # For 'barg' questions
    lapply(1:26, function(i) {
      selected_value <- input[[paste0("barg", i)]]
      lapply(1:7, function(j) {
        if (!is.null(selected_value) && selected_value == j) {
          shinyjs::addClass(id = paste0("barg", i, "_", j), class = "selected")
        } else {
          shinyjs::removeClass(id = paste0("barg", i, "_", j), class = "selected")
        }
      })
    })
    
    # For 'post' questions
    lapply(1:8, function(i) {
      selected_value <- input[[paste0("post", i)]]
      lapply(1:7, function(j) {
        if (!is.null(selected_value) && selected_value == j) {
          shinyjs::addClass(id = paste0("post", i, "_", j), class = "selected")
        } else {
          shinyjs::removeClass(id = paste0("post", i, "_", j), class = "selected")
        }
      })
    })
    
    # For 'nfc' questions (Need for Closure)
    lapply(1:15, function(i) {
      selected_value <- input[[paste0("nfc", i)]]
      lapply(1:6, function(j) {
        if (!is.null(selected_value) && selected_value == j) {
          shinyjs::addClass(id = paste0("nfc", i, "_", j), class = "selected")
        } else {
          shinyjs::removeClass(id = paste0("nfc", i, "_", j), class = "selected")
        }
      })
    })
    
    # For 'ncs' questions (Need for Cognition Scale)
    lapply(1:18, function(i) {
      selected_value <- input[[paste0("ncs", i)]]
      lapply(1:5, function(j) {
        if (!is.null(selected_value) && selected_value == j) {
          shinyjs::addClass(id = paste0("ncs", i, "_", j), class = "selected")
        } else {
          shinyjs::removeClass(id = paste0("ncs", i, "_", j), class = "selected")
        }
      })
    })
  })
  
  # Create downloadButton UI
  output$downloadButton <- renderUI({
    if (!is.null(report())) {
      downloadButton("downloadReport", "Download Your Report")
    }
  })
  
# Define downloadReport handler
output$downloadReport <- downloadHandler(
  filename = function() {
    paste("negotiation-report-", Sys.Date(), ".html", sep = "")
  },
  content = function(file) {
    # Copy the report file to a temporary directory
    tempReport <- file.path(tempdir(), "report_template.Rmd")
    file.copy("report_template.Rmd", tempReport, overwrite = TRUE)
    
    # Copy any assets (like images) to temp directory
    if(file.exists("www/mbslogo.jpg")) {
      dir.create(file.path(tempdir(), "www"), showWarnings = FALSE)
      file.copy("www/mbslogo.jpg", file.path(tempdir(), "www/mbslogo.jpg"), overwrite = TRUE)
    }
    
    # Generate the report data
    # report_data <- generate_report(user_responses)
    # The report will be generated when the download button is clicked
    # No need to call generate_report here anymore
    
    tryCatch({
      message("Starting report generation...")
      
      rmarkdown::render(tempReport, 
                      output_file = file, 
                      params = list(
                        name = user_responses$Name,
                        df = report_data$df,
                        planning_items = report_data$planning_items,
                        bargaining_items = report_data$bargaining_items,
                        implementation_items = report_data$implementation_items,
                        nfc_items = report_data$nfc_items,
                        ncs_items = report_data$ncs_items,
                        svo_angle = user_responses$SVO_angle,
                        svo_type = report_data$svo_type_label,
                        svo_items = report_data$svo_items
                      ),
                      envir = new.env(parent = globalenv()))
      
      message("Report generated successfully!")
      
    }, error = function(e) {
      message("Error generating report: ", e$message)
      print(e)
      writeLines(paste("Error generating report:", e$message), file)
    })
  }
)

# Updated generate_report function that returns the data
generate_report <- function(user_responses) {
  # Define benchmarks
  benchmarks <- data.frame(
    Category = c("pre_count", "pre_arena", "pre_preps", "pre_imp", "pre",
                 "barg_mut", "barg_com", "barg_arg", "barg_get", "barg",
                 "imp_imp", "imp_feedb", "imp",
                # New NFC categories
                 "nfc_order", "nfc_predictability", "nfc_decisiveness", 
                 "nfc_ambiguity", "nfc_closemindedness", "nfc_total",
                # New NCS category
                 "ncs_total",
                # SVO
                 "SVO_angle"),
    Benchmark = c(4.4, 4.61, 5.1, 4.37, 4.66, 
                  4.67, 5.28, 5.13, 3.62, 4.55,
                  5.26, 4.16, 4.82,
                  # New NFC benchmarks (replace with actual benchmarks if available)
                  4.2, 3.9, 4.1, 4.3, 3.8, 4.05,
                  # NCS benchmark (replace with actual benchmark if available)
                  3.5,
                  # SVO angle benchmark (average is typically around 30-35 degrees - prosocial)
                  32.1)
  )
  
  # Prepare data for the report
  summary_scores <- data.frame(
    Category = c("pre_count", "pre_arena", "pre_preps", "pre_imp", "pre",
                 "barg_mut", "barg_com", "barg_arg", "barg_get", "barg",
                 "imp_imp", "imp_feedb", "imp",
                # New NFC categories
                 "nfc_order", "nfc_predictability", "nfc_decisiveness", 
                 "nfc_ambiguity", "nfc_closemindedness", "nfc_total",
                # New NCS category
                 "ncs_total",
                # SVO
                 "SVO_angle"),
    Your_Response = c(
      user_responses$pre_count,
      user_responses$pre_arena,
      user_responses$pre_preps,
      user_responses$pre_imp,
      user_responses$pre,
      user_responses$barg_mut,
      user_responses$barg_com,
      user_responses$barg_arg,
      user_responses$barg_get,
      user_responses$barg,
      user_responses$imp_imp,
      user_responses$imp_feedb,
      user_responses$imp,
      # New NFC responses
      user_responses$nfc_order,
      user_responses$nfc_predictability,
      user_responses$nfc_decisiveness, 
      user_responses$nfc_ambiguity,
      user_responses$nfc_closemindedness,
      user_responses$nfc_total,
      # New NCS response
      user_responses$ncs_total,
      # SVO angle
      user_responses$SVO_angle
    )
  )
  
  # Create separate lists for planning, bargaining, and implementation items
  planning_items <- user_responses %>%
    select(starts_with("Plan")) %>%
    pivot_longer(cols = everything(), names_to = "Item", values_to = "Score")
  
  bargaining_items <- user_responses %>%
    select(starts_with("Barg")) %>%
    pivot_longer(cols = everything(), names_to = "Item", values_to = "Score")
  
  implementation_items <- user_responses %>%
    select(starts_with("Post")) %>%
    pivot_longer(cols = everything(), names_to = "Item", values_to = "Score")
  
  # Create separate list for NFC items
  nfc_items <- user_responses %>%
    select(starts_with("NFC")) %>%
    pivot_longer(cols = everything(), names_to = "Item", values_to = "Score")
  
  # Create separate list for NCS items
  ncs_items <- user_responses %>%
    select(starts_with("NCS")) %>%
    pivot_longer(cols = everything(), names_to = "Item", values_to = "Score")
    
  # Create list for SVO data
  svo_data <- data.frame(
    SVO_angle = user_responses$SVO_angle,
    SVO_type = user_responses$SVO_type
  )
  
  # Map SVO type to a label for reporting
  svo_type_label <- case_when(
    user_responses$SVO_type == 1 ~ "Competitive",
    user_responses$SVO_type == 2 ~ "Individualistic",
    user_responses$SVO_type == 3 ~ "Prosocial",
    user_responses$SVO_type == 4 ~ "Altruistic",
    TRUE ~ "Not classified"
  )
  
  # Create data for SVO items display
  svo_items <- user_responses %>%
    select(starts_with("SVO")) %>%
    select(SVO1:SVO6, SVO1_Self:SVO6_Other) %>%
    pivot_longer(cols = starts_with("SVO"), names_to = "Item", values_to = "Value")
  
  # Merge with benchmarks
  df <- merge(summary_scores, benchmarks, by = "Category")
  
  # RETURN THE DATA (this was missing!)
  return(list(
    df = df,
    planning_items = planning_items,
    bargaining_items = bargaining_items,
    implementation_items = implementation_items,
    nfc_items = nfc_items,
    ncs_items = ncs_items,
    svo_type_label = svo_type_label,
    svo_items = svo_items
  ))
}
# Run the application 
shinyApp(ui = ui, server = server)