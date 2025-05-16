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

# Use environment variable to authenticate
key_path <- Sys.getenv("google_sheets_key", "")
if (key_path == "") {
  stop("Google Sheets key environment variable not found")
}

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
    column(8, titlePanel("Negotiation Behavior Inventory")),
    column(4, tags$img(src = "mbslogo.jpg", height = "100px", style = "float:right;"))
  ),
  
  # Add the preamble
  wellPanel(
    HTML("<p>We ask you to reflect on your behaviors during negotiations in which you were personally involved. We define the term 'negotiation' in a broad sense. In addition to events commonly thought to involve negotiation, like negotiating a job offer or making a purchase, negotiation can involve attempts to obtain co-operation from others, settle a dispute, resolve legal issues, or convincing someone to follow your plan of action.</p>")
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
    h4("Reflecting on how you negotiated in the past, to what extent would you do the following behaviors before a negotiation?"),
    h4("Each of the behaviors below are things people can do to prepare for a negotiation."),
    
    # Survey items: NPI
    lapply(1:21, function(i) {
      # Your existing survey items code
    }),
    
    h4("Reflecting on how you negotiated in the past, to what extent would you do the following behaviors during a negotiation?"),
    h4("Each of the behaviors are things people can do during a negotiation."),
    
    # During negotiation survey items
    lapply(1:26, function(i) {
      # Your existing survey items code
    }),
    
    # New section for after negotiation behaviors
    h4("Reflecting on how you negotiated in the past, to what extent would you do the following behaviors following a negotiation?"),
    h4("Each of the behaviors are things people can do after reaching an agreement."),
    
    # After negotiation survey items
    lapply(1:8, function(i) {
      # Your existing survey items code
    }),
    
    # Need for Closure Scale
    h4("Need for Closure Scale"),
    h4("Please indicate the extent to which you agree or disagree with each of the following statements."),
    
    # Survey items for Need for Closure
    lapply(1:15, function(i) {
      # Your existing survey items code
    }),
    
    # Need for Cognition Scale
    h4("Need for Cognition Scale"),
    h4("Please indicate the extent to which each statement is characteristic of you."),
    
    # Survey items for Need for Cognition
    lapply(1:18, function(i) {
      # Your existing survey items code
    }),
    
    # SVO Scale
    h4("Social Value Orientation"),
    div(class = "svo-instructions",
        HTML("<p>For this section, imagine you have been randomly paired with another person, whom we will refer to as the other. 
              This other person is someone you do not know and will remain mutually anonymous. All of your choices are completely confidential.</p>
              <p>You will be making a series of decisions about allocating resources between you and this other person. 
              For each of the following questions, please indicate the distribution you prefer most by moving the slider to your preferred position.</p>
              <p>Assume that your decisions would yield money for both yourself and the other person (not really, for course purposes, but answer as if it would). There are no right or wrong answers; 
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
    tags$p("Click the 'Submit' button below. It will save your responses and after a few seconds, you will get a download link to your individualized feedback report below."),
    
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
      paste0("report_", gsub("[^[:alnum:]]", "_", input$name), ".pdf")
    },
    content = function(file) {
      file.copy(report(), file)
    }
  )
  
  observeEvent(input$submit, {
    # Disable the submit button and update its text
    shinyjs::disable("submit")
    shinyjs::html("submit", "Generating your report, please wait...")
    shinyjs::addClass("submit", "disabled-button")
    
    # Check if name and email are filled
    name_filled <- nzchar(input$name)
    email_filled <- nzchar(input$email)
    
    if (!name_filled) {
      shinyjs::html("name_error", "Please enter your name.")
    } else {
      shinyjs::html("name_error", "")
    }
    
    if (!email_filled || !is_valid_email(input$email)) {
      shinyjs::html("email_error", "Please enter a valid email address.")
    } else {
      shinyjs::html("email_error", "")
    }
    
    # Check if all questions are answered
    unanswered <- c(
      sapply(1:21, function(i) { is.null(input[[paste0("plan", i)]]) }),
      sapply(1:26, function(i) { is.null(input[[paste0("barg", i)]]) }),
      sapply(1:8, function(i) { is.null(input[[paste0("post", i)]]) }),
      sapply(1:15, function(i) { is.null(input[[paste0("nfc", i)]]) }),  # NFC questions
      sapply(1:18, function(i) { is.null(input[[paste0("ncs", i)]]) })   # NCS questions
      # SVO questions are handled by sliders with defaults, so they're always answered
    )
    
    if (any(unanswered) || !name_filled || !email_filled) {
      # If any question is unanswered or name/email is missing, show an alert
      shinyjs::alert("Please complete all required fields and answer all questions before submitting.")
      shinyjs::enable("submit")
      shinyjs::html("submit", "Submit")
      shinyjs::removeClass("submit", "disabled-button")
    } else {
      # All questions are answered and name/email are filled, proceed with submission
      
      # Get SVO values from hidden inputs
      svo_self_values <- c(
        as.numeric(input$svo1_self_value),
        as.numeric(input$svo2_self_value),
        as.numeric(input$svo3_self_value),
        as.numeric(input$svo4_self_value),
        as.numeric(input$svo5_self_value),
        as.numeric(input$svo6_self_value)
      )
      
      svo_other_values <- c(
        as.numeric(input$svo1_other_value),
        as.numeric(input$svo2_other_value),
        as.numeric(input$svo3_other_value),
        as.numeric(input$svo4_other_value),
        as.numeric(input$svo5_other_value),
        as.numeric(input$svo6_other_value)
      )
      
      # Get gender value (handle self-identified)
      gender_value <- input$gender
      if (gender_value == "Prefer to self-identify" && !is.null(input$gender_self) && nzchar(input$gender_self)) {
        gender_value <- paste("Self-identified:", input$gender_self)
      }
      
      # Create response data frame
      user_responses <- data.frame(
        Timestamp = Sys.time(),
        Name = input$name,
        Email = input$email,
        Age = input$age,
        Gender = gender_value,
        Plan1 = as.numeric(input$plan1),
        Plan2 = as.numeric(input$plan2),
        Plan3 = as.numeric(input$plan3),
        Plan4 = as.numeric(input$plan4),
        Plan5 = as.numeric(input$plan5),
        Plan6 = as.numeric(input$plan6),
        Plan7 = as.numeric(input$plan7),
        Plan8 = as.numeric(input$plan8),
        Plan9 = as.numeric(input$plan9),
        Plan10 = as.numeric(input$plan10),
        Plan11 = as.numeric(input$plan11),
        Plan12 = as.numeric(input$plan12),
        Plan13 = as.numeric(input$plan13),
        Plan14 = as.numeric(input$plan14),
        Plan15 = as.numeric(input$plan15),
        Plan16 = as.numeric(input$plan16),
        Plan17 = as.numeric(input$plan17),
        Plan18 = as.numeric(input$plan18),
        Plan19 = as.numeric(input$plan19),
        Plan20 = as.numeric(input$plan20),
        Plan21 = as.numeric(input$plan21),
        Barg1 = as.numeric(input$barg1),
        Barg2 = as.numeric(input$barg2),
        Barg3 = as.numeric(input$barg3),
        Barg4 = as.numeric(input$barg4),
        Barg5 = as.numeric(input$barg5),
        Barg6 = as.numeric(input$barg6),
        Barg7 = as.numeric(input$barg7),
        Barg8 = as.numeric(input$barg8),
        Barg9 = as.numeric(input$barg9),
        Barg10 = as.numeric(input$barg10),
        Barg11 = as.numeric(input$barg11),
        Barg12 = as.numeric(input$barg12),
        Barg13 = as.numeric(input$barg13),
        Barg14 = as.numeric(input$barg14),
        Barg15 = as.numeric(input$barg15),
        Barg16 = as.numeric(input$barg16),
        Barg17 = as.numeric(input$barg17),
        Barg18 = as.numeric(input$barg18),
        Barg19 = as.numeric(input$barg19),
        Barg20 = as.numeric(input$barg20),
        Barg21 = as.numeric(input$barg21),
        Barg22 = as.numeric(input$barg22),
        Barg23 = as.numeric(input$barg23),
        Barg24 = as.numeric(input$barg24),
        Barg25 = as.numeric(input$barg25),
        Barg26 = as.numeric(input$barg26),
        Post1 = as.numeric(input$post1),
        Post2 = as.numeric(input$post2),
        Post3 = as.numeric(input$post3),
        Post4 = as.numeric(input$post4),
        Post5 = as.numeric(input$post5),
        Post6 = as.numeric(input$post6),
        Post7 = as.numeric(input$post7),
        Post8 = as.numeric(input$post8),
        # Add Need for Closure fields
        NFC1 = as.numeric(input$nfc1),
        NFC2 = as.numeric(input$nfc2),
        NFC3 = as.numeric(input$nfc3),
        NFC4 = as.numeric(input$nfc4),
        NFC5 = as.numeric(input$nfc5),
        NFC6 = as.numeric(input$nfc6),
        NFC7 = as.numeric(input$nfc7),
        NFC8 = as.numeric(input$nfc8),
        NFC9 = as.numeric(input$nfc9),
        NFC10 = as.numeric(input$nfc10),
        NFC11 = as.numeric(input$nfc11),
        NFC12 = as.numeric(input$nfc12),
        NFC13 = as.numeric(input$nfc13),
        NFC14 = as.numeric(input$nfc14),
        NFC15 = as.numeric(input$nfc15),
        # Add Need for Cognition fields
        NCS1 = as.numeric(input$ncs1),
        NCS2 = as.numeric(input$ncs2),
        NCS3 = as.numeric(input$ncs3),
        NCS4 = as.numeric(input$ncs4),
        NCS5 = as.numeric(input$ncs5),
        NCS6 = as.numeric(input$ncs6),
        NCS7 = as.numeric(input$ncs7),
        NCS8 = as.numeric(input$ncs8),
        NCS9 = as.numeric(input$ncs9),
        NCS10 = as.numeric(input$ncs10),
        NCS11 = as.numeric(input$ncs11),
        NCS12 = as.numeric(input$ncs12),
        NCS13 = as.numeric(input$ncs13),
        NCS14 = as.numeric(input$ncs14),
        NCS15 = as.numeric(input$ncs15),
        NCS16 = as.numeric(input$ncs16),
        NCS17 = as.numeric(input$ncs17),
        NCS18 = as.numeric(input$ncs18),
        # Add SVO values
        SVO1 = as.numeric(input$svo1),
        SVO2 = as.numeric(input$svo2),
        SVO3 = as.numeric(input$svo3),
        SVO4 = as.numeric(input$svo4),
        SVO5 = as.numeric(input$svo5),
        SVO6 = as.numeric(input$svo6),
        # Store the actual self/other values as well
        SVO1_Self = svo_self_values[1],
        SVO1_Other = svo_other_values[1],
        SVO2_Self = svo_self_values[2],
        SVO2_Other = svo_other_values[2],
        SVO3_Self = svo_self_values[3],
        SVO3_Other = svo_other_values[3],
        SVO4_Self = svo_self_values[4],
        SVO4_Other = svo_other_values[4],
        SVO5_Self = svo_self_values[5],
        SVO5_Other = svo_other_values[5],
        SVO6_Self = svo_self_values[6],
        SVO6_Other = svo_other_values[6]
      )
      
      # Calculate summary scores
      user_responses <- user_responses %>%
        mutate(
          pre_count = rowMeans(select(., Plan1:Plan5), na.rm = TRUE),
          pre_arena = rowMeans(select(., Plan6:Plan12), na.rm = TRUE),
          pre_preps = rowMeans(select(., Plan13:Plan17), na.rm = TRUE),
          pre_imp = rowMeans(select(., Plan18:Plan19), na.rm = TRUE),
          pre = rowMeans(select(., Plan1:Plan19), na.rm = TRUE),
          barg_mut = rowMeans(select(., Barg1:Barg6), na.rm = TRUE),
          barg_com = rowMeans(select(., Barg7:Barg9), na.rm = TRUE),
          barg_arg = rowMeans(select(., Barg13, Barg12, Barg15, Barg18, Barg11, Barg17, Barg16, Barg14), na.rm = TRUE),
          barg_get = rowMeans(select(., Barg25, Barg21, Barg22, Barg19, Barg26, Barg23, Barg20, Barg24), na.rm = TRUE),
          barg = rowMeans(select(., Barg1:Barg9, Barg13, Barg12, Barg15, Barg18, Barg11, Barg17, Barg16, Barg14, Barg25, Barg21, Barg22, Barg19, Barg26, Barg23, Barg20, Barg24), na.rm = TRUE),
          imp_imp = rowMeans(select(., Post1:Post3), na.rm = TRUE),
          imp_feedb = rowMeans(select(., Post5, Post6), na.rm = TRUE),
          imp = rowMeans(select(., Post1:Post3, Post5, Post6), na.rm = TRUE),
          nfc_order = rowMeans(select(., NFC3, NFC12, NFC13), na.rm = TRUE),
          nfc_predictability = rowMeans(select(., NFC6, NFC10, NFC15), na.rm = TRUE),
          nfc_decisiveness = rowMeans(select(., NFC7, NFC8, NFC9), na.rm = TRUE),
          nfc_ambiguity = rowMeans(select(., NFC1, NFC2, NFC4, NFC11), na.rm = TRUE),
          nfc_closemindedness = rowMeans(select(., NFC5, NFC14), na.rm = TRUE),
          nfc_total = rowMeans(select(., NFC1:NFC15), na.rm = TRUE),
          # Reverse score the NCS negative items (3, 4, 5, 7, 8, 9, 12, 16, 17)
          NCS3_rev = 6 - NCS3,
          NCS4_rev = 6 - NCS4,
          NCS5_rev = 6 - NCS5,
          NCS7_rev = 6 - NCS7,
          NCS8_rev = 6 - NCS8,
          NCS9_rev = 6 - NCS9,
          NCS12_rev = 6 - NCS12,
          NCS16_rev = 6 - NCS16,
          NCS17_rev = 6 - NCS17,
          # Need for Cognition summary score (using reversed items where appropriate)
          ncs_total = rowMeans(select(., NCS1, NCS2, NCS3_rev, NCS4_rev, NCS5_rev, NCS6, 
                               NCS7_rev, NCS8_rev, NCS9_rev, NCS10, NCS11, NCS12_rev, 
                               NCS13, NCS14, NCS15, NCS16_rev, NCS17_rev, NCS18), na.rm = TRUE),
          
          # SVO calculations based on the scoring syntax
          # Calculate mean values for self and other across the first six items
          SVO_mean_first_six_Items_Self = rowMeans(select(., SVO1_Self, SVO2_Self, SVO3_Self, 
                                                 SVO4_Self, SVO5_Self, SVO6_Self), na.rm = TRUE),
          SVO_mean_first_six_Items_Other = rowMeans(select(., SVO1_Other, SVO2_Other, SVO3_Other, 
                                                   SVO4_Other, SVO5_Other, SVO6_Other), na.rm = TRUE),
          
          # Calculate SVO angle according to the formula
          SVO_angle = (atan((SVO_mean_first_six_Items_Other - 50) / 
                           (SVO_mean_first_six_Items_Self - 50)) * 180) / pi,
          
          # Categorize the SVO angle into types
          SVO_type = case_when(
            SVO_angle <= -12.04 ~ 1, # competitive
            SVO_angle > -12.04 & SVO_angle <= 22.45 ~ 2, # individualistic
            SVO_angle > 22.45 & SVO_angle <= 57.15 ~ 3, # prosocial
            SVO_angle > 57.15 ~ 4, # altruistic
            TRUE ~ NA_real_
          )
        )
      
      shinyjs::show("loading")
      
      tryCatch({
        # Send to Google Sheets 
        sheet_id <- "19cbvKPdMlg7vr9XBNZsVdkbsrQl0nETpatpcJW2_9nU"
        sheet_append(sheet_id, user_responses)
        
        # Generate report
        report(generate_report(user_responses))
        
        shinyjs::hide("loading")
        
        # Update the submit button text
        shinyjs::html("submit", "Report Generated")
        
      }, error = function(e) {
        # Show error alert
        shinyjs::alert(paste("An error occurred:", e$message))
        
        # Re-enable the submit button
        shinyjs::enable("submit")
        shinyjs::html("submit", "Submit")
        shinyjs::removeClass("submit", "disabled-button")
        shinyjs::hide("loading")
      })
    }
  })
}


# Helper function to generate report
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
  
  # Convert user_responses to a list if it's not already
  user_responses_list <- as.list(user_responses)
  
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
  
  # Render the Rmd template
  report_path <- tempfile(fileext = ".pdf")
  message("Report path: ", report_path)
  
  tryCatch({
    # Update the render call
    rmarkdown::render("report_template.Rmd", 
                    output_file = report_path, 
                    params = list(
                      name = user_responses$Name,
                      df = df,
                      planning_items = planning_items,
                      bargaining_items = bargaining_items,
                      implementation_items = implementation_items,
                      nfc_items = nfc_items,      # Add NFC items
                      ncs_items = ncs_items,      # Add NCS items
                      svo_angle = user_responses$SVO_angle,
                      svo_type = svo_type_label,  # Add SVO type label
                      svo_items = svo_items       # Add SVO items data
                    ))
    message("Report generated successfully.")
  }, error = function(e) {
    message("Error generating report: ", e$message)
  })
  
  return(report_path)
}

# Run the application 
shinyApp(ui = ui, server = server)