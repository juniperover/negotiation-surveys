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

# Use service account for authentication
setwd(this.path::here())
gs4_auth(path = "secrets/root-alignment-448702-q4-8384ab9efedf.json")

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
      }
    "))
  ),
  
  fluidRow(
    column(8, titlePanel("Negotiation Behavior Inventory")), # Full width on mobile
    column(4, tags$img(src = "mbslogo.jpg", height = "100px", style = "float:right;")) # Full width on mobile
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
      selectInput("gender", "Gender:", 
                  choices = c(" ", "Male", "Female", "Other"))
      ),
    
    mainPanel(
      h4("Reflecting on how you negotiated in the past, to what extent would you do the following behaviors before a negotiation?"),
      h4("Each of the behaviors below are things people can do to prepare for a negotiation."),
      
      # Survey items
      lapply(1:21, function(i) {
        tagList(
          h5(switch(i,
                    "1. Spend time thinking about your counterpart’s goals.",
                    "2. Spend time studying your counterpart’s tactics and patterns of behavior in similar situations.",
                    "3. Spend time studying the counterpart’s emotional state as they enter into negotiations.",
                    "4. Spend time researching if your counterpart might value things differently than you.",
                    "5. Spend time researching the counterpart’s point of view on the situation.",
                    "6. Exhaust every source of information at your disposal (including, but not limited to family, friends, co-workers, institutions, the library, and the Internet).",
                    "7. Research alternative outcomes that satisfy the need of the parties, (i.e., find alternative arrangements that meet you and your counterpart’s needs).",
                    "8. Communicate in advance your intention to negotiate.",
                    "9. Make sure all relevant parties will be included in the negotiation.",
                    "10. Establish a shared perception of the situation that requires resolution.",
                    "11. Attempt to remove or minimize distractions that could draw attention away from the negotiation.",
                    "12. Spend time researching the events leading to the negotiation.",
                    "13. Spend time thinking about your goals.",
                    "14. Prioritize the goals for the negotiation.",
                    "15. Create a table or list of what you value most to what you value least.",
                    "16. Ensure that you have the necessary resources to follow through with the deal that you reach.",
                    "17. Maintain competence in the skills needed to analyze the deal (e.g., technical evaluation, accounting, developing relationships, etc.).",
                    "18. Understand your no-deal options.",
                    "19. Understand your counterpart’s no-deal options.",
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
      
      h4("Reflecting on how you negotiated in the past, to what extent would you do the following behaviors during a negotiation?"),
      h4("Each of the behaviors are things people can do during a negotiation."),
      
      # During negotiation survey items
      lapply(1:26, function(i) {
        tagList(
          h5(switch(i,
                    "1. Have plans in advance to deal with counterproductive (e.g., negative, extreme, irrational) behavior from your counterpart.",
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
      
      # New section for after negotiation behaviors
      h4("Reflecting on how you negotiated in the past, to what extent would you do the following behaviors following a negotiation?"),
      h4("Each of the behaviors are things people can do after reaching an agreement."),
      
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
    
      # Add the new text here
      tags$p("Click the 'Submit' button below. It will save your responses and after a few seconds, you will get a download link to your individualized feedback report below."),
      
      div(class = "button-container",
          actionButton("submit", "Submit"),
          uiOutput("downloadButton")
      ),
      
      
      tags$div(id = "loading", style = "display: none;",
               tags$p("Processing your responses and generating report. Please wait...")
      ),
    )
  )
)

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
  })
  
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
      sapply(1:21, function(i) is.null(input[[paste0("plan", i)]])),
      sapply(1:26, function(i) is.null(input[[paste0("barg", i)]])),
      sapply(1:8, function(i) is.null(input[[paste0("post", i)]]))
    )
    
    if (any(unanswered) || !name_filled || !email_filled) {
      # If any question is unanswered or name/email is missing, show an alert
      shinyjs::alert("Please complete all required fields and answer all questions before submitting.")
      shinyjs::enable("submit")
      shinyjs::html("submit", "Submit")
      shinyjs::removeClass("submit", "disabled-button")
    } else {
      # All questions are answered and name/email are filled, proceed with submission
      user_responses <- data.frame(
        Timestamp = Sys.time(),
        Name = input$name,
        Email = input$email,
        Age = input$age,
        Gender = input$gender,
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
        Post8 = as.numeric(input$post8)
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
          imp = rowMeans(select(., Post1:Post3, Post5, Post6), na.rm = TRUE)
        )
      
      # Print the data frame for debugging
      # print(user_responses)
      
      shinyjs::show("loading")
      
      tryCatch({
        # Send to Google Sheets
        sheet_id <- "1RPA9xx6m9cOB_X22_96FTktGq7R3ujF0i5NFjWv3mR4"
        sheet_append(sheet_id, user_responses)
        
        # Generate report
        report(generate_report(user_responses))
        
        shinyjs::hide("loading")
        
        # Enable the download button
        output$downloadButton <- renderUI({
          downloadButton("downloadReport", "Download Your Report")
        })
        
        # Update the submit button text
        shinyjs::html("submit", "Report Generated")
        
      }, error = function(e) {
        output$thankYou <- renderText({
          paste("An error occurred:", e$message)
        })
        shinyjs::enable("submit")
        shinyjs::html("submit", "Submit")
        shinyjs::removeClass("submit", "disabled-button")
        shinyjs::alert(paste("An error occurred:", e$message))
      })
    }
  })
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0("report_", gsub("[^[:alnum:]]", "_", input$name), ".pdf")
    },
    content = function(file) {
      file.copy(report(), file)
    }
  )
}


# Helper function to generate report
generate_report <- function(user_responses) {
  # Define benchmarks
  benchmarks <- data.frame(
    Category = c("pre_count", "pre_arena", "pre_preps", "pre_imp", "pre",
                 "barg_mut", "barg_com", "barg_arg", "barg_get", "barg",
                 "imp_imp", "imp_feedb", "imp"),
    Benchmark = c(4.4, 4.61, 5.1, 4.37, 4.66, 
                  4.67, 5.28, 5.13, 3.62, 4.55,
                  5.26, 4.16, 4.82)
  )
  
  # Prepare data for the report
  summary_scores <- data.frame(
    Category = c("pre_count", "pre_arena", "pre_preps", "pre_imp", "pre",
                 "barg_mut", "barg_com", "barg_arg", "barg_get", "barg",
                 "imp_imp", "imp_feedb", "imp"),
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
      user_responses$imp
    )
  )
  
  # Merge with benchmarks
  df <- merge(summary_scores, benchmarks, by = "Category")
  
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
  
  # Render the Rmd template
  report_path <- tempfile(fileext = ".pdf")
  message("Report path: ", report_path)
  
  tryCatch({
    rmarkdown::render("report_template.Rmd", 
                      output_file = report_path, 
                      params = list(
                        name = user_responses$Name,
                        df = df,
                        planning_items = planning_items,
                        bargaining_items = bargaining_items,
                        implementation_items = implementation_items
                      ))
    message("Report generated successfully.")
  }, error = function(e) {
    message("Error generating report: ", e$message)
  })
  
  return(report_path)
}
# Run the application 
shinyApp(ui = ui, server = server)