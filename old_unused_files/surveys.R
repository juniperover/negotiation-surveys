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
      
      # Survey items: NPI
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
    
    # Need for Closure Scale
h4("Need for Closure Scale"),
h4("Please indicate the extent to which you agree or disagree with each of the following statements."),

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
      sapply(1:8, function(i) is.null(input[[paste0("post", i)]])),
      sapply(1:15, function(i) is.null(input[[paste0("nfc", i)]])),  # NFC questions
      sapply(1:18, function(i) is.null(input[[paste0("ncs", i)]]))   # NCS questions
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
        NCS18 = as.numeric(input$ncs18)
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
                               NCS13, NCS14, NCS15, NCS16_rev, NCS17_rev, NCS18), na.rm = TRUE)
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
                 "imp_imp", "imp_feedb", "imp",
                # New NFC categories
                 "nfc_order", "nfc_predictability", "nfc_decisiveness", 
                 "nfc_ambiguity", "nfc_closemindedness", "nfc_total",
                # New NCS category
                "ncs_total"),
    Benchmark = c(4.4, 4.61, 5.1, 4.37, 4.66, 
                  4.67, 5.28, 5.13, 3.62, 4.55,
                  5.26, 4.16, 4.82,
                  # New NFC benchmarks (replace with actual benchmarks if available)
                  4.2, 3.9, 4.1, 4.3, 3.8, 4.05,
                  # NCS benchmark (replace with actual benchmark if available)
                  3.5)
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
                 "ncs_total"),
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
      user_responses$ncs_total
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
  
  # Create separate list for NFC items
  nfc_items <- user_responses %>%
    select(starts_with("NFC")) %>%
    pivot_longer(cols = everything(), names_to = "Item", values_to = "Score")
  
  # Create separate list for NCS items
  ncs_items <- user_responses %>%
    select(starts_with("NCS")) %>%
    pivot_longer(cols = everything(), names_to = "Item", values_to = "Score")
  
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
                    nfc_items = nfc_items,  # Add NFC items
                    ncs_items = ncs_items   # Add NCS items
                  ))
    message("Report generated successfully.")
  }, error = function(e) {
    message("Error generating report: ", e$message)
  })
  
  return(report_path)
}
# Run the application 
shinyApp(ui = ui, server = server)
