# SVO Slider items - FIXED version with correct structure
lapply(1:6, function(i) {
  tagList(
    h5(paste("SVO Item", i)),
    
    # Define the self/other values for each slider position
    tags$div(class = "svo-slider-container",
      # This div will display the values in a table format
      tags$div(id = paste0("svo_values_", i), class = "svo-values"),
      
      # Display slider in the middle
      tags$div(class = "svo-labels svo-top-label",
              tags$div(class = "svo-label", "You Receive")
      ),
      
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
})