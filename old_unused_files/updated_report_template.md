---
title: "Negotiation Behavior Inventory Report"
params:
  name: NA
  df: NA
  planning_items: NA
  bargaining_items: NA
  implementation_items: NA
  nfc_items: NA
  ncs_items: NA
output:
  pdf_document:
classoption: a4paper
header-includes:
    - \usepackage{fancyhdr}
    - \usepackage[utf8]{inputenc}
geometry: margin = 1in
---

\addtolength{\headheight}{.5cm} 
\pagestyle{fancyplain} 
\lhead{\includegraphics[height=1.7cm]{www/mbslogo.jpg}}
\renewcommand{\headrulewidth}{0pt} 
\let\oldrule=\rule
    \renewcommand{\rule}[1]{\oldrule{\linewidth}} 
\pagenumbering{gobble}

# `r params$name`

This report provides feedback about how you engage in effective negotiation behaviors. It should be a source of reflection on areas where you are performing well, and areas where you can improve. In each section of the report, you can see how you compare with your peers. *Note: "MBS Participants" refers to the average score of MBS participants.*


```{r setup, include=FALSE}
# Load necessary libraries
library(knitr)
library(rmarkdown)
library(tools)
library(openxlsx)
library(dplyr)
library(xtable)
library(grid)
library(ggplot2)
library(ggpubr)
library(stringr)
library(cowplot)
library(tidyr)
options(width = 120)
knitr::opts_chunk$set(comment = NA)

# Prep the datasets
plot_data <- params$df
planning_items <- params$planning_items
bargaining_items <- params$bargaining_items
implementation_items <- params$implementation_items
nfc_items <- params$nfc_items  # Add this line
ncs_items <- params$ncs_items  # Add this line
```

# Planning

The planning aspect of negotiation refers behaviors that should take place before the first offer is made. 

```{r, echo = F, fig.align='center', fig.width = 6.4, fig.height = 1, include = T, message = F, fig.margin = T, warning = F, results = 'asis'}
pre_df <- plot_data[9, ]

pre_long <- pre_df %>%
  pivot_longer(cols = c(Your_Response, Benchmark),
               names_to = "Measure",
               values_to = "Value")

title <- str_pad("Overall planning score", width = 40, side=c('right'), pad=' ')

q11 <- ggplot(pre_long, aes(y = Value, x = Measure, fill = factor(Measure))) +
  geom_bar(stat = "identity") + theme_bw() + ylab("") + xlab(title) +
  theme(legend.position = "none", axis.title.y = element_text(angle=0, hjust = 0, size = 9)) + 
  geom_text(data = pre_long, aes(label = round(Value, 2), y = 6.5)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), 
  labels=c("1" = "1", "2" = "", "3" = "3", "4" = "", "5" = "5", "6" = "", "7" = "7")) + 
  scale_x_discrete(labels = c("Your_Response" = "You", "Benchmark" = "MBS Participants")) + 
  coord_cartesian(ylim=c(1,8)) + scale_fill_manual(values = c("grey60", "Green")) 
q11 <- q11 + rotate()

q11
```

### Planning dimensions

```{r, echo = F, fig.align='center', fig.width = 6.4, fig.height = 1, include = T, message = F, fig.margin = T, warning = F, results = 'asis'}
title <- str_wrap("Efforts to initiate the negotiation and to understand the situation", width = 40)
title <- unlist(str_split(title, pattern = "\\n", n = Inf, simplify = FALSE), use.names = F)
title <- str_pad(title, width = 40, side = c("right"), pad = " ")
title <- paste(title[1], title[2], sep ="\n")

pre_df <- plot_data[10, ]

pre_long <- pre_df %>%
  pivot_longer(cols = c(Your_Response, Benchmark),
               names_to = "Measure",
               values_to = "Value")

q1 <- ggplot(pre_long, aes(y = Value, x = Measure, fill = factor(Measure))) +
  geom_bar(stat = "identity") + theme_bw() + ylab("") + xlab(title) +
  theme(legend.position = "none", axis.title.y = element_text(angle=0, hjust = 0, size = 9)) + 
  geom_text(data = pre_long, aes(label = round(Value, 2), y = 6.5)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), 
  labels=c("1" = "1", "2" = "", "3" = "3", "4" = "", "5" = "5", "6" = "", "7" = "7")) + 
  scale_x_discrete(labels = c("Your_Response" = "You", "Benchmark" = "MBS Participants")) + 
  coord_cartesian(ylim=c(1,8)) + scale_fill_manual(values = c("grey60", "Green")) 
q1 <- q1 + rotate()
```

```{r, echo = F, fig.align='center', fig.width = 6.4, fig.height = 1, include = T, message = F, fig.margin = T, warning = F, results = 'asis'}
title <- str_wrap("Understanding the counterpart's intentions and behaviors", width = 40)
title <- unlist(str_split(title, pattern = "\\n", n = Inf, simplify = FALSE), use.names = F)
title <- str_pad(title, width = 40, side = c("right"), pad = " ")
title <- paste(title[1], title[2], sep ="\n")

pre_df <- plot_data[11, ]

pre_long <- pre_df %>%
  pivot_longer(cols = c(Your_Response, Benchmark),
               names_to = "Measure",
               values_to = "Value")

q2 <- ggplot(pre_long, aes(y = Value, x = Measure, fill = factor(Measure))) +
  geom_bar(stat = "identity") + theme_bw() + ylab("") + xlab(title) +
  theme(legend.position = "none", axis.title.y = element_text(angle=0, hjust = 0, size = 9)) + 
  geom_text(data = pre_long, aes(label = round(Value, 2), y = 6.5)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), 
  labels=c("1" = "1", "2" = "", "3" = "3", "4" = "", "5" = "5", "6" = "", "7" = "7")) + 
  scale_x_discrete(labels = c("Your_Response" = "You", "Benchmark" = "MBS Participants")) + 
  coord_cartesian(ylim=c(1,8)) + scale_fill_manual(values = c("grey60", "Green")) 
q2 <- q2 + rotate()
```

```{r, echo = F, fig.align='center', fig.width = 6.4, fig.height = 1, include = T, message = F, fig.margin = T, warning = F, results = 'asis'}
title <- str_wrap("Efforts to understand your needs and interests for the negotiation", width = 40)
title <- unlist(str_split(title, pattern = "\\n", n = Inf, simplify = FALSE), use.names = F)
title <- str_pad(title, width = 40, side = c("right"), pad = " ")
title <- paste(title[1], title[2], sep ="\n")

pre_df <- plot_data[13, ]

pre_long <- pre_df %>%
  pivot_longer(cols = c(Your_Response, Benchmark),
               names_to = "Measure",
               values_to = "Value")

q3 <- ggplot(pre_long, aes(y = Value, x = Measure, fill = factor(Measure))) +
  geom_bar(stat = "identity") + theme_bw() + ylab("") + xlab(title) +
  theme(legend.position = "none", axis.title.y = element_text(angle=0, hjust = 0, size = 9)) + 
  geom_text(data = pre_long, aes(label = round(Value, 2), y = 6.5)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), 
  labels=c("1" = "1", "2" = "", "3" = "3", "4" = "", "5" = "5", "6" = "", "7" = "7")) + 
  scale_x_discrete(labels = c("Your_Response" = "You", "Benchmark" = "MBS Participants")) + 
  coord_cartesian(ylim=c(1,8)) + scale_fill_manual(values = c("grey60", "Green")) 
q3 <- q3 + rotate()
```

```{r, echo = F, fig.align='center', fig.width = 6.4, fig.height = 1, include = T, message = F, fig.margin = T, warning = F, results = 'asis'}
title <- str_wrap("Awareness of what will happen if the negotiation fails", width = 40)
title <- unlist(str_split(title, pattern = "\\n", n = Inf, simplify = FALSE), use.names = F)
title <- str_pad(title, width = 40, side = c("right"), pad = " ")
title <- paste(title[1], title[2], sep ="\n")

pre_df <- plot_data[12, ]

pre_long <- pre_df %>%
  pivot_longer(cols = c(Your_Response, Benchmark),
               names_to = "Measure",
               values_to = "Value")

q4 <- ggplot(pre_long, aes(y = Value, x = Measure, fill = factor(Measure))) +
  geom_bar(stat = "identity") + theme_bw() + ylab("") + xlab(title) +
  theme(legend.position = "none", axis.title.y = element_text(angle=0, hjust = 0, size = 9)) + 
  geom_text(data = pre_long, aes(label = round(Value, 2), y = 6.5)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), 
  labels=c("1" = "1", "2" = "", "3" = "3", "4" = "", "5" = "5", "6" = "", "7" = "7")) + 
  scale_x_discrete(labels = c("Your_Response" = "You", "Benchmark" = "MBS Participants")) + 
  coord_cartesian(ylim=c(1,8)) + scale_fill_manual(values = c("grey60", "Green")) 
q4 <- q4 + rotate()

pl <- align_plots(q1, q2, q3, q4, align="v")

ggdraw(pl[[1]])
ggdraw(pl[[2]])
ggdraw(pl[[3]])
ggdraw(pl[[4]])
```

\newpage

# Need for Closure Assessment

This section shows your responses to the Need for Closure scale, which measures your preference for order, structure, and closure in your thinking and decision-making.

## Your Need for Closure Profile

```{r nfc_chart, echo=FALSE, fig.width=8, fig.height=5, fig.align='center'}
# Create a dataframe for NFC categories
nfc_df <- plot_data %>% 
  filter(Category %in% c("nfc_order", "nfc_predictability", "nfc_decisiveness", 
                         "nfc_ambiguity", "nfc_closemindedness", "nfc_total"))

# Skip this chunk if no NFC data is available
if(nrow(nfc_df) > 0) {
  # Rename categories for better readability
  nfc_df$Category_Label <- factor(nfc_df$Category, 
                                 levels = c("nfc_order", "nfc_predictability", "nfc_decisiveness", 
                                            "nfc_ambiguity", "nfc_closemindedness", "nfc_total"),
                                 labels = c("Preference for Order", "Preference for Predictability", 
                                            "Decisiveness", "Discomfort with Ambiguity", 
                                            "Close-mindedness", "Overall Need for Closure"))
  
  # Create the chart
  ggplot(nfc_df, aes(x = Category_Label)) +
    geom_bar(aes(y = Your_Response, fill = "Your Score"), stat = "identity", position = "dodge", width = 0.4) +
    geom_point(aes(y = Benchmark, color = "Benchmark"), size = 3) +
    geom_line(aes(y = Benchmark, group = 1, color = "Benchmark"), linetype = "dashed", size = 1) +
    scale_fill_manual(values = c("Your Score" = "#4285F4")) +
    scale_color_manual(values = c("Benchmark" = "#EA4335")) +
    labs(title = "Your Need for Closure Profile",
         y = "Score (1-6 scale)",
         x = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.title = element_blank(),
          legend.position = "bottom")
}
```

## Interpretation

```{r nfc_interpret, echo=FALSE, results='asis'}
# Skip this chunk if no NFC data is available
if(exists("nfc_df") && nrow(nfc_df) > 0) {
  cat("- **Preference for Order** (Score: ", round(nfc_df$Your_Response[nfc_df$Category == "nfc_order"], 2), "): Your preference for order, structure, and routine in your life.\n\n")
  
  cat("- **Preference for Predictability** (Score: ", round(nfc_df$Your_Response[nfc_df$Category == "nfc_predictability"], 2), "): Your comfort with predictable situations and discomfort with uncertainty.\n\n")
  
  cat("- **Decisiveness** (Score: ", round(nfc_df$Your_Response[nfc_df$Category == "nfc_decisiveness"], 2), "): Your tendency to make quick decisions to achieve closure.\n\n")
  
  cat("- **Discomfort with Ambiguity** (Score: ", round(nfc_df$Your_Response[nfc_df$Category == "nfc_ambiguity"], 2), "): Your level of discomfort with ambiguous or uncertain situations.\n\n")
  
  cat("- **Close-mindedness** (Score: ", round(nfc_df$Your_Response[nfc_df$Category == "nfc_closemindedness"], 2), "): Your openness to considering different opinions or perspectives.\n\n")
  
  cat("- **Overall Need for Closure** (Score: ", round(nfc_df$Your_Response[nfc_df$Category == "nfc_total"], 2), "): Your general tendency to seek definitive answers and avoid ambiguity.\n\n")
}
```

### How This Relates to Negotiation

Your Need for Closure can influence your negotiation style in several ways:

- Higher scores may indicate a preference for reaching agreements quickly, potentially at the expense of exploring all options.
- Lower scores may indicate comfort with ambiguity and willingness to keep options open longer during negotiations.
- Understanding your need for closure can help you recognize when you might be rushing to resolution versus when you might benefit from exploring more options.

\newpage

# Need for Cognition Assessment

This section shows your responses to the Need for Cognition scale, which measures your tendency to engage in and enjoy thinking.

## Your Need for Cognition Score

```{r ncs_chart, echo=FALSE, fig.width=6, fig.height=4, fig.align='center'}
# Create a dataframe for NCS category
ncs_df <- plot_data %>% 
  filter(Category %in% c("ncs_total"))

# Skip this chunk if no NCS data is available
if(nrow(ncs_df) > 0) {
  # Rename category for better readability
  ncs_df$Category_Label <- "Need for Cognition"
  
  # Create the chart
  ggplot(ncs_df, aes(x = Category_Label)) +
    geom_bar(aes(y = Your_Response, fill = "Your Score"), stat = "identity", width = 0.5) +
    geom_point(aes(y = Benchmark, color = "Average Score"), size = 3) +
    scale_fill_manual(values = c("Your Score" = "#4285F4")) +
    scale_color_manual(values = c("Average Score" = "#EA4335")) +
    ylim(0, 5) +
    labs(title = "Your Need for Cognition Score",
         y = "Score (1-5 scale)",
         x = "") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          legend.position = "bottom")
}
```

## Interpretation

```{r ncs_interpret, echo=FALSE, results='asis'}
# Skip this chunk if no NCS data is available
if(exists("ncs_df") && nrow(ncs_df) > 0) {
  cat("Your Need for Cognition score (", round(ncs_df$Your_Response[1], 2), ") reflects your tendency to engage in and enjoy thinking.\n\n")
  
  cat("- **Higher scores** (above 3.5) suggest that you tend to enjoy cognitive challenges and complex problem-solving. You likely prefer to think deeply about issues and are motivated by intellectual engagement.\n\n")
  
  cat("- **Lower scores** (below 3.5) suggest that you may prefer simpler cognitive tasks and might be less inclined to engage in effortful thinking when it's not necessary.\n\n")
}
```

### How This Relates to Negotiation

Your Need for Cognition can influence your negotiation approach in several ways:

- A higher Need for Cognition may lead you to analyze negotiation situations more thoroughly, consider more alternatives, and develop more complex negotiation strategies.

- You might enjoy the intellectual challenge of negotiation, including analyzing the other party's interests, crafting persuasive arguments, and finding creative solutions that satisfy both parties.

- Understanding your Need for Cognition can help you recognize when you might benefit from more or less cognitive engagement in negotiations depending on the situation.

### Planning behaviors for reflection

```{r, tidy = T, echo = F}
dypre <- planning_items

dypre <- subset(dypre, Item != "Plan20" & Item != "Plan21")

dypre$q <- c("Spend time thinking about your counterpart's goals.",
             "Spend time studying your counterpart's tactics and patterns of behavior in similar situations.",
             "Spend time studying the counterpart's emotional state as they enter into negotiations.",
             "Spend time researching if your counterpart might value things differently than you.",
             "Spend time researching the counterpart's point of view on the situation.",
             "Exhaust every source of information at your disposal (including, but not limited to family, friends, co-workers, institutions, the library, and the Internet).",
             "Research alternative outcomes that satisfy the need of the parties - (i.e., find alternative arrangements that meet you and your counterpart's needs).",
             "Communicate in advance your intention to negotiate.",
             "Make sure all relevant parties will be included in the negotiation.",
             "Establish a shared perception of the situation that requires resolution.",
             "Attempt to remove or minimize distractions that could draw attention away from the negotiation.",
             "Spend time researching the events leading to the negotiation.",
             "Spend time thinking about your goals.",
             "Prioritize the goals for the negotiation.",
             "Create a table or list of what you value most to what you value least.",
             "Ensure that you have the necessary resources to follow through with the deal that you reach.",
             "Maintain competence in the skills needed to analyze the deal (e.g., technical evaluation, accounting, developing relationships, etc.).",
             "Understand your no-deal options.",
             "Understand your counterpart's no-deal options.")

dypre$c_value <- c(5.02, 4.02, 4.19, 4.08, 4.59,
                   4.22, 4.9, 4.59, 5.03, 5,
                   4.25, 4.34, 6.03, 5.5, 4.1,
                   5, 4.9, 4.86, 3.94)

dypre$Dimension <- c("Understanding the counterpart",
             "Understanding the counterpart",
             "Understanding the counterpart",
             "Understanding the counterpart",
             "Understanding the counterpart",
             "Setting the arena",
             "Setting the arena",
             "Setting the arena",
             "Setting the arena",
             "Setting the arena",
             "Setting the arena",
             "Setting the arena",
             "Preparing the self",
             "Preparing the self",
             "Preparing the self",
             "Preparing the self",
             "Preparing the self",
             "Understanding the impasse",
             "Understanding the impasse")

dypre <- dypre[, c(5, 3, 2, 4)]

colnames(dypre) <- c("Dimension", "Behavior", "You", "MBS Participants")

dypre <- dypre[order(dypre$You, decreasing = TRUE), ]

```


```{r, results = 'asis', echo = F}
writeLines("\\begin{flushleft}")
cat("For further reflection, below are the three questions you rated highest:")
writeLines("\\end{flushleft}")
```
```{r, echo = F}
kable(dypre[1:3, ], row.names	= F)
```
```{r, results = 'asis', echo = F}
writeLines("\\begin{flushleft}")
cat("For further reflection, below are the three questions you rated lowest:")
writeLines("\\end{flushleft}")
```
```{r, echo = F}
kable(dypre[17:19, ],  row.names	= F)
```

\newpage

# Bargaining

The bargaining aspect of negotiation refers to the process of making offers and proposals. 

```{r, echo = F, fig.align='center', fig.width = 6.4, fig.height = 1, include = T, message = F, fig.margin = T, warning = F, results = 'asis'}
title <- str_pad("Overall bargaining score", width = 40, side=c('right'), pad=' ')

barg_df <- plot_data[1, ]

barg_long <- barg_df %>%
  pivot_longer(cols = c(Your_Response, Benchmark),
               names_to = "Measure",
               values_to = "Value")

q5 <- ggplot(barg_long, aes(y = Value, x = Measure, fill = factor(Measure))) +
  geom_bar(stat = "identity") + theme_bw() + ylab("") + xlab(title) +
  theme(legend.position = "none", axis.title.y = element_text(angle=0, hjust = 0, size = 9)) + 
  geom_text(data = barg_long, aes(label = round(Value, 2), y = 6.5)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), 
  labels=c("1" = "1", "2" = "", "3" = "3", "4" = "", "5" = "5", "6" = "", "7" = "7")) + 
  scale_x_discrete(labels = c("Your_Response" = "You", "Benchmark" = "MBS Participants")) + 
  coord_cartesian(ylim=c(1,8)) + scale_fill_manual(values = c("grey60", "Green")) 
q5 <- q5 + rotate()

q5
```

## Bargaining dimensions

```{r, echo = F, fig.align='center', fig.width = 6.4, fig.height = 1, include = T, message = F, fig.margin = T, warning = F, results = 'asis'}
title <- str_wrap("Constructing and communicating persuasive arguments", width = 40)
title <- unlist(str_split(title, pattern = "\\n", n = Inf, simplify = FALSE), use.names = F)
title <- str_pad(title, width = 40, side = c("right"), pad = " ")
title <- paste(title[1], title[2], sep ="\n")

barg_df <- plot_data[2, ]

barg_long <- barg_df %>%
  pivot_longer(cols = c(Your_Response, Benchmark),
               names_to = "Measure",
               values_to = "Value")

q6 <- ggplot(barg_long, aes(y = Value, x = Measure, fill = factor(Measure))) +
  geom_bar(stat = "identity") + theme_bw() + ylab("") + xlab(title) +
  theme(legend.position = "none", axis.title.y = element_text(angle=0, hjust = 0, size = 9)) + 
  geom_text(data = barg_long, aes(label = round(Value, 2), y = 6.5)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), 
  labels=c("1" = "1", "2" = "", "3" = "3", "4" = "", "5" = "5", "6" = "", "7" = "7")) + 
  scale_x_discrete(labels = c("Your_Response" = "You", "Benchmark" = "MBS Participants")) + 
  coord_cartesian(ylim=c(1,8)) + scale_fill_manual(values = c("grey60", "Green")) 
q6 <- q6 + rotate()
```

```{r, echo = F, fig.align='center', fig.width = 6.4, fig.height = 1, include = T, message = F, fig.margin = T, warning = F, results = 'asis'}
title <- str_wrap("Making a deal that includes all of your interests", width = 40)
title <- unlist(str_split(title, pattern = "\\n", n = Inf, simplify = FALSE), use.names = F)
title <- str_pad(title, width = 40, side = c("right"), pad = " ")
title <- paste(title[1], title[2], sep ="\n")

barg_df <- plot_data[3, ]

barg_long <- barg_df %>%
  pivot_longer(cols = c(Your_Response, Benchmark),
               names_to = "Measure",
               values_to = "Value")

q7 <- ggplot(barg_long, aes(y = Value, x = Measure, fill = factor(Measure))) +
  geom_bar(stat = "identity") + theme_bw() + ylab("") + xlab(title) +
  theme(legend.position = "none", axis.title.y = element_text(angle=0, hjust = 0, size = 9)) + 
  geom_text(data = barg_long, aes(label = round(Value, 2), y = 6.5)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), 
  labels=c("1" = "1", "2" = "", "3" = "3", "4" = "", "5" = "5", "6" = "", "7" = "7")) + 
  scale_x_discrete(labels = c("Your_Response" = "You", "Benchmark" = "MBS Participants")) + 
  coord_cartesian(ylim=c(1,8)) + scale_fill_manual(values = c("grey60", "Green")) 
q7 <- q7 + rotate()
```

```{r, echo = F, fig.align='center', fig.width = 6.4, fig.height = 1, include = T, message = F, fig.margin = T, warning = F, results = 'asis'}
title <- str_wrap("Efforts to claim as much value as possible", width = 40)
title <- unlist(str_split(title, pattern = "\\n", n = Inf, simplify = FALSE), use.names = F)
title <- str_pad(title, width = 40, side = c("right"), pad = " ")
title <- paste(title[1], title[2], sep ="\n")

barg_df <- plot_data[4, ]

barg_long <- barg_df %>%
  pivot_longer(cols = c(Your_Response, Benchmark),
               names_to = "Measure",
               values_to = "Value")

q8 <- ggplot(barg_long, aes(y = Value, x = Measure, fill = factor(Measure))) +
  geom_bar(stat = "identity") + theme_bw() + ylab("") + xlab(title) +
  theme(legend.position = "none", axis.title.y = element_text(angle=0, hjust = 0, size = 9)) + 
  geom_text(data = barg_long, aes(label = round(Value, 2), y = 6.5)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), 
  labels=c("1" = "1", "2" = "", "3" = "3", "4" = "", "5" = "5", "6" = "", "7" = "7")) + 
  scale_x_discrete(labels = c("Your_Response" = "You", "Benchmark" = "MBS Participants")) + 
  coord_cartesian(ylim=c(1,8)) + scale_fill_manual(values = c("grey60", "Green")) 
q8 <- q8 + rotate()
```

```{r, echo = F, fig.align='center', fig.width = 6.4, fig.height = 1, include = T, message = F, fig.margin = T, warning = F, results = 'asis'}
title <- str_wrap("Efforts to make both sides better off", width = 40)
title <- unlist(str_split(title, pattern = "\\n", n = Inf, simplify = FALSE), use.names = F)
title <- str_pad(title, width = 40, side = c("right"), pad = " ")
# title <- paste(title[1], title[2], sep ="\n")

barg_df <- plot_data[5, ]

barg_long <- barg_df %>%
  pivot_longer(cols = c(Your_Response, Benchmark),
               names_to = "Measure",
               values_to = "Value")

q9 <- ggplot(barg_long, aes(y = Value, x = Measure, fill = factor(Measure))) +
  geom_bar(stat = "identity") + theme_bw() + ylab("") + xlab(title) +
  theme(legend.position = "none", axis.title.y = element_text(angle=0, hjust = 0, size = 9)) + 
  geom_text(data = barg_long, aes(label = round(Value, 2), y = 6.5)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), 
  labels=c("1" = "1", "2" = "", "3" = "3", "4" = "", "5" = "5", "6" = "", "7" = "7")) + 
  scale_x_discrete(labels = c("Your_Response" = "You", "Benchmark" = "MBS Participants")) + 
  coord_cartesian(ylim=c(1,8)) + scale_fill_manual(values = c("grey60", "Green")) 
q9 <- q9 + rotate()

bl <- align_plots(q6, q7, q8, q9, align="v")

ggdraw(bl[[1]])
ggdraw(bl[[2]])
ggdraw(bl[[3]])
ggdraw(bl[[4]])
```

\newpage

## Bargaining behaviors for reflection

```{r, tidy = T, echo = F}
dybar <- bargaining_items

dybar <- subset(dybar, Item != "Barg10")

dybar <- dybar[1:25, ]
 
dybar$q <- c("Have plans in advance to deal with counterproductive (e.g., negative, extreme, irrational) behavior from your counterpart.",
"Ask questions to learn about your counterpart.",
"Test your understanding of what your counterpart was trying to say by repeating what they said.",
"Try to focus on satisfying your underlying needs rather than a specific list of requirements.",
"Spend time inventing options for mutual benefit.",
"Keep track of how much value is created by systematically evaluating each offer against previous offers.",
"Try to reach a deal that touches on all of the issues involved.",
"Review the final terms with your counterpart to confirm mutual agreement about their interpretation.",
"Make sure that you were fully understood.",
"Check if your arguments are persuasive.",
"Check if your counterpart is telling the truth.",
"Check if the counterpart is committed to making a deal with you.",
"Make sure the help you are getting is appropriate for the task at hand.",
"Justify each offer with a reason.",
"Provide reasons that the counterpart should find plausible.",
"Present facts that support your offer.",
"Support your offer with facts.",
"Refuse to make concessions.",
"Insist on getting something every time you make a concession.",
"Ask for a lot in your offers.",
"Avoid making concessions.",
"Use your power to make the counterpart concede",
"Always try to get something in return for an accommodation you make",
"Make ambitious offers.",
"Downplay threats made by the counterpart")

dybar$c_value <- c(4.21, 5.18, 4.74, 4.55, 5.21,
                   4.15, 5.05, 5.35, 5.51, 4.71,
                   4.83, 4.71, 4.87, 5.29, 5.59,
                   5.65, 5.57, 3, 3.4, 3.68,
                   3.29, 3.67, 3.98, 4.05, 3.98)

dybar$Dimension <- c("Working for mutual benefit",
"Working for mutual benefit",
"Working for mutual benefit",
"Working for mutual benefit",
"Working for mutual benefit",
"Working for mutual benefit",
"Comprehensive",
"Comprehensive",
"Comprehensive",
"Argumentation",
"Argumentation",
"Argumentation",
"Argumentation",
"Argumentation",
"Argumentation",
"Argumentation",
"Argumentation",
"Getting the most",
"Getting the most",
"Getting the most",
"Getting the most",
"Getting the most",
"Getting the most",
"Getting the most",
"Getting the most")

dybar <- dybar[, c(5, 3, 2, 4)]

colnames(dybar) <- c("Dimension", "Behavior", "You", "MBS Participants")

dybar <- dybar[order(dybar$You, decreasing = TRUE), ]
```

```{r, results = 'asis', echo = F}
writeLines("\\begin{flushleft}")
cat("For further reflection, below are the three questions you rated highest:")
writeLines("\\end{flushleft}")
```
```{r, echo = F}
kable(dybar[1:3, ], row.names	= F)
```
```{r, results = 'asis', echo = F}
writeLines("\\begin{flushleft}")
cat("For further reflection, below are the three questions you rated lowest:")
writeLines("\\end{flushleft}")
```
```{r, echo = F}
kable(dybar[23:25, ],  row.names	= F)
```

\newpage

# Implementation

The implementation aspect of negotiation refer to actions that should be taken after reaching an agreement. 

```{r, echo = F, fig.align='center', fig.width = 6.4, fig.height = 1, include = T, message = F, fig.margin = T, warning = F, results = 'asis'}
title <- str_pad("Overall implementation score", width = 40, side=c('right'), pad=' ')

imp_df <- plot_data[6, ]

imp_long <- imp_df %>%
  pivot_longer(cols = c(Your_Response, Benchmark),
               names_to = "Measure",
               values_to = "Value")

q10 <- q10 + rotate()

q10
```

## Implementation dimensions

```{r, echo = F, fig.align='center', fig.width = 6.4, fig.height = 1, include = T, message = F, fig.margin = T, warning = F, results = 'asis'}
title <- str_wrap("Efforts that help you and your counterpart to follow through on promises", width = 40)
title <- unlist(str_split(title, pattern = "\\n", n = Inf, simplify = FALSE), use.names = F)
title <- str_pad(title, width = 40, side = c("right"), pad = " ")
title <- paste(title[1], title[2], sep ="\n")

imp_df <- plot_data[8, ]

imp_long <- imp_df %>%
  pivot_longer(cols = c(Your_Response, Benchmark),
               names_to = "Measure",
               values_to = "Value")

q11 <- ggplot(imp_long, aes(y = Value, x = Measure, fill = factor(Measure))) +
  geom_bar(stat = "identity") + theme_bw() + ylab("") + xlab(title) +
  theme(legend.position = "none", axis.title.y = element_text(angle=0, hjust = 0, size = 9)) + 
  geom_text(data = imp_long, aes(label = round(Value, 2), y = 6.5)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), 
  labels=c("1" = "1", "2" = "", "3" = "3", "4" = "", "5" = "5", "6" = "", "7" = "7")) + 
  scale_x_discrete(labels = c("Your_Response" = "You", "Benchmark" = "MBS Participants")) + 
  coord_cartesian(ylim=c(1,8)) + scale_fill_manual(values = c("grey60", "Green")) 
q11 <- q11 + rotate()

```

```{r, echo = F, fig.align='center', fig.width = 6.4, fig.height = 1, include = T, message = F, fig.margin = T, warning = F, results = 'asis'}
title <- str_wrap("Consulting more experienced negotiators to improve for the future", width = 40)
title <- unlist(str_split(title, pattern = "\\n", n = Inf, simplify = FALSE), use.names = F)
title <- str_pad(title, width = 40, side = c("right"), pad = " ")
title <- paste(title[1], title[2], sep ="\n")

imp_df <- plot_data[7, ]

imp_long <- imp_df %>%
  pivot_longer(cols = c(Your_Response, Benchmark),
               names_to = "Measure",
               values_to = "Value")

q12 <- ggplot(imp_long, aes(y = Value, x = Measure, fill = factor(Measure))) +
  geom_bar(stat = "identity") + theme_bw() + ylab("") + xlab(title) +
  theme(legend.position = "none", axis.title.y = element_text(angle=0, hjust = 0, size = 9)) + 
  geom_text(data = imp_long, aes(label = round(Value, 2), y = 6.5)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), 
  labels=c("1" = "1", "2" = "", "3" = "3", "4" = "", "5" = "5", "6" = "", "7" = "7")) + 
  scale_x_discrete(labels = c("Your_Response" = "You", "Benchmark" = "MBS Participants")) + 
  coord_cartesian(ylim=c(1,8)) + scale_fill_manual(values = c("grey60", "Green")) 
q12 <- q12 + rotate()

il <- align_plots(q11, q12, align="v")

ggdraw(il[[1]])
ggdraw(il[[2]])
```

## Items for reflection

```{r, tidy = T, echo = F, results = 'asis'}
dyimp <- implementation_items

imp_subset <- c("Post1", "Post2", "Post3", "Post5", "Post6")

dyimp <- subset(dyimp, Item %in% imp_subset)

dyimp$q <- c("Express your commitment to the agreement.", 
"Check that you have addressed your counterpart's key concerns.",
"Agree on a plan to implement every aspect of the agreement.",
"Periodically seek advice about negotiating from a more experienced negotiator.",
"Periodically seek advice about negotiating from peers.")

dyimp$Dimension <- c("Implementation",
"Implementation",
"Implementation",
"Seeking feedback",
"Seeking feedback")

dyimp$c_value <- c(5.48, 4.97, 5.25, 4.1, 4.22)

dyimp <- dyimp[, c(4, 3, 2, 5)]

colnames(dyimp) <- c("Dimension", "Behavior", "You", "MBS Participants")

dyimp <- dyimp[order(dyimp$You, decreasing = TRUE), ]
```


```{r, results = 'asis', echo = F}
writeLines("\\begin{flushleft}")
cat("For further reflection, below are the two questions you rated highest:")
writeLines("\\end{flushleft}")
```
```{r, echo = F}
kable(dyimp[1:2, ], row.names = F)
```
```{r, results = 'asis', echo = F}
writeLines("\\begin{flushleft}")
cat("For further reflection, below are the two questions you rated lowest:")
writeLines("\\end{flushleft}")
```
```{r, echo = F}
kable(dyimp[4:5, ], row.names = F)
```

\newpage <- ggplot(imp_long, aes(y = Value, x = Measure, fill = factor(Measure))) +
  geom_bar(stat = "identity") + theme_bw() + ylab("") + xlab(title) +
  theme(legend.position = "none", axis.title.y = element_text(angle=0, hjust = 0, size = 9)) + 
  geom_text(data = imp_long, aes(label = round(Value, 2), y = 6.5)) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), 
  labels=c("1" = "1", "2" = "", "3" = "3", "4" = "", "5" = "5", "6" = "", "7" = "7")) + 
  scale_x_discrete(labels = c("Your_Response" = "You", "Benchmark" = "MBS Participants")) + 
  coord_cartesian(ylim=c(1,8)) + scale_fill_manual(values = c("grey60", "Green")) 
q10