library(dplyr)
library(shiny)

cancers <- readr::read_csv("data/cancer_class_list.csv") %>%
    mutate(
        label = case_when(
            stringr::str_detect(value, "^cancer") ~ "Unspecified/Multiple",
            stringr::str_detect(value, "^skin") ~ "Skin (Non-Melanoma)",
            stringr::str_detect(value, "^head") ~ "Head/Neck",
            stringr::str_detect(value, "^multiple") ~ "Multiple Myeloma",
            stringr::str_detect(value, "^cns") ~ "CNS",
            TRUE ~ stringr::str_to_title(value)
        )
    ) %>%
    arrange(label)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("GWAS Cancer SNP Explorer"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
       sliderInput(
           "effectsize",
           "Effect Size",
           min = 1,
           max = 10,
           value = 5
       ),

       checkboxGroupInput(
           "cancerclass",
           "Select Cancer Class",
           choiceValues = cancers$value,
           choiceNames =  cancers$label,
           # width = "500px"
       )
    ),

    # Show a plot of the generated distribution
    mainPanel(
       # plotOutput(
    )
  )
))
