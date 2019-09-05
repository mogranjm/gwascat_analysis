library(dplyr)
library(shiny)

cancers <- readr::read_csv("data/cancer_class_list.csv")
gwas_cancers <- readr::read_csv("data/classified_gwas_catalog.csv") %>%
    select(PVALUE_MLOG, OR.or.BETA)

shinyUI(fluidPage(

  # Application title
  titlePanel("GWAS Cancer SNP Explorer"),

  #Sidebar
  sidebarLayout(
    sidebarPanel(

    # Slider input for effect size threshold
       sliderInput(
           "sig",
           "Significance (-log P-Value)",
           min = floor(min(gwas_cancers$PVALUE_MLOG)),
           max = ceiling(max(gwas_cancers$PVALUE_MLOG)),
           value = c(-log(1e-08), -log(1e-08)),
           step = 0.01
       ),

       textOutput("equiv_lower_p"),
       textOutput("equiv_upper_p"),

       sliderInput(
           "effectsize",
           "Effect Size",
           # 1, 2,
           min = floor(min(gwas_cancers$OR.or.BETA, na.rm = T)),
           max = ceiling(max(gwas_cancers$OR.or.BETA, na.rm = T)),
           value = c(1, 1.2),
           step = 0.1
           # dragRange = 0.2
       ),

    # Checkbox input for cancer type selection
       checkboxGroupInput(
           "cancerclass",
           "Select Cancer Class",
           choiceValues = cancers$value,
           choiceNames =  cancers$label
           # width = "500px"
       )
    ),

    # Show a genome plot for the locations of SNPs matching the search criteria
    mainPanel(
       plotOutput("genomePlot")

    )
  )
))
