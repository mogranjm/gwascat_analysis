#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)

chrom_levels <- c(seq(1:22), "X", "Y")

chromosomes <- readr::read_csv("data/chromosome_lengths.csv") %>%
    mutate(SEQNAME = factor(SEQNAME, levels = chrom_levels))

gwas_cancers <- readr::read_csv("data/classified_gwas_catalog.csv")

# Define server logic
shinyServer(function(input, output) {

    output$equiv_upper_p <- renderText({
        paste("Equivalent Upper P-Value: ",
              exp(-input$sig[1]) %>% signif(digits = 3))
    })

    output$equiv_lower_p <- renderText({
        paste("Equivalent Lower P-Value: ",
              exp(-input$sig[2]) %>% signif(digits = 3))
    })

    output$genomePlot <- renderPlot({

        # Generate chromosome plot
        plot_snps <- function(traits){

            snps <- gwas_cancers %>%
                select(
                    Chr = seqnames,
                    Start = CHR_POS,
                    SNPid = SNPS,
                    Cancer = cancer_class,
                    PVALUE_MLOG,
                    EffectSize = OR.or.BETA
                ) %>%
                filter(
                    Cancer %in% traits,
                    # PVALUE_MLOG < input$sig,
                    if(input$sig[1] == input$sig[2]){
                        PVALUE_MLOG < input$sig[1]}
                    else{
                        between(PVALUE_MLOG, input$sig[1], input$sig[2])
                    },
                    # EffectSize > input$effectsize
                    between(EffectSize, input$effectsize[1], input$effectsize[2])
                ) %>%
                mutate(
                    Chr = factor(Chr, levels = chrom_levels)
                )

            p <- ggplot() + #data = snps, aes(x = Chr, y = as.numeric(Start))) +
                geom_bar(
                    data    = chromosomes,
                    mapping = aes(
                        x = SEQNAME,
                        y = as.numeric(SEQLENGTH)
                    ),
                    stat    = "identity",
                    fill    = "grey90",
                    colour  = "black",
                    show.legend = F
                    ) +
                theme(
                    axis.text = element_text(size = 14),
                    axis.title = element_text(size = 14),
                    axis.text.y = element_blank(),
                    axis.title.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "white"),
                    legend.position = "bottom"
                )

            p + geom_segment(
                data    = snps,
                mapping = aes(
                    x      = as.numeric(Chr) - 0.45,
                    xend   = as.numeric(Chr) + 0.45,
                    y      = Start,
                    yend   = Start,
                    colour = stringr::str_to_title(Cancer)
                    ),
                size = 2, alpha = 0.5,
                show.legend = T
            ) +
            guides(colour = guide_legend(ncol = 3, byrow = F, title = "Cancer Class")) +
            scale_colour_brewer(palette = "Set1") +
            labs(x = "Chromosome", y = "Position")

        }

        plot_snps(input$cancerclass)

})

    })

