library(shiny)
library(tidyverse)
library(viridis)
library(cowplot)

library(readr)
cleaned_root <- readRDS("clean_true_root.rda")

theme_tsl <- function(base_size = 11, base_family = "") {
  theme_grey(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # white background and dark border, no gridlines
      panel.background = element_rect(fill = "white", colour = NA),
      panel.border = element_rect(fill = NA, colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # axis options
      axis.ticks = element_line(colour = "black"),
      axis.text = element_text(size = rel(0.8), colour = "black"),
      axis.title.x = element_text(margin=margin(10,0,0,0)),
      axis.title.y = element_text(margin=margin(0,10,0,0), angle = 90),
      # facetting options
      strip.background = element_rect(fill = "grey85", colour = "grey20"),
      # match legend key to background
      legend.key = element_rect(fill = "white", colour=NA),
      
      complete = TRUE
    )
}
shinyplot <- function(data, access, exp) {
  data %>% 
    filter(accession == access,
           experiment %in% exp) %>% 
    ggplot(aes(start_x,start_y)) + 
    geom_segment(aes(xend=end_x,yend=end_y, color=hours),arrow = arrow(length = unit(0.01, "npc")),alpha=1) + 
    xlim(0,15) + 
    ggtitle(exp) + coord_fixed(ratio = 1) + 
    scale_y_reverse(limits=c(30,0)) + scale_color_gradient(low="darkred", high="gold", limits = c(0, 768)) + 
    theme_set(theme_cowplot()) + theme_tsl() + theme(legend.position = "none",
                                                     axis.title = element_blank()) + facet_grid(~rhiz_num)
  
}

ui <- fluidPage(
  titlePanel("6 accessions GLO-Roots root system architecture"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "access1", 
                  label = "Accession A", 
                  choices = list('Bay-0', 'Col-0', 'Est1', 'Sha', 'Tottarp', 'Tsu1'), selected = 'Bay-0'),
      
      selectInput(inputId = "access2", 
                  label = "Accession B", 
                  choices = list('Bay-0', 'Col-0', 'Est1', 'Sha', 'Tottarp', 'Tsu1'), selected = 'Bay-0'),

    ),
    
    mainPanel(
      textOutput("text1"),
      plotOutput("access1"),
      textOutput("text2"),
      plotOutput("access2")
    )
  )
)

server <- function(input, output) {
  output$text1 <- renderText({input$access1})
  output$access1 <- renderPlot({
    
    plotall <- cleaned_root %>% 
      filter(accession == input$access1,
             experiment %in% c("HL06", "HL11", "HL15")) %>% 
      ggplot(aes(start_x,start_y)) + 
      geom_segment(aes(xend=end_x,yend=end_y, color=hours),arrow = arrow(length = unit(0.01, "npc")),alpha=1) + 
      xlim(0,15) + 
      scale_y_reverse(limits=c(30,0)) + scale_color_gradient(low="darkred", high="gold", limits = c(0, 768)) + 
      coord_fixed(ratio = 1) + 
      ggtitle("All replicates") +
      xlab("Width (cm)") + 
      ylab("Depth (cm)") + 
      theme_set(theme_cowplot()) + theme_tsl() + theme(legend.position = "bottom")
    
    plot1 <- shinyplot(cleaned_root, input$access1, "HL06")
    plot2 <- shinyplot(cleaned_root, input$access1, "HL11")
    plot3 <- shinyplot(cleaned_root, input$access1, "HL15")
    
    plotrep <- plot_grid(plot1, plot2, plot3, nrow = 3)
    plot_grid(plotall, plotrep, nrow =1, rel_widths = c(1, 3))
  })
  output$text2 <- renderText({input$access2})
  output$access2 <- renderPlot({
    
    plotall <- cleaned_root %>% 
      filter(accession == input$access2,
             experiment %in% c("HL06", "HL11", "HL15")) %>% 
      ggplot(aes(start_x,start_y)) + 
      geom_segment(aes(xend=end_x,yend=end_y, color=hours),arrow = arrow(length = unit(0.01, "npc")),alpha=1) + 
      xlim(0,15) + 
      scale_y_reverse(limits=c(30,0)) + scale_color_gradient(low="darkred", high="gold", limits = c(0, 768)) + 
      coord_fixed(ratio = 1) + 
      ggtitle("All replicates") +
      xlab("Width (cm)") + 
      ylab("Depth (cm)") + 
      theme_set(theme_cowplot()) + theme_tsl() + theme(legend.position = "bottom")
    
    plot1 <- shinyplot(cleaned_root, input$access2, "HL06")
    plot2 <- shinyplot(cleaned_root, input$access2, "HL11")
    plot3 <- shinyplot(cleaned_root, input$access2, "HL15")
    
    plotrep <- plot_grid(plot1, plot2, plot3, nrow = 3)
    plot_grid(plotall, plotrep, nrow =1, rel_widths = c(1, 3))
  })
}

shinyApp(ui = ui, server = server)
