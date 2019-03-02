# Simple app to visualize physico-chemical conditions in the Baltic Sea
# All data are from HELCOM
# downloaded on 2018-04-20 from 
# here: http://metadata.helcom.fi/geonetwork/srv/eng/catalog.search#/metadata/d9f761dc-dbf1-4180-87e8-578cc1fdda1a

library(shiny)
library(shinydashboard)
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(mapdata))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(cowplot))

# data
baltic <- read_csv(file = "baltic-data-reorganized-from-eutrophication-shape-file.csv", 
                   col_types = paste(c("c", "i", "c", "i", "i", "i", "i", "i",
                                       'n', 'n', 'i', 'n', 'c', 'c', 'n', 'n',
                                       rep('n', 15), 'l'), collapse=""))

baltic <- select(baltic, -Type, -coords.x1, -coords.x2, -optional)
baltic <- select(baltic, Cruise:BotDepth, StationNam:AUID, Depth, Secchi, Temperature=Temp, Salinity:Chl_a)
Vars <- colnames(baltic)[13:28]

# get a map of the baltic
load("wm.Rsave")

header  <- dashboardHeader(titleWidth = 300, title="Baltic Sea gradient")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Customize", tabName = "Customize", icon = icon("th"))
    # ,
    # menuItem("About", icon = icon("th"), tabName = "About")
  )
)
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Customize",
            fluidRow(
              box(width = 2, height = 800, status = 'primary', solidHeader = TRUE, collapsible = FALSE, title = "Customize ranges and variables",
                  sliderInput(inputId = "Year",  label = "Years",  step = 1,sep = "",
                              min = min(baltic$Year), 
                              max=max(baltic$Year), 
                              value= range(baltic$Year, na.rm=T)),
                  sliderInput(inputId = "Month", label = "Months", step = 1, 
                              min = min(baltic$Month), 
                              max=max(baltic$Month),
                              value= range(baltic$Month)),
                  sliderInput(inputId = "Depth", label = "Depth", step = 1,
                              min = min(baltic$Depth), 
                              max=max(baltic$Depth), 
                              value= range(baltic$Depth)),
                  checkboxGroupInput(inputId = "What", 
                                     label = "Select two variables", 
                                     choices = Vars, 
                                     selected = Vars[3:4])
              ),
              box(width=10, height = 800, status = 'primary', 
                  solidHeader = TRUE, collapsible = FALSE, 
                  title = "Plot",
                  actionButton("refresh4", "Update the plot", icon = icon("refresh"), width = "200px"),
                  plotOutput(outputId = "p1", height = "700")
                  )
      )
    )
  )
)

ui <- dashboardPage(title = "Baltic Sea Gradient (HELCOM data)",
                    header, 
                    sidebar,
                    body
                    )


server <- function(input, output) {
  
  sub_data <- reactive({
    sub_baltic <- baltic %>% 
      filter(Year  >= input$Year[1]  && Year  <= input$Year[2]) %>% 
      filter(Month >= input$Month[1] && Month <= input$Month[2]) %>% 
      filter(Depth >= input$Depth[1] && Depth <= input$Depth[2]) %>%
      gather(., Cual, Cuanto, Depth:Chl_a)
    return(sub_baltic)
  })
    
  make_plot <- eventReactive({
    input$refresh4
  }, {
    sub_baltic <- sub_data() %>% filter(Cual %in% input$What)
    
    validate(
      need(nrow(sub_baltic) > 1 , "It appears that the filters produce an empty dataset. Please broaden the search.")
    )
    
    validate(
      need(length(input$What) %in% 1:3 , "Please select up to three variables to plot.\n(Until I figure out how to scale the plotting area correctly.) ")
    )
    
    mkpl <- function(df) {
      ggplot()  +
        geom_polygon(data=wm, fill="grey95", 
                     aes(x=long, y=lat, group = group), 
                     colour="black", size=.1 ) +
        theme_void()+
        coord_quickmap(xlim=c(9.5,30),  ylim=c(54, 65.5))+
        stat_summary_2d(data=df, aes(y=Latitude, x=Longitude, z=Cuanto), colour="white",bins = 600, alpha=.85) +
        theme(legend.position = "bottom", 
              legend.direction = "horizontal", 
              panel.border = element_rect(colour="black")) + 
        theme(plot.margin = unit(c(.2, .2, .2, .2), "cm")) +
        scale_fill_viridis(option=4, direction=-1, guide = guide_colorbar(
          direction = "horizontal",
          barheight = unit(4, units = "mm"),
          barwidth = unit(50, units = "mm"),
          draw.ulim = F,
          title.position = 'top',
          title.hjust = 0.5,
          label.hjust = 0.5
        )) 
    }
    
    pls <- sub_baltic %>% group_by(Cual) %>% nest %>%
      mutate(plot=purrr::map2(.x=data, .y=Cual,
                              .f=function(x,y) mkpl(df=x) +
                                labs(title=paste(
                                  "Data: HELCOM (www.helcom.fi)", "\n",
                                  "Parameter: ", y, "\n",
                                  "Years: ", paste0(input$Year, collapse="--"), "\n",
                                  "Months: ", paste0(month.name[input$Month], collapse="--"), "\n",
                                  "Depth: ", paste0(input$Depth, collapse="--"), "\n",
                                  # "Data: http://metadata.helcom.fi/geonetwork/srv/eng/catalog.search#/metadata/d9f761dc-dbf1-4180-87e8-578cc1fdda1a",
                                  sep=""),
                                    fill=y)))
    if (length(input$What) >1 ) {
      # theme_set(theme_cowplot(font_size=12, line_size = 0)) # reduce default font size
      return(cowplot::plot_grid(plotlist = pls$plot, ncol=3))
    } else {
      return(pls$plot)
      }
  })
  
  output$p1 <- renderPlot({
    make_plot()
  })
  
}

shinyApp(ui, server)