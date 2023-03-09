#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(plotly)
library(metR)
library(DT)


# df <- read_csv("data/EV6005_assignment2.csv")
df <- readxl::read_xls("data/Assignment 2 Data file.xls", sheet = "Phys & chem data", skip = 3, col_names = c("Date", "Depth", "Temp",	"Dissolved Oxygen", "remove", "Chlorophyll_a", "PO4-Phosphate",	"Silica","Total Organic Nitrogen")) %>% 
  filter(!is.na(Date)) %>% 
  mutate(Date = as.Date(Date)) %>% 
  select(-remove)
df
depths <- unique(df$Depth)[order(unique(df$Depth))]

lst <- lapply(c("Temp", "Dissolved Oxygen"), function(v) {
  wid <- pivot_wider(df, id_cols = Date, names_from = Depth, names_prefix = "wtr_", values_from = v)
  wid <- wid[, c("Date", paste0("wtr_", depths))]
  wid[, 2:ncol(wid)] <- t(apply(wid[, 2:ncol(wid)], 1, function(x)  approx(depths, x, xout = depths, rule = 2)$y))
  lng <- pivot_longer(wid, cols = wtr_1:wtr_13, names_to = "Depth", names_prefix = "wtr_", names_transform = as.numeric) %>% 
    mutate(var = v)
})
names(lst) <- c("Temp", "Dissolved Oxygen")






plank <- readxl::read_xls("data/Assignment 2 Data file.xls", sheet = "Plankton data", skip = 4) %>% 
  select(-c("...6", "...10", "...13", "R= rare")) %>% 
  rename("Pennate diatoms" = diatoms, Asterionella = nella, Stephanodiscus = discus, "Melosira spp" = "spp.",
         Aphanizomenon = menon, "Green algae" = algae)
  
# plank
plank2 <- plank %>% 
  gather(var, value, -Date) %>% 
  mutate(value =  factor(value, levels = c("R", "O", "A"), labels = c("Rare", "Occasional", "Abundant"))) %>% 
  # mutate(family = if_any(var %in% c("Pennate diatoms", "Asterionella", "Stephanodiscus"), "Diatom", "TEST")) %>% 
  mutate(family = case_when(var %in% c("Pennate diatoms", "Asterionella", "Stephanodiscus", "Melosira spp") ~ "Diatom",
                            var %in% c("Anabaena", "Aphanizomenon", "Microcystis") ~ "Cyanobacteria",
                            var %in% c("Ceratium", "Green algae" ) ~ "Others")) %>% 
  mutate(family = factor(family))


plank2

plank2 %>% 
  drop_na() %>% 
  ggplot() +
  # geom_point(aes(Date, value, colour = var)) +
  # geom_line(aes(Date, value, colour = var)) +
  geom_tile(aes(Date, var, fill = value, group = family)) +
  # geom_point(aes(Date, var, size = value, group = family, colour = value), alpha = 0.5, shape = 15) +
  # facet_grid(~family, rows = 3, cols = 1) +
  facet_wrap(~family, ncol = 1, scales = "free_y") +
  scale_fill_brewer(type = "qual", palette = "Dark2") +
  scale_fill_brewer(type = "seq", palette = "Blues") +
  scale_colour_brewer(type = "seq", palette = "Blues") +
  labs(y = "Plankton species", fill = "Occurence", size = "Occurence") +
  theme_bw(12)

# ggplot(df) +
#   geom_point(aes(Date, Temp, colour = Depth))
# 
df %>%
  select(1:4) %>% 
  gather(var, value, 3:4) %>% 
  mutate(value = round(value, 1)) %>% 
  ggplot() +
  geom_text(aes(Date, Depth, label = value), size = 2) +
  facet_wrap(~var, nrow = 1) +
  scale_y_reverse() +
  theme_bw(12)

hmap.cols <- rev(RColorBrewer::brewer.pal(11, "Spectral"))


df %>%
  select(1:3) %>% 
  gather(var, value, 3) %>% 
  mutate(value = round(value, 1)) %>% 
  filter(Depth %in% c(1, 3,5, 7,9, 11)) %>% 
  ggplot() +
  geom_tile(aes(Date, Depth, fill = value)) +
  # geom_raster(aes(Date, Depth, fill = value), interpolate = TRUE) +
  # geom_contour(aes(Date, Depth, z = value), bins = 20) +
  # geom_label_contour(aes(Date, Depth, z = value), skip = 0) +
  facet_wrap(~var, nrow = 1) +
  scale_fill_gradientn(colours = hmap.cols, na.value = "grey") +
  scale_y_reverse() +
  theme_bw(12)

brks <- seq(0, 200, 10)

df %>%
  select(c(1,2,4)) %>% 
  gather(var, value, 3) %>% 
  mutate(value2 = round(value, 1)) %>% 
  filter(Depth %in% c(1, 3,5, 7,9, 11)) %>% 
  mutate(Depth2 = case_when(
    Depth == 1 ~ 0,
    Depth == 11 ~ 12,
    TRUE ~ Depth
  )) %>% 
  mutate(value = case_when(
    value < 20 ~ 110,
    TRUE ~ value
  )) %>%
  ggplot() +
  geom_text(aes(Date, Depth, label = value2), size = 3) +
  # geom_contour(aes(Date, Depth, z = value), breaks = brks) +
  geom_contour2(aes(Date, Depth2, z = value, label = ..level..), breaks = brks, colour = "blue", label.placer = label_placer_minmax(
    direction = c("vertical", "horizontal"),
    rot_adjuster = isoband::angle_fixed()
  )) +
  geom_contour_fill(aes(Date, Depth, z = value)) +
  # geom_text_contour(aes(Date, Depth, z = value), min.size = 2, skip = ) +
  # geom_label_contour(aes(Date, Depth, z = value), skip = 0) +
  facet_wrap(~var, nrow = 1) +
  # scale_fill_gradientn(colours = hmap.cols, na.value = "grey") +
  scale_y_reverse() +
  theme_bw(12)


df %>%
  select(1:3) %>% 
  gather(var, value, 3) %>% 
  mutate(value = round(value, 1)) %>% 
  filter(Depth %in% c(1, 3,5, 7,9, 11)) %>% 
  ggplot() +
  geom_point(aes(Date, Depth, colour = value)) +
  stat_density2d(aes(Date, Depth, fill = value), geom="polygon", alpha = .2) +
  geom_density_2d(aes(Date, Depth,),colour = "white", alpha = .4) +
  facet_wrap(~var, nrow = 1) +
  scale_fill_gradientn(colours = hmap.cols, na.value = "grey") +
  scale_colour_gradientn(colours = hmap.cols, na.value = "grey") +
  scale_y_reverse() +
  theme_bw(12)


library(akima)
library(ggplot2) 


mendota_interp.df <- interp(x = as.numeric(df$Date), 
                            y = df$Depth,
                            z = df$Temp,
                            xo = seq(min(as.numeric(df$Date)), max(as.numeric(df$Date)), by = 1),
                            yo = seq(min(df$Depth), max(df$Depth), by = 0.5),
                            extrap = TRUE,
                            linear = TRUE)
interp.df <- interp2xyz(mendota_interp.df, data.frame = TRUE)
interp.df <- interp.df %>% 
  rename(date = x, depth = y, value = z) %>% 
  mutate(date = lubridate::as_date(date))

ggplot(interp.df, aes(x = date, y = depth, z = value, fill = value)) +
  geom_raster() +
  scale_y_reverse(expand = c(0,0)) +
  scale_fill_gradientn(colours = hmap.cols, na.value = "grey", name = "Water\nTemp \nºC") +
  scale_x_date(date_breaks = "1 month", 
               date_labels = "%b",
               # limits = as_date(c('2016-12-06','2017-02-25')),
               # labels=date_format("%b-%d"),
               expand = c(0,0)) + 
  ylab("Depth (m)") +
  xlab("") 
  


# 
# df %>% 
#   mutate(Temp = round(Temp)) %>%
#   filter(Depth <= 11) %>% 
#   ggplot() +
#   geom_tile(aes(Date, Depth, fill = Temp)) +
#   scale_y_reverse()

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("EV6005 - Assignment 2",
             tabPanel("Introduction",
                      column(12,
                             h1("Interpreting lake monitoring data"),
                             p("This assignment is worth 20 marks (10%) towards the final module mark."),
                             p("You must submit your assignment through Turnitin before the 21st November 2022 (23:59 UTC). Penalties for late submission will be applied.")
                             ),
                      column(6,
                             h2("Part A: Physical changes in the water column of a reservoir"),
                             p("First, you have to download a set of actual monitoring data for temperature and oxygen from a shallow reservoir in a temperate climate in 1993. The reservoir is used for public water supply and has a low retention time."),
                             downloadButton("download_data", "Download"),
                             p("Plot by hand the depth-time distributions (isopleths) of temperature and saturation of oxygen as two separate plots. From your plots describe, in no more than 250 words, what is happening in the water column over the year."),
                             p("You must photograph or scan your two plots and either incorporate them into your Word document or upload them separately to Turnitin.")
                             ),
                      column(6,
                             h2("Part B: Seasonal succession in the water column of the reservoir"),
                             p("The major algal species present in the reservoir and their relative abundance were also determined in a mixed sample from the surface 5 m. Nutrient concentrations were measured at 1 m depth. You have been provided with this data. Describe in no more than 250 words the seasonal succession within the phytoplankton community sampled and give possible reasons for the changes in species dominance that occurred during the year.")
                             )
                      ),
             tabPanel("Part A",
                      h2("Physical changes in the water column of a reservoir"),
                      p("Click through the tabs below to explore the data and different ways to visualize the data."),
                      tabsetPanel(
                        tabPanel("Data",
                                 sidebarPanel(
                                   p("Explore the data table across. This is the same format as the data is presented within the Excel document.")
                                 ),
                                 mainPanel(
                                   h2("Physical & chemical data"),
                                   DT::DTOutput("phys_data")
                                 )
                        ),
                        tabPanel("Plot - Time series & Profile",
                                 sidebarPanel(
                                   p("Select a variable below to plot."),
                                   checkboxGroupInput("var", "Variable", names(df)[3:4]),
                                   br(),
                                   p("Select a depth to plot data from"),
                                   selectInput("depth", "Depth(s)", choices = depths, multiple = TRUE),
                                   checkboxInput("add_line", "Add line(s) between points"),
                                   br(),
                                   p("Click on the points in the plot to plot a depth profile for that date below.")
                                   # selectInput("date", "Date", choices = unique(df$Date)),
                                 ),
                                 mainPanel(
                                   h2("Time series plots"),
                                   wellPanel(
                                     plotlyOutput("var_ts", height = "600px")
                                     ),
                                   p("The plot above is interactive. Click on a point to view a vertical profile from that date."),
                                   h2("Depth profiles"),
                                   wellPanel(
                                     plotlyOutput("var_profile", height = "600px"),
                                     )
                                   )
                                 ),
                        tabPanel("Plot for isopleths",
                                 sidebarPanel(
                                   radioButtons("var_sel", label = "Variable", choices = names(df)[3:4]),
                                   radioButtons("plot_type", label = "Plot", choices = c("Text", "Points")),
                                   checkboxInput("add_iso", "Add isopleths"),
                                   conditionalPanel("input.add_iso",
                                                    numericInput("contour_int", "Isopleth interval", 2, min = 1, max = 5, step = 1),
                                                    p("To aid in interpretation of these diagram, colour can be used to fill the spaces between the contours."),
                                                    checkboxInput("fill_contour", "Fill contours")
                                                    )
                                   # conditionalPanel("input.var_sel == 'Dissolved Oxygen'",
                                   #                  checkboxInput("fix_outlier", "Correct outlier value")
                                   #                  )
                                   ),
                                 mainPanel(
                                   plotOutput("var_text", height = "600px")
                                   )
                                 )
                        )
                      ),
             tabPanel("Part B",
                      h2("Seasonal succession in the water column of the reservoir"),
                      p("Click through the tabs below to explore the data and plots of the data."),
                      tabsetPanel(
                        tabPanel("Data",
                                 sidebarPanel(
                                   p("Here is a data table of the plankton data from the reservoir."),
                                   p("Data is classified as Abundant (A), Occasional (O) and Rare (R) and the cells are shaded to represent this classification.")
                                 ),
                                 mainPanel(
                                   p(tags$b("Table 2. ", "Occurence of plankton species in a temperate reservoir. Classified as rare (R), occasional (O) or abundant (A).")),
                                   DT::DTOutput("plank_data")
                                   )
                                 ),
                        tabPanel("Plot",
                                 fluidRow(
                                   column(4,
                                          p("Across is a timeseries plot of the plankton abundance."),
                                          checkboxGroupInput("plank_spp", "Select plankton species:", choices = unique(plank2$var), selected = unique(plank2$var))
                                   ),
                                   column(8,
                                          wellPanel(
                                            plotlyOutput("plank_plot", height = "600px")
                                            )
                                          )
                                   )
                                 )
                        )
                      )
             )
  )

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$download_data <- downloadHandler(
    filename = "ev6005_data_assignment2.xls", content = function(f) {
      file.copy("data/Assignment 2 Data file.xls", f)
    } 
  )
  
  
  dep <- reactiveValues(df = NULL)
  
  observe({
    dep$df <- df %>% 
      select(Date, Depth, input$var) %>% 
      gather(var, value, input$var) %>% 
      filter(Depth %in% input$depth)
  })
  
  output$phys_data <- DT::renderDT({
    
    df %>% 
      DT::datatable(options = list(pageLength = 20, searching = FALSE), selection = "none")
  })
  
  output$var_ts <- renderPlotly({
    
    validate(
      need(length(input$var) > 0, "Select a variable.")
    )
    validate(
      need(length(input$depth) > 0, "Select a depth.")
    )
    
    p <- dep$df %>% 
      ggplot() +
      {if(input$add_line) geom_line(aes(Date, value, colour = Depth))} +
      geom_point(aes(Date, value, colour = Depth)) +
      facet_wrap(~var, nrow = 3, scales = "free_y") +
      scale_colour_continuous(type = "viridis", limits = c(0, 13), direction = -1) +
      theme_bw(12)
    p
    
    if(nrow(dep$df[selected$sel$pointNumber, "Date"]) == 1) {
      
      tst <- dep$df %>% 
        filter(Date %in% dep$df[selected$sel$pointNumber + 1, "Date"])
      
      p <- p +
        geom_point(data = tst, aes(Date, value, colour = Depth), size = 3, shape = 2)
    }
    
    ggplotly(p, source = "ts")
  })
  
  selected <- reactiveValues(sel = NULL)
  observeEvent(input$clear_sel1, {
    selected$sel <- NULL
  })
  
  
  
  #selected
  selected <- reactiveValues(sel = NULL, date = NULL)
  observe({
    # suppress warnings
    storeWarn<- getOption("warn")
    options(warn = -1)
    selected$sel <- event_data(event = "plotly_click", source = "ts")
    selected$date <- dep$df[selected$sel$pointNumber + 1, 1]
    
    #restore warnings, delayed so plot is completed
    shinyjs::delay(expr =({
      options(warn = storeWarn)
    }) ,ms = 100)
  })
  
  # Reset selected point when changing variables - https://stackoverflow.com/questions/42996303/removing-plotly-click-event-data
  observeEvent(input$view_var, {
    if(input$view_var > 1) {
      if(!is.null(selected$sel)) {
        selected$sel <- NULL
      }
      
    }
  })
  
    
  output$var_profile <- renderPlotly({
    
    validate(
      need(nrow(dep$df[selected$sel$pointNumber + 1, "Date"]) == 1, "Select a point on the plot.")
    )

      dat1 <- df %>% 
        select(Date, Depth, input$var) %>% 
        gather(var, value, input$var)
      dat2 <- dat1 %>% 
        filter(Date %in% as.vector(dep$df[selected$sel$pointNumber + 1, 1]))
      p <- ggplot() +
        ggtitle(as.vector(dep$df[selected$sel$pointNumber, 1])) +
        labs(title = dat1[1, 1]) +
        geom_point(data = dat1, aes(.data[["value"]], .data[["Depth"]]), alpha = 0) +
        geom_point(data = dat2, aes(.data[["value"]], .data[["Depth"]])) +
        geom_path(data = dat2, aes(.data[["value"]], .data[["Depth"]])) +
        scale_y_reverse(limits = c(13, 0)) +
        facet_wrap(~var, scales = "free_x") +
        # scale_x_continuous(limits = xlim) +
        theme_bw(12)
      ggplotly(p)
    })
  
  # Switch on points to add legend when adding contours
  observe({
    if(input$fill_contour) {
      updateRadioButtons(session = session, inputId = "plot_type", selected = "Points")
    }
  })
  
  output$var_text <- renderPlot({
    
    brks <- seq(0, 200, by = input$contour_int)
    
    # dat <- df %>%
    #   select(Date, Depth, input$var_sel) %>% 
    #   gather(var, value, input$var_sel) %>% 
    #   # filter(Date > "1993-05-01" & Date < "1993-10-01" & Depth %in% c(1, 3,5, 7,9, 11)) %>% 
    #   filter(Depth %in% c(1, 3,5, 7,9, 11)) %>% 
    #   mutate(Depth2 = case_when(
    #     Depth == 1 ~ 0,
    #     Depth == 11 ~ 12,
    #     TRUE ~ Depth
    #   ))
    
    dat <- lst[[input$var_sel]] %>%
      filter(Depth %in% c(1, 3,5, 7,9, 11)) %>% 
      mutate(Depth2 = case_when(
        Depth == 1 ~ 0,
        Depth == 11 ~ 12,
        TRUE ~ Depth
      ))
    
    # if(input$fix_outlier & input$var_sel == "Dissolved Oxygen") {
    #   dat <- dat %>% 
    #     mutate(value = case_when(
    #       value < 20 ~ 110,
    #       TRUE ~ value
    #     ))
    # }
      
    p <- dat %>% 
      mutate(value2 = round(value, 1)) %>% 
      ggplot() +
      {if(input$plot_type == "Points") geom_point(aes(Date, Depth, colour = value), size = 5)} +
      {if(input$plot_type == "Text") geom_text(aes(Date, Depth, label = value2), size = 2.5)} +
      {if(input$fill_contour) geom_contour_fill(aes(Date, Depth2, z = value), breaks = brks)} +
      {if(input$add_iso) geom_contour2(aes(Date, Depth2, z = value, label = ..level..), breaks = brks, colour = "blue", 
                                           label.placer = label_placement_minmax(
                                             direction = c("vertical", "horizontal"),
                                             rot_adjuster = isoband::angle_fixed()
                                           ))} +
      # {if(input$add_iso) geom_label_contour(aes(Date, Depth, z = value))} +
      facet_wrap(~var, nrow = 1) +
      guides(fill = FALSE) +
      scale_colour_gradientn(colours = hmap.cols, na.value = "grey") +
      scale_fill_gradientn(colours = hmap.cols, na.value = "grey") +
      scale_y_reverse(breaks = seq(12, 0, -1), limits = c(12, 0)) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      theme_bw(16)
    
    return(p)
  })
  
  observe({
    if(input$var_sel == "Temp") {
      updateNumericInput(session = session, "contour_int", value = 2, min = 1, max = 5, step = 1)
    } else if(input$var_sel == "Dissolved Oxygen") {
      updateNumericInput(session = session, "contour_int", value = 10, min = 5, max = 25, step = 5)
    }
  })
  
  output$plank_data <- DT::renderDT({
    
    cols <- RColorBrewer::brewer.pal(3, "Greens")
    plank %>% 
      mutate(Date = format(Date, "%Y-%m-%d")) %>% 
      DT::datatable(plank, options = list(pageLength = 20), selection = "none") %>%
      formatStyle(names(plank)[-1],
                  backgroundColor = styleEqual(
                    levels = list('R', 'O', "A"),
                    values = cols))
  })
  
  output$plank_plot <- renderPlotly({
    
    validate(
      need(length(input$plank_spp) > 0, "Select a plankton species.")
    )
    
    p <- plank2 %>% 
      drop_na() %>% 
      filter(var %in% input$plank_spp) %>%
      ggplot() +
      # geom_point(aes(Date, value, colour = var)) +
      # geom_line(aes(Date, value, colour = var)) +
      geom_tile(aes(Date, var, fill = value, group = family)) +
      # geom_point(aes(Date, var, size = value, group = family, colour = value), alpha = 0.5, shape = 15) +
      # facet_grid(~family, rows = 3, cols = 1) +
      facet_wrap(~family, ncol = 1, scales = "free_y") +
      # scale_fill_brewer(type = "qual", palette = "Dark2") +
      scale_x_datetime(date_breaks = "1 month", date_labels = "%b") +
      scale_fill_brewer(type = "seq", palette = "Greens") +
      scale_colour_brewer(type = "seq", palette = "Greens") +
      labs(y = "", fill = "Occurence", size = "Occurence") +
      theme_bw(12)
    
    gp <- ggplotly(p)
    
    gp <- ggplotly(p = p)
    
    # Get the names of the legend entries
    df <- data.frame(id = seq_along(gp$x$data), legend_entries = unlist(lapply(gp$x$data, `[[`, "name")))
    # Extract the group identifier
    df$legend_group <- gsub("^\\((.*?),\\d+\\)", "\\1", df$legend_entries)
    # Add an indicator for the first entry per group
    df$is_first <- !duplicated(df$legend_group)
    
    for (i in df$id) {
      # Is the layer the first entry of the group?
      is_first <- df$is_first[[i]]
      # Assign the group identifier to the name and legendgroup arguments
      gp$x$data[[i]]$name <- df$legend_group[[i]]
      gp$x$data[[i]]$legendgroup <- gp$x$data[[i]]$name
      # Show the legend only for the first layer of the group 
      if (!is_first) gp$x$data[[i]]$showlegend <- FALSE
    }
    
    return(gp)
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
