# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(RColorBrewer)
library(readr)
library(tidyr)

# Define the UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  fluidRow(
    column(12, align = "center", 
           div(style = "text-align: center;", 
               titlePanel("Urban Resources (Energy, Population, Tree Cover) and their contribution to Melbourne City’s CO2 offset.")),
           div(style = "text-align: legt;",
               h5("Presented by:"),
               h5("Student Name: Anish S"),
               h5("Student ID: 34113335"), 
               h5("Applied 11"))
           )),
  fluidRow(
    column(8, align = "center", 
           div(style = "text-align: left;",
               h3("Introduction")),
           div(style = "text-align: left;",
               p("This webpage aims to present data on 3 urban resources - Energy, Tree Cover and Population \
               Distribution and how these resources contribute/reduce to City of Melbourne's CO2 offset over \
               the years. While there are many different urban aspects, I personally have chosen these 3 as actions \
               can be readily taken based on data analysis of how they influence the City of Melbourne."),
               p(" I have primarily divided my data story into 2 parts"),
               p("- (1) Spread of these 3 resources throughout the city and"),
               p("- (2) How they individually contribute City's Total Offset over the years."),
               p("In Section (1), I will primarily show how these resources are spread or utilized throughout the city \
               over the years. In Section (2), I will show how these resources have contributed to the City's CO2 Offset over \
               the years.")))
  ),
  fluidRow(
    column(12, align = "center",
           div(style = "text-align: left;",
               h3("Spread of Resources")), 
           div(style = "text-align: left;",
               p("There are 13 CLUE Areas or suburb-locatlities identified by the 'Census of Land Use and Employment'. They \
               are as follows:"),
               tags$ol(
                 tags$li("Carlton"),
                 tags$li("Docklands"),
                 tags$li("East Melbourne"),
                 tags$li("Kensington"),
                 tags$li("Melbourne (CBD)"),
                 tags$li("North Melbourne"),
                 tags$li("Parkville"),
                 tags$li("Port Melbourne"),
                 tags$li("South Yarra"),
                 tags$li("Southbank"),
                 tags$li("West Melbourne (Industrial)"),
                 tags$li("West Melbourne (Residential)"),
                 tags$li("Melbourne (Remainder)")
               ),
               p(" I have covered the spread of trees, energy consumtion & population distribution across \
               these 13 areas of the City and in case of resources liek energy and population, I have also provided \
               a time aspect to the respective visualisations to see how these resources (energy & popln.) have been utilised \
               and developed over time."),
               p("Let's go through each resource one-by-one:"),
               p(tags$b("1. Tree Cover in Melbourne: \n"), 
                 ("This visualisation shows the spread of trees by their nature of location (in Street or Park) \
                 across the 13 CLUE Areas of the City of Melbourne. It's easily observed that 'Parkville' has the \
                 higest no. of trees compared to any other region owing due to hosting a lot of parks. Please hover over the \
                 bars to get the exact data.")),
               p(tags$b("2. Energy Consumption (tJ) across the City: \n"),
                 ("This visualisation shows the Consumption of Energy in each of the 13 different CLUE Areas for \
                 the years 2011-2021. You can play around and filter the data by any combination of years via the Checkbox list \
                 right below these 3 visualisations. Please hover over the segments of the donut chart to get the exact \
                 energy consumption values. Here, it appears that Mlebourne CBD consumes the most amount of energy in any of the years\
                 of the past decade. This could be due to it being a major tourist spot and in lieu of being the heart of Melbourne.")),
               p(tags$b("3. Population Distribution by Age and Gender: \n"),
                 ("This visualisation shows the Population Pyramid for the selected CLUE Areas (choose from below\
                  this visualisation) for teh 4 different age categories and by Gender (Blue - Male and Pink - Female) across\
                  the years 2011 - 2021. For more accurate population counts, please hover over the horizontal bars \
                  corresponding to each age category - gender combination. Here, most of the data is concentrated around \
                  'Adult' category of age but more or less equally distributed amongst the 2 genders.")),
               p(""))
           ),
    column(4, 
           plotlyOutput("treePlot"), 
           p(tags$i(class = "ref_title", "Tree Data Sourced from:")),
           p(class = "references", 
             tags$ul(
               tags$li(HTML('<a href = "https://data.melbourne.vic.gov.au/explore/dataset/trees-with-species-and-dimensions-urban-forest/information/?location=12,-37.81303,144.94575&basemap=mbs-7a7333"> Tree cover  </a>')),
               tags$li(HTML('<a href = "https://8billiontrees.com/carbon-offsets-credits/carbon-ecological-footprint-calculators/how-much-carbon-does-a-tree-capture/#:~:text=How%20Much%20Carbon%20Does%20a%20Tree%20Offset%3F,it%20reaches%2040%20years%20old.&text=An%20acre%20of%20mature%20trees,drive%20your%20car%2026%2C000%20miles."> Tree Sequestration </a>'))
             ))),
    column(4, 
           plotlyOutput("energyPlot"), 
           p(tags$i(class = "ref_title", "Energy Data Sourced from:")),
           p(class = "references",
             tags$ul(
               tags$li(HTML('<a href = "https://data.melbourne.vic.gov.au/explore/dataset/property-level-energy-consumption-modelled-on-building-attributes-baseline-2011-/information/"> Energy Consumption in Melbourne </a>')),
               tags$li(HTML('<a href = "https://www.iea.org/data-and-statistics/data-product/greenhouse-gas-emissions-from-energy-highlights"> World CO2 offsets per energy consumed in recent years</a>'))
             ))),
    column(4, 
           plotlyOutput("populationPlot"), 
           checkboxGroupInput(
             inputId = "selected_clue_areas",
             label = "Select CLUE Areas",
             choices = NULL,
             selected = NULL, 
             inline = TRUE
           ), 
           p(tags$i(class = "ref_title",  "Population Data sourced from:")),
           p(class = "references",
             tags$ul(
               tags$li(HTML('<a href = "https://www.abs.gov.au/statistics/people/population/regional-population-age-and-sex/latest-release#data-downloads"> Population Spread in Australia </a>')),
               tags$li(HTML('<a href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5666301/"> CO2 exhalation rates by people </a>'))
             )))
  ),
  fluidRow(
    column(width = 12,
           align = "center", 
           checkboxGroupInput("selected_years", "Select Year(s):", choices = 2011:2021, selected = 2011, inline = TRUE)
    ),
    column(width = 12,
           align = "left",
           div(style = "text-align: left; color: #db520d;",
               p(tags$b("NOTE:"),
                 ("The 'Year(s)' Check box above is common for bothe Visualisations (2) and (3)."))))
  ),
  fluidRow(
    column(12, align = "center",
           div(style = "text-align: left;",
               h3("CO2 offset by Areas in Melb. City over the years")),
           div(style = "text-align: left;",
               p("This section deals with the CO2 contribution of each of 3 resources (Energy, Population Distribution \
                 and Tree Cover) over the years in each of the 13 CLUE Area regions of the City of Melbourne."),
               p("Play around with the different combinations of the 3 resources to see their impact on the City's \
               total CO2-offset over the years. Also hover over each of the data points in the graph to get exact values of CO2\
                offset in tonnes."))),
    column(12, align = "center",
           plotlyOutput("LineGraph"),
           column(width = 12,
                  align = "center",
                  checkboxGroupInput("CO2_options", "Select CO2 Data:",
                                     choices = c("Tree", "Population", "Energy"),
                                     selected = c("Tree", "Population", "Energy"),
                                     inline = TRUE)),
           p(tags$i(class = "ref_title",  "CO2 Data sourced from:")),
           p(class = "references", style = 'text-align: central',
             tags$ul(
               tags$p(class = "references", HTML('<a href = "https://www.iea.org/data-and-statistics/data-product/greenhouse-gas-emissions-from-energy-highlights"> World CO2 offsets per energy consumed in recent years</a>')),
               tags$p(class = "references", HTML('<a href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5666301/"> CO2 exhalation rates by people </a>')),
               tags$p(class = "references", HTML('<a href = "https://8billiontrees.com/carbon-offsets-credits/carbon-ecological-footprint-calculators/how-much-carbon-does-a-tree-capture/#:~:text=How%20Much%20Carbon%20Does%20a%20Tree%20Offset%3F,it%20reaches%2040%20years%20old.&text=An%20acre%20of%20mature%20trees,drive%20your%20car%2026%2C000%20miles."> Tree Sequestration </a>'))
             )))
    )
)

# Define the server logic
server <- function(input, output, session) {
  # Load the data
  tree_data <- read.csv('final_tree.csv')
  energy_data <- read.csv("final_energy.csv")
  popln_data <- read.csv('final_population.csv')
  CO2_data <- read.csv('final_merged_CO2.csv')
  
  
  # Define the plot for tree data
  output$treePlot <- renderPlotly({
    # Create the ggplot object
    p <- ggplot(tree_data, aes(x = featurenam, y = no_trees, fill = located_in, 
                               text = paste("No. of Trees:", no_trees, "<br>CO2 Sequestered:", round(sum_CLUEArea_CO2offset_tonnes, 2), "tonnes"))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_brewer(palette = "Set3") +
      labs(x = "CLUE Area", y = "Number of Trees", fill = "Location", title = "1. No. of trees by CLUE Area") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert ggplot object to plotly for interactivity
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
  
  # Reactive data based on selected years
  filtered_eng_data <- reactive({
    req(input$selected_years)
    energy_data %>%
      filter(year %in% input$selected_years)
  })
  
  
  # Render the interactive donut chart
  output$energyPlot <- renderPlotly({
    # Summarize the data by CLUE Areas (featurenam) for the selected years
    summarized_data <- filtered_eng_data () %>%
      group_by(featurenam) %>%
      summarize(energy_tJ = sum(energy_tJ)) %>%
      arrange(desc(energy_tJ)) %>%
      mutate(featurenam = factor(featurenam, levels = unique(featurenam)))
    
    # Create interactive donut chart
    plot_ly(
      data = summarized_data,
      labels = ~featurenam,
      values = ~energy_tJ,
      type = 'pie',
      hole = 0.4,  # Create a donut chart
      textinfo = 'label+percent',
      hoverinfo = 'none',
      hovertemplate = paste(
        "CLUE Area: %{label}<br>",
        "Energy Consumed: %{value} tJ<br>",
        "<extra></extra>"
      ),
      marker = list(colors = RColorBrewer::brewer.pal(9, "Set3")), 
      outsidetextfont = list(size = 10)
    ) %>%
      layout(
        title = "2. Energy Consumption by CLUE Areas",
        showlegend = FALSE, 
        margin = list(l = 40, r = 20, b = 75, t = 40)
        
      )
  })
  
  # Create reactive expression to filter data based on selected years and gender
  filtered_popln_data <- reactive({
    req(input$selected_years, input$selected_clue_areas)
    popln_data %>%
      filter(Year %in% input$selected_years,
             Gender != "A",  # Exclude 'A' category
             SA2.name %in% input$selected_clue_areas) %>%
      group_by(Year, Age_category, Gender) %>%
      summarize(Population_Count = sum(Population_Count)) %>%
      ungroup() %>%
      mutate(Population_Count = ifelse(Gender == "M", -Population_Count, Population_Count))
  })
  
  # observe the input for Clue Areas selected
  observe({
    updateCheckboxGroupInput(session, "selected_clue_areas",
                             choices = unique(popln_data$SA2.name),
                             selected = unique(popln_data$SA2.name), 
                             inline = TRUE)
  })
  
  output$populationPlot <- renderPlotly({
    # Choose specific colors from Set3 palette
    chosen_colors <- brewer.pal(12, "Set3")[c(8, 5)]
    
    # Create population pyramid
    pyramid_plot <- plot_ly(data = filtered_popln_data(),
                            x = ~Population_Count,
                            y = ~Age_category,
                            color = ~Gender,
                            colors = chosen_colors,  
                            type = "bar",
                            orientation = "h",
                            hoverinfo = "text",
                            text = ~paste("Gender: ", Gender, "<br>",
                                          "Population: ", abs(Population_Count))) %>%
      layout(title = "3. Population Pyramid",
             xaxis = list(title = "Population Count"),
             yaxis = list(title = "Age Category"),
             barmode = "relative",
             bargap = 0.2,
             bargroupgap = 0.1)
    
    # Render the plot
    pyramid_plot
  })
  
  # Generate line chart based on selected options
  output$LineGraph <- renderPlotly({
    # Initialize selected_columns as an empty vector
    selected_columns <- c()
    
    # Check which options are selected and construct selected_columns accordingly
    if ("Energy" %in% input$CO2_options) {
      selected_columns <- c(selected_columns, "Year", "CLUE_Areas", "Energy_CO2_tonnes")
    }
    if ("Tree" %in% input$CO2_options) {
      selected_columns <- c(selected_columns, "Year", "CLUE_Areas", "Trees_CO2_absorbed_tonnes")
    }
    if ("Population" %in% input$CO2_options) {
      selected_columns <- c(selected_columns, "Year", "CLUE_Areas", "Population_CO2_tonnes")
    }
    if ("Energy" %in% input$CO2_options && "Tree" %in% input$CO2_options) {
      selected_columns <- c(selected_columns, "Year", "CLUE_Areas", "Energy_CO2_tonnes", "Trees_CO2_absorbed_tonnes")
    }
    if ("Energy" %in% input$CO2_options && "Population" %in% input$CO2_options) {
      selected_columns <- c(selected_columns, "Year", "CLUE_Areas", "Energy_CO2_tonnes", "Population_CO2_tonnes")
    }
    if ("Tree" %in% input$CO2_options && "Population" %in% input$CO2_options) {
      selected_columns <- c(selected_columns, "Year", "CLUE_Areas", "Trees_CO2_absorbed_tonnes", "Population_CO2_tonnes")
    }
    if ("Tree" %in% input$CO2_options && "Population" %in% input$CO2_options && "Energy" %in% input$CO2_options) {
      selected_columns <- c(selected_columns, "Year", "CLUE_Areas", "Trees_CO2_absorbed_tonnes", "Population_CO2_tonnes", "Energy_CO2_tonnes")
    }
    
    
    # Check if only one option is selected
    if (length(input$CO2_options) == 1) {
      # Filter data based on selected options without calculating CO2_offset
      filtered_CO2_data <- CO2_data %>%
        select(all_of(selected_columns)) %>%
        arrange(CLUE_Areas, Year)
      
      # Convert Trees_CO2_absorbed_tonnes to negative values if present
      if ("Trees_CO2_absorbed_tonnes" %in% selected_columns) {
        filtered_CO2_data$Trees_CO2_absorbed_tonnes <- -filtered_CO2_data$Trees_CO2_absorbed_tonnes
      }
      
      # assign respective column data to CO2_offset
      filtered_CO2_data$CO2_offset <- filtered_CO2_data[, -c(1, 2)]
      
    } else {
      # Filter data based on selected options
      filtered_CO2_data <- CO2_data %>%
        select(all_of(selected_columns)) %>%
        arrange(CLUE_Areas, Year)
      
      # Convert Trees_CO2_absorbed_tonnes to negative values if present
      if ("Trees_CO2_absorbed_tonnes" %in% selected_columns) {
        filtered_CO2_data$Trees_CO2_absorbed_tonnes <- -filtered_CO2_data$Trees_CO2_absorbed_tonnes
      }
      
      # Calculate CO2_offset by summing selected columns (except Year and CLUE_Areas)
      filtered_CO2_data$CO2_offset <- rowSums(filtered_CO2_data[, -c(1, 2)], na.rm = TRUE)
    }
    
    # Create line chart
    line_chart <- filtered_CO2_data %>%
      plot_ly(x = ~Year, y = ~CO2_offset, color = ~CLUE_Areas, 
              colors = brewer.pal(n = n_distinct(filtered_CO2_data$CLUE_Areas), name = "Set3"),
              mode = "lines+markers",
              hoverinfo = "text",
              text = ~paste("CLUE Area:", CLUE_Areas, "<br>",
                            "Year:", Year, "<br>",
                            "CO2 Offset:", CO2_offset)) %>%
      layout(title = "4. CO2 Offset by Resource",
             xaxis = list(title = "Year"),
             yaxis = list(title = "CO2 Offset"),
             showlegend = TRUE)
    
    # Render the plot
    line_chart
  })


}

# Run the application 
shinyApp(ui = ui, server = server)
