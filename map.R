if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(shiny, shinydashboard, sf, leaflet, maps,rnaturalearth, dplyr, readr, fuzzyjoin, ggplot2, assertthat)


working_dir <- "D:/Johannes/University/MasterModul_plus/FAllStudie"
setwd(working_dir)

initial_pdb_path <- file.path(getwd(), "files/7DDD.pdb")


covid_data <- read.csv("files/WHO-COVID-19-global-table-data.csv")

assert_that(dir.exists(working_dir), msg = paste("Directory does not exist:", working_dir))
setwd(working_dir)
assert_that(file.exists(initial_pdb_path), msg = paste("PDB file not found:", initial_pdb_path))

vaccination_data <- read.csv("files/vaccination-data.csv", stringsAsFactors = FALSE)


# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- st_transform(world, crs = 4326)

map_data <- world %>%
  inner_join(covid_data, by = c("name" = "Name"))
# Merge vaccination data with map_data
map_data <- map_data %>%
  left_join(vaccination_data, by = c("name" = "COUNTRY")) %>%
  mutate(
    PERSONS_VACCINATED_1PLUS_DOSE = as.numeric(PERSONS_VACCINATED_1PLUS_DOSE),
    PERSONS_LAST_DOSE = as.numeric(PERSONS_LAST_DOSE),
    PERSONS_BOOSTER_ADD_DOSE = as.numeric(PERSONS_BOOSTER_ADD_DOSE)
  )
# Shiny App UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", 
              href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
  ),
  titlePanel("Global COVID-19 Dashboard"),
  tabsetPanel(
    id = "tabs",
    # Map Tab
    tabPanel("Map",
             fluidRow(
               box(width = 12, leafletOutput("map", height = "600px"))
             )
    ),
    # Bar Plot Tab
    tabPanel("Deaths",
             fluidRow(
               box(width = 12, plotOutput("barplot", height = "600px"))
             )
    ),
    # Vaccination Progress Tab
    tabPanel("Vaccination Progress",
             fluidRow(
               box(width = 12, plotOutput("vaccination_plot", height = "600px"))
             ),
             fluidRow(
               box(width = 12, 
                   title = "Filters",
                   status = "primary",
                   solidHeader = TRUE,
                   sliderInput("max_percent", 
                               "Maximum Vaccination Percentage (%):", 
                               min = 0, max = 100, value = 100, step = 1),
                   sliderInput("num_countries", 
                               "Number of countries to show:", 
                               min = 5, max = 50, value = 20, step = 5)
               )
             )
    ),
    # Country Data Tab
    tabPanel("Country Data",
             fluidRow(
               box(width = 12, 
                   title = "Select a Country",
                   status = "primary",
                   solidHeader = TRUE,
                   selectInput("selected_country", 
                               "Choose a country:", 
                               choices = unique(map_data$name_long), 
                               selected = unique(map_data$name_long)[1])
               )
             ),
             fluidRow(
               box(width = 12, 
                   title = "Country Information",
                   status = "info",
                   solidHeader = TRUE,
                   uiOutput("country_info")
               )
             ),
             fluidRow(
               box(width = 4, 
                   title = "Cases per 100k: Comparison",
                   status = "warning",
                   solidHeader = TRUE,
                   plotOutput("cases_comparison_plot", height = "300px")
               ),
               box(width = 4, 
                   title = "Deaths per 100k: Comparison",
                   status = "danger",
                   solidHeader = TRUE,
                   plotOutput("deaths_comparison_plot", height = "300px")
               ),
               box(width = 4, 
                   title = "Vaccination Rate: Comparison",
                   status = "success",
                   solidHeader = TRUE,
                   plotOutput("vaccination_comparison_plot", height = "300px")
               )
             )
    )
  )
)

# Shiny App Server
server <- function(input, output, session) {
  # Render Leaflet map
  output$map <- renderLeaflet({
    pal <- colorNumeric(palette = "YlOrRd", domain = map_data$`Deaths...cumulative.total.per.100000.population`, na.color = "transparent")
    leaflet(map_data) %>%
      addTiles() %>%
      setView(lng = 10, lat = 50, zoom = 5) %>%  # Set initial center (Europe) and zoom level
      addPolygons(
        fillColor = ~pal(`Deaths...cumulative.total.per.100000.population`),
        color = "white",
        weight = 1,
        fillOpacity = 0.7,
        label = ~paste(name,
                       "Deaths per 100k: ", round(`Deaths...cumulative.total.per.100000.population`, 2),
                       "Cases per 100k: ", round(`Cases...cumulative.total.per.100000.population`, 2)),
        layerId = ~name,  # Add layerId for click events
        highlightOptions = highlightOptions(weight = 3, color = "blue", bringToFront = TRUE)
      ) %>%
      addLegend("bottomright", pal = pal, 
                values = ~`Deaths...cumulative.total.per.100000.population`,
                title = "Deaths per 100k", opacity = 1)
  })
  
  # Observe clicks on the map
  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    if (!is.null(click$id)) {
      # Get the clicked country data
      country_data <- map_data %>%
        filter(name == click$id)
      
      # Extract values
      country_name <- country_data$name_long
      population <- format(country_data$pop_est, big.mark = ",")
      income_group <- country_data$income_grp
      cases_total <- format(country_data$`Cases...cumulative.total`, big.mark = ",")
      deaths_total <- format(country_data$`Deaths...cumulative.total`, big.mark = ",")
      cases_per_100k <- round(country_data$`Cases...cumulative.total.per.100000.population`, 2)
      deaths_per_100k <- round(country_data$`Deaths...cumulative.total.per.100000.population`, 2)
      
      # Show modal dialog
      showModal(
        modalDialog(
          title = paste("COVID-19 Situation in", country_name),
          div(
            style = "font-size: 16px; line-height: 1.5;",
            p(strong("Population:"), population),
            p(strong("Income Group:"), income_group),
            p(strong("Total Cases:"), cases_total),
            p(strong("Total Deaths:"), deaths_total),
            p(strong("COVID Cases per 100,000 people:"), cases_per_100k),
            p(strong("COVID Deaths per 100,000 people:"), deaths_per_100k),
            p(strong("Vaccinated with at least one dose:"), format(country_data$PERSONS_VACCINATED_1PLUS_DOSE, big.mark = ",")),
            p(strong("Vaccinated with a complete primary series:"), format(country_data$PERSONS_LAST_DOSE, big.mark = ",")),
            p(strong("Vaccinated with at least one booster dose:"), format(country_data$PERSONS_BOOSTER_ADD_DOSE, big.mark = ",")),
            p("Official numbers reported by", country_name, 
              "officials / WHO as of 01.01.2025.")
          ),
          easyClose = TRUE,
          footer = tagList(
            actionButton("go_to_country_data", "View More Details", 
                         class = "btn-primary", 
                         onclick = paste0("Shiny.onInputChange('go_to_country_data_click', '", country_name, "');")),
            
            tags$a(
              href = "https://data.who.int/dashboards/covid19/data?n=c",
              style = "color: #007BFF; text-decoration: none;",
              "Explore the data yourself on the WHO Website."
            )
          )
        )
      )
    }
  })
  
  # Navigate to "Country Data" tab and update selected country
  observeEvent(input$go_to_country_data_click, {
    removeModal()
    updateTabsetPanel(session, "tabs", selected = "Country Data")
    updateSelectInput(session, "selected_country", selected = input$go_to_country_data_click)
  })
  
  # Render bar plot for top 20 countries by deaths per 100k
  output$barplot <- renderPlot({
    # Filter top 20 countries by deaths per 100k
    top_20_data <- map_data %>%
      slice_max(order_by = `Deaths...cumulative.total.per.100000.population`, n = 30)
    
    # Create the plot
    ggplot(top_20_data, aes(
      x = reorder(name_long, `Deaths...cumulative.total.per.100000.population`), 
      y = `Deaths...cumulative.total.per.100000.population`, 
      fill = `Deaths...cumulative.total.per.100000.population`)) +
      geom_bar(stat = "identity") +
      scale_fill_fermenter(palette = "YlOrRd", direction = 1) +
      labs(
        title = "Countries with the Highest COVID-19 Death Rates (Per 100k Population)",
        x = "Country (name_long)",
        y = "Deaths per 100k",
        fill = "Deaths per 100k"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  # Vaccination progress plot
  output$vaccination_plot <- renderPlot({
    # Prozentsatz berechnen und filtern
    filtered_data <- map_data %>%
      mutate(
        percent_vaccinated = pmin(PERSONS_VACCINATED_1PLUS_DOSE / pop_est * 100, 100)
      ) %>%
      filter(percent_vaccinated <= input$max_percent) %>% # Filtern nach Maximalwert
      slice_max(order_by = percent_vaccinated, n = input$num_countries) # Sortieren nach Prozent
    
    # Diagramm
    ggplot(filtered_data, aes(
      x = reorder(name_long, percent_vaccinated), 
      y = percent_vaccinated, 
      fill = percent_vaccinated)) +
      geom_bar(stat = "identity") +
      scale_fill_fermenter(palette = "Blues", direction = 1) +
      labs(
        title = "Vaccination Progress (Filtered by Percentage)",
        x = "Country",
        y = "Vaccinated (% of Population)",
        fill = "Vaccinated (%)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$country_info <- renderUI({
    req(input$selected_country)
    country_data <- map_data %>%
      filter(name_long == input$selected_country)
    
    if (nrow(country_data) == 0) {
      return(div("No data available for the selected country."))
    }
    
    population <- format(country_data$pop_est, big.mark = ",")
    income_group <- country_data$income_grp
    cases_total <- format(country_data$`Cases...cumulative.total`, big.mark = ",")
    deaths_total <- format(country_data$`Deaths...cumulative.total`, big.mark = ",")
    cases_per_100k <- round(country_data$`Cases...cumulative.total.per.100000.population`, 2)
    deaths_per_100k <- round(country_data$`Deaths...cumulative.total.per.100000.population`, 2)
    vaccinated_1_dose <- format(country_data$PERSONS_VACCINATED_1PLUS_DOSE, big.mark = ",")
    vaccinated_full <- format(country_data$PERSONS_LAST_DOSE, big.mark = ",")
    vaccinated_booster <- format(country_data$PERSONS_BOOSTER_ADD_DOSE, big.mark = ",")
    
    div(
      style = "background-color: #f9f9f9; border-radius: 10px; padding: 20px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);",
      h3(style = "text-align: center; color: #007BFF;", paste("COVID-19 Data for", input$selected_country)),
      div(
        style = "display: flex; flex-wrap: wrap; gap: 20px; justify-content: space-between;",
        
        div(
          style = "flex: 1 1 calc(33.33% - 20px); background-color: #dfe7fd; border-radius: 8px; padding: 15px; text-align: center;",
          tags$i(class = "fa fa-users fa-2x", style = "color: #5a8dee;"),
          h4("Population"),
          p(style = "font-size: 18px; font-weight: bold;", population)
        ),
        
        div(
          style = "flex: 1 1 calc(33.33% - 20px); background-color: #fdded3; border-radius: 8px; padding: 15px; text-align: center;",
          tags$i(class = "fa fa-money-bill-alt fa-2x", style = "color: #e55353;"),
          h4("Income Group"),
          p(style = "font-size: 18px; font-weight: bold;", income_group)
        ),
        
        div(
          style = "flex: 1 1 calc(33.33% - 20px); background-color: #d4edda; border-radius: 8px; padding: 15px; text-align: center;",
          tags$i(class = "fa fa-syringe fa-2x", style = "color: #28a745;"),
          h4("Vaccinated (1+ Dose)"),
          p(style = "font-size: 18px; font-weight: bold;", vaccinated_1_dose)
        ),
        
        div(
          style = "flex: 1 1 calc(33.33% - 20px); background-color: #f8d7da; border-radius: 8px; padding: 15px; text-align: center;",
          tags$i(class = "fa fa-virus fa-2x", style = "color: #dc3545;"),
          h4("Total Cases"),
          p(style = "font-size: 18px; font-weight: bold;", cases_total)
        ),
        
        div(
          style = "flex: 1 1 calc(33.33% - 20px); background-color: #fff3cd; border-radius: 8px; padding: 15px; text-align: center;",
          tags$i(class = "fa fa-procedures fa-2x", style = "color: #ffc107;"),
          h4("Deaths per 100k"),
          p(style = "font-size: 18px; font-weight: bold;", deaths_per_100k)
        ),
        
        div(
          style = "flex: 1 1 calc(33.33% - 20px); background-color: #cce5ff; border-radius: 8px; padding: 15px; text-align: center;",
          tags$i(class = "fa fa-thermometer-three-quarters fa-2x", style = "color: #007BFF;"),
          h4("Cases per 100k"),
          p(style = "font-size: 18px; font-weight: bold;", cases_per_100k)
        )
      ),
      div(
        style = "margin-top: 20px; text-align: center;",
        p("Official numbers reported by ", input$selected_country, 
          " officials / WHO as of 01.01.2025.")
      )
    )
  })
  
  # Comparative Plots
  output$cases_comparison_plot <- renderPlot({
    req(input$selected_country)
    ggplot(map_data, aes(x = reorder(name_long, `Cases...cumulative.total.per.100000.population`), 
                         y = `Cases...cumulative.total.per.100000.population`, 
                         fill = name_long == input$selected_country)) +
      geom_bar(stat = "identity", show.legend = FALSE, alpha = 0.9) +
      scale_fill_manual(values = c("FALSE" = "#d3d3d3", "TRUE" = "#4682b4")) +
      labs(
        x = NULL,  # Remove x-axis label for a cleaner look
        y = "Cases per 100k"
      ) +
      theme_minimal(base_size = 15) +
      theme(
        axis.title.y = element_text(face = "bold", color = "#555555"),
        axis.text.y = element_text(size = 10, face = "italic"),
        axis.text.x = element_blank(),  # Hide x-axis text for simplicity
        axis.ticks.x = element_blank(),  # Hide x-axis ticks
        panel.grid.major.y = element_line(color = "#e6e6e6", size = 0.5)
      )
  })
  
  output$deaths_comparison_plot <- renderPlot({
    req(input$selected_country)
    ggplot(map_data, aes(x = reorder(name_long, `Deaths...cumulative.total.per.100000.population`), 
                         y = `Deaths...cumulative.total.per.100000.population`, 
                         fill = name_long == input$selected_country)) +
      geom_bar(stat = "identity", show.legend = FALSE, alpha = 0.9) +
      scale_fill_manual(values = c("FALSE" = "#f0a5a5", "TRUE" = "#e74c3c")) +
      labs(
        x = NULL,  # Remove x-axis label for a cleaner look
        y = "Deaths per 100k"
      ) +
      theme_minimal(base_size = 15) +
      theme(
        axis.title.y = element_text(face = "bold", color = "#555555"),
        axis.text.y = element_text(size = 10, face = "italic"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(color = "#f2dede", size = 0.5)
      )
  })
  
  
  output$vaccination_comparison_plot <- renderPlot({
    req(input$selected_country)
    map_data <- map_data %>%
      mutate(percent_vaccinated = pmin(PERSONS_VACCINATED_1PLUS_DOSE / pop_est * 100, 100))
    
    ggplot(map_data, aes(x = reorder(name_long, percent_vaccinated), 
                         y = percent_vaccinated, 
                         fill = name_long == input$selected_country)) +
      geom_bar(stat = "identity", show.legend = FALSE, alpha = 0.9) +
      scale_fill_manual(values = c("FALSE" = "#c8e6c9", "TRUE" = "#4caf50")) +
      labs(
        x = NULL,  # Remove x-axis label for a cleaner look
        y = "Vaccinated (% of Population)"
      ) +
      theme_minimal(base_size = 15) +
      theme(
        axis.title.y = element_text(face = "bold", color = "#555555"),
        axis.text.y = element_text(size = 10, face = "italic"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.y = element_line(color = "#dff0d8", size = 0.5)
      )
  })
}

shinyApp(ui, server)
