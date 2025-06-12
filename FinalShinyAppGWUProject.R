# Load libraries
library(shiny)
library(tidyverse)
library(sf)
library(tmap)
library(tmaptools)
library(tigris)
library(ggplot2)
library(broom)

# Set tigris options
options(tigris_use_cache = TRUE)

# Enable OpenStreetMap as basemap
tmap_options(basemaps = c(OpenStreetMap = "OpenStreetMap"))

# Load spatial data
tracts_dc <- tracts(state = "DC", year = 2020, class = "sf") %>%
  st_transform(4326)

# Load BEI data
bei <- read_csv("C:/Users/joshu/OneDrive/Desktop/V2 GWU Project/BEI Final with Equal Weighted Averages.csv")

# Load EJScreen data
ejs <- read_csv("C:/Users/joshu/OneDrive/Desktop/V2 GWU Project/EJScreen_2024_Tract_with_AS_CNMI_GU_VI.csv")

# Filter EJScreen to DC
ejs_dc <- ejs %>%
  filter(str_starts(ID, "11")) %>%
  select(GEOID = ID,
         asthma_index = P_DEMOGIDX_2,
         diabetes_index = P_DEMOGIDX_5)

# Clean BEI
bei <- bei %>%
  mutate(GEOID = as.character(`Census ID`)) %>%
  select(GEOID, starts_with("Driver"), Overall_Driver_Avg)

# Merge all data
tract_data <- tracts_dc %>%
  left_join(bei, by = "GEOID") %>%
  left_join(ejs_dc, by = "GEOID")

# Descriptive driver names
driver_labels <- c(
  "Driver1_Avg" = "Education Access",
  "Driver2_Avg" = "Employment Access",
  "Driver3_Avg" = "Income Access",
  "Driver4_Avg" = "Housing Conditions",
  "Driver5_Avg" = "Transportation Access",
  "Driver6_Avg" = "Food Environment",
  "Driver7_Avg" = "Environmental Hazards",
  "Driver8_Avg" = "Healthcare Access",
  "Driver9_Avg" = "Community Safety",
  "Overall_Driver_Avg" = "Overall Built Environment Index"
)

health_labels <- c(
  "asthma_index" = "Asthma Index",
  "diabetes_index" = "Diabetes Index"
)

tmap_mode("view")

# UI
ui <- fluidPage(
  titlePanel("DC Census Tracts: Built Environment Drivers & Health Outcomes"),
  sidebarLayout(
    sidebarPanel(
      selectInput("driver", "Select Driver:",
                  choices = setNames(names(driver_labels), driver_labels)),
      selectInput("health", "Select Health Index:",
                  choices = setNames(names(health_labels), health_labels))
    ),
    mainPanel(
      tmapOutput("map", height = "800px"),
      hr(),
      h3("Correlation Summary: Drivers vs. Health Indices"),
      plotOutput("correlation_plot", height = "400px"),
      hr(),
      h4("Correlation Coefficients Table"),
      tableOutput("correlation_table")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$map <- renderTmap({
    drv <- input$driver
    health <- input$health
    
    driver_title <- driver_labels[drv]
    health_title <- health_labels[health]
    full_title <- paste(driver_title, "and", health_title)
    
    palette <- ifelse(health == "asthma_index", "Reds", "Greens")
    
    tm_shape(tract_data) +
      tm_polygons(
        col = drv,
        palette = "Blues",
        title = driver_title,
        border.alpha = 0.3,
        alpha = 0.7,
        id = "GEOID",
        na.value = "#800080",
        legend.show.na = TRUE,
        legend.na.text = "Missing Driver Score",
        popup.vars = setNames(
          c("GEOID", drv),
          c("Census Tract", driver_title)
        )
      ) +
      tm_bubbles(
        col = health,
        size = 0.6,
        shape = 21,
        palette = palette,
        border.col = "black",
        border.alpha = 0.6,
        alpha = 0.85,
        title.col = health_title,
        popup.vars = setNames(health, health_title),
        na.value = "#800080",
        legend.show.na = TRUE,
        legend.na.text = "Missing Health Index"
      ) +
      tm_layout(
        title = full_title,
        title.size = 1.3,
        legend.outside = TRUE,
        legend.outside.position = "right",
        frame = FALSE,
        outer.margins = 0
      )
  })
  
  # Compute correlation data once, use for both chart and table
  correlation_results <- reactive({
    driver_cols <- names(driver_labels)
    results <- data.frame()
    
    for (drv in driver_cols) {
      for (health in c("asthma_index", "diabetes_index")) {
        df <- tract_data %>%
          select(drv_col = all_of(drv), health_col = all_of(health)) %>%
          drop_na()
        
        if (nrow(df) > 2) {
          corr <- cor(-df$drv_col, df$health_col)
          results <- rbind(results, data.frame(
            Driver = driver_labels[drv],
            Health = health_labels[health],
            Correlation = round(corr, 3)
          ))
        }
      }
    }
    
    results
  })
  
  output$correlation_plot <- renderPlot({
    results <- correlation_results()
    
    ggplot(results, aes(x = Driver, y = Correlation, fill = Health)) +
      geom_col(position = "dodge") +
      coord_flip() +
      theme_minimal() +
      labs(title = "Correlation between Drivers and Health Indices",
           y = "Pearson Correlation Coefficient", x = "") +
      scale_fill_manual(values = c("Asthma Index" = "red", "Diabetes Index" = "green")) +
      theme(legend.position = "bottom", text = element_text(size = 12))
  })
  
  output$correlation_table <- renderTable({
    correlation_results()
  })
}

# Run the app
shinyApp(ui, server)
