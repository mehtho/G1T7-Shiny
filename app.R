pacman::p_load(shiny, tidyverse, tmap, sf, smoothr, SpatialAcc, hash, cowplot)

name2file <- new.env(hash=T, parent=emptyenv())

name2file[["Markets & Food Centres"]] <- "markets_and_food_centres.rds"
name2file[["Supermarkets"]] <- "supermarkets.rds"

name2file[["MRT Stations"]] <- "mrt.rds"
name2file[["ATMs"]] <- "poi_atm.rds"
name2file[["Banks"]] <- "poi_bank.rds"
name2file[["Beauty Salons"]] <- "poi_beauty_salon.rds"
name2file[["Cafes"]] <- "poi_cafe.rds"
name2file[["Clothing Stores"]] <- "poi_clothing_store.rds"
name2file[["Convenience Stores"]] <- "poi_convenience_store.rds"
name2file[["Dentists"]] <- "poi_dentist.rds"
name2file[["Doctors"]] <- "poi_doctor.rds"
name2file[["Gyms"]] <- "poi_gym.rds"
name2file[["Libraries"]] <- "poi_library.rds"
name2file[["Lodging"]] <- "poi_lodging.rds"
name2file[["Places of Worship"]] <- "poi_place_of_worship.rds"
name2file[["Restaurants"]] <- "poi_restaurant.rds"
name2file[["Schools"]] <- "poi_school.rds"
name2file[["Tourist Attractions"]] <- "poi_tourist_attraction.rds"


name2file[["Bus Stops"]] <- "osm_sg.rds"

location_options <- c(c("Markets & Food Centres", "Supermarkets"), sort(c("MRT Stations", "ATMs", "Banks", "Beauty Salons", "Cafes", "Clothing Stores", 
                                                                          "Convenience Stores", "Dentists", "Doctors", "Gyms",
                                                                            "Libraries", "Lodging", "Places of Worship", "Restaurants",
                                                                          "Schools", "Tourist Attractions", "Bus Stops")))

plot_acc <- function(method, quantiles, grid_size, point_type, exponent, subz, useHex) {
  if(useHex) {
    grid <- read_rds(paste('data/rds/grid_', grid_size, '_hexagon', ifelse(subz, '_sz', '_pa'),'.rds', sep=""))
  }
  else {
    grid <- read_rds(paste('data/rds/grid_', grid_size, '_square', ifelse(subz, '_sz', '_pa'),'.rds', sep=""))
  }
  
  points <- read_rds(paste('data/rds/', name2file[[point_type]], sep="")) %>%
    mutate(capacity = 500)
  
  centroid.coords <- st_coordinates(st_centroid(grid))
  points.coords <- st_coordinates(points)
  
  dm <- exp(distance(centroid.coords, points.coords, type = "euclidean") / 1000 * exponent)
  
  acc <- data.frame(ac(grid$demand,
                       points$capacity,
                       dm, 
                       d0 = 250,
                       power = 2, 
                       family = method))
  
  colnames(acc) <- "acc"
  hexagon <- bind_cols(grid, as_tibble(acc))
  hexagon$acc[is.infinite(hexagon$acc)] <- NA
  
  mapex <- st_bbox(grid)
  
  tm <- tm_shape(grid) + 
    tm_polygons() +
    tm_shape(hexagon,
             bbox = mapex) + 
    tm_fill(col = "acc",
            n = quantiles,
            style = "quantile",
            border.col = "black",
            border.lwd = 1,
            na.rm = TRUE) +
    tm_shape(points) +
    tm_symbols(size = 0.1) +
    tm_layout(main.title = paste("Accessibility to ", point_type, ": ", method," method", sep=""),
              main.title.position = "center",
              main.title.size = 1,
              legend.outside = FALSE,
              legend.height = 0.5, 
              legend.width = 0.5,
              legend.format = list(digits = 3),
              legend.position = c("right", "top"),
              frame = FALSE) +
    tm_compass(type="8star", size = 2) +
    tm_scale_bar(width = 0.20) +
    tm_grid(lwd = 0.1, alpha = 0.5)
  
  hexagon_acc <- st_join(hexagon, read_rds('data/rds/mpsz.rds') , join = st_intersects)
  
  region_bxp <- ggplot(data=hexagon_acc, 
                       aes(y = acc, 
                           x = REGION_N)) +
    geom_boxplot(outliers = FALSE) +
    geom_point(stat="summary", 
               fun.y="mean", 
               colour ="red", 
               size=2)
  
  plot_grid(tmap_grob(tm), region_bxp, nrow = 2, rel_heights = c(2, 1))
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Accessibility of ..."),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("family", label = "Accessibility Modelling Family:",
                      choices = c("Hansen", "KD2SFCA", "SAM"),
                      selected = "250"),
          sliderInput("quantiles", label = "Quantiles:",
                      min = 4, max = 10, value = 6),
          selectInput("gridSize", label = "Grid Size:",
                      choices = c("250", "500", "1000"),
                      selected = "250"),
          selectInput("poiType", label = "Place of Interest:",
                      choices = location_options,
                      selected = "supermarkets"),
          sliderInput("exponent", label = "Distance Exponent:",
                      min = 1.5, max = 3, value = 2, step = 0.25),
          radioButtons("granularity", label = "Subzone or Planning Area Population:",
                       choices = c("Subzone", "Planning Area"),
                       selected = "Subzone"),
          radioButtons("gridShape", label = "Grid Shape:",
                       choices = c("Hexagon", "Square"),
                       selected = "Hexagon")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("accPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$accPlot <- renderPlot({
      plot_acc(input$family, input$quantiles, input$gridSize, input$poiType, input$exponent, input$granularity == "Subzone", input$gridShape == "Hexagon")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
