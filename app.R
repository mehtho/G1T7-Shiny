pacman::p_load(shiny, spatstat, raster, tidyverse, tmap, sf, smoothr, SpatialAcc, hash, cowplot)

name2file <- new.env(hash=T, parent=emptyenv())

name2file[["Markets or Food Centres"]] <- "markets_and_food_centres.rds"

name2file[["Markets"]] <- "markets_and_food_centres_MARKET.rds"
name2file[["Hawker Centres"]] <- "markets_and_food_centres_HAWKER_CENTRE.rds"
name2file[["Markets and Food Centres"]] <- "markets_and_food_centres_MARKET_AND_HAWKER.rds"

name2file[["Supermarkets"]] <- "supermarkets.rds"

name2file[["Supermarkets: Fairprice"]] <- "supermarkets_FAIRPRICE.rds"
name2file[["Supermarkets: Sheng Siong"]] <- "supermarkets_SHENG SIONG.rds"
name2file[["Supermarkets: Cold Storage"]] <- "supermarkets_COLD STORAGE.rds"
name2file[["Supermarkets: Others"]] <- "supermarkets_OTHER.rds"

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

location_options <- c(c("Markets or Food Centres", "Hawker Centres", "Markets and Food Centres"), 
                      c("Supermarkets", "Supermarkets: Fairprice", "Supermarkets: Sheng Siong", "Supermarkets: Cold Storage", "Supermarkets: Others"),
                      sort(c("MRT Stations", "ATMs", "Banks", "Beauty Salons", "Cafes", "Clothing Stores", 
                                                                          "Convenience Stores", "Dentists", "Doctors", "Gyms",
                                                                            "Libraries", "Lodging", "Places of Worship", "Restaurants",
                                                                          "Schools", "Tourist Attractions", "Bus Stops")))

tmap_color_palettes <- c("inferno", "magma", "plasma", "viridis", "YlGn", "YlGnBu", "GnBu", "BuGn", "PuBuGn", "PuBu", "BuPu", "RdPu", "PuRd", "YlOrRd")
                      
scales <- c("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "log10_pretty")

evenly_separated <- function(A, B) {
  seq <- seq(0, A, length.out = B)
  return(seq)
}

plot_kde <- function(bandwidth, bandwidthNum, kernel, point_type) {
  merged <- read_rds("data/rds/merged.rds")
  place <- read_rds(paste('data/rds/', name2file[[point_type]], sep="")) %>%
    mutate(capacity = 500)
  place_sp <- as_Spatial(place)
  place_sp <- as(place_sp, "SpatialPoints")
  place_ppp <- as(place_sp, "ppp")
  place_ppp_jit <- rjitter(place_ppp,
                           retry=TRUE,
                           nsim=1,
                           drop=TRUE)
  merged_owin <- read_rds("data/rds/merged_owin.rds")
  placeSG_ppp = place_ppp[merged_owin]
  placeSG_ppp.km <- rescale(placeSG_ppp, 1000, "km")
  if (bandwidth == "fixed") {
    KDE <- density(placeSG_ppp.km, 
                   sigma=bandwidthNum, 
                   edge=TRUE, 
                   kernel=kernel)
  } else if (bandwidth == "adaptive") {
    KDE <- adaptive.density(placeSG_ppp.km, 
                            method="kernel")
  } else if (bandwidth == "diggle") {
    KDE <- density(placeSG_ppp.km, 
                   sigma=bw.diggle, 
                   edge=TRUE, 
                   kernel=kernel)
  } else if (bandwidth == "cvl") {
    KDE <- density(placeSG_ppp.km, 
                   sigma=bw.CvL, 
                   edge=TRUE, 
                   kernel=kernel)
  } else if (bandwidth == "scott") {
    KDE <- density(placeSG_ppp.km, 
                   sigma=bw.scott, 
                   edge=TRUE, 
                   kernel=kernel)
  } else {
    KDE <- density(placeSG_ppp.km, 
                   sigma=bw.ppl, 
                   edge=TRUE, 
                   kernel=kernel)
  }
  gridded <- as.SpatialGridDataFrame.im(KDE)
  KDEraster <- raster(gridded)
  projection(KDEraster) <- CRS("+init=EPSG:3414 +units=km")
  tmap_mode("view")
  tm_basemap(server="OpenStreetMap.HOT") +
    tm_basemap(server = "Esri.WorldImagery") +
    tm_shape(KDEraster) +
    tm_raster("v",
              title = "Kernel Density",
              style = "pretty",
              alpha = 0.6,
              palette = c("#fafac3","#fd953b","#f02a75","#b62385","#021c9e")) +
    tm_shape(merged)+
    tm_polygons(alpha=0.1,
                id="PLN_AREA_N", 
                popup.vars=c(
                  "Subzone",
                  "Region",
                  "Total Population",
                  "9 and under",
                  "10-19",
                  "20-29",
                  "30-39",
                  "40-49",
                  "50-59",
                  "60-69",
                  "70 and above"
                ))+
    tmap_options(check.and.fix = TRUE)
}

plot_sppa <- function(subzone, whichFunction, point_type, nsim) {
  mpsz_sp <- read_rds("data/rds/mpsz_sp.rds")
  sz = mpsz_sp[mpsz_sp@data$SUBZONE_N == toupper(subzone),]
  sz_sp = as(sz, "SpatialPolygons")
  sz_owin = as(sz_sp, "owin")
  place <- read_rds(paste('data/rds/', name2file[[point_type]], sep="")) %>%
    mutate(capacity = 500)
  place_sp <- as_Spatial(place)
  place_sp <- as(place_sp, "SpatialPoints")
  place_ppp <- as(place_sp, "ppp")
  place_ppp_jit <- rjitter(place_ppp,
                           retry=TRUE,
                           nsim=1,
                           drop=TRUE)
  sz_ppp = place_ppp_jit[sz_owin]
  
  if (tolower(whichFunction) == "g") {
    plot(envelope(sz_ppp, Gest, correction="all", nsim=nsim))
  } else if (tolower(whichFunction) == "f") {
    plot(envelope(sz_ppp, Fest, correction="all", nsim=nsim))
  } else if (tolower(whichFunction) == "k") {
    K.csr <- envelope(sz_ppp, Kest, nsim=nsim, rank=1, glocal=TRUE)
    plot(K.csr, . - r ~ r, 
         xlab="d", ylab="K(d)-r", xlim=c(0,500))
  } else {
    L.csr <- envelope(sz_ppp, Lest, nsim=nsim, rank=1, glocal=TRUE)
    plot(L.csr, . - r ~ r, 
         xlab="d", ylab="L(d)-r", xlim=c(0,500))
  }
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Accessibility Modelling in Singapore"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("family", label = "Accessibility Modelling Family:",
                      choices = c("Hansen", "KD2SFCA", "SAM"),
                      selected = "250"),
          sliderInput("quantiles", label = "Breaks:",
                      min = 4, max = 10, value = 6),
          selectInput("gridSize", label = "Grid Size:",
                      choices = c("250", "500", "1000"),
                      selected = "250"),
          selectInput("poiType", label = "Place of Interest:",
                      choices = location_options,
                      selected = "Markets"),
          sliderInput("exponent", label = "Distance Exponent:",
                      min = 1, max = 3, value = 2, step = 0.25),
          radioButtons("granularity", label = "Subzone or Planning Area Population:",
                       choices = c("Subzone", "Planning Area"),
                       selected = "Subzone"),
          radioButtons("gridShape", label = "Grid Shape:",
                       choices = c("Hexagon", "Square"),
                       selected = "Hexagon"),
          selectInput("colorPal", label="Colour Palette",
                      choices=tmap_color_palettes,
                      selected="viridis"),
          selectInput("scale", label="Scaling:",
                      choices = scales,
                      selected="quantile"),
          numericInput("cap_m", "Capacity Multiplier:", value = 1, min = 1, max = 100),
          actionButton("generate_button", "Generate Plots"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("accPlot", height="700"),
          plotOutput("accBarPlot", height="200")
        )
    )
)

server <- function(input, output) {
  trigger <- reactiveVal(FALSE)
  
  plot_acc <- function(method, quantiles, grid_size, point_type, exponent, subz, useHex, colorPal, scale, cap_mult) {
    req(trigger())
    
    if(useHex) {
      grid <- read_rds(paste('data/rds/grid_', grid_size, '_hexagon', ifelse(subz, '_sz', '_pa'),'.rds', sep=""))
    }
    else {
      grid <- read_rds(paste('data/rds/grid_', grid_size, '_square', ifelse(subz, '_sz', '_pa'),'.rds', sep=""))
    }
    
    points <- read_rds(paste('data/rds/', name2file[[point_type]], sep=""))
    
    centroid.coords <- st_coordinates(st_centroid(grid))
    points.coords <- st_coordinates(points)
    
    points$capacity <- points$capacity * cap_mult
    
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
    
    if(is.na(max(hexagon$acc))) {
      bks <- evenly_separated(100, quantiles)
    }
    else{
      bks <- evenly_separated(max(hexagon$acc), quantiles)
    }
    
    tm <- tm_shape(grid) + 
      tm_polygons() +
      tm_shape(hexagon,
               bbox = mapex) + 
      tm_fill(col = "acc",
              n = quantiles,
              style = scale,
              breaks = bks,
              border.col = "black",
              border.lwd = 1,
              na.rm = TRUE,
              palette=colorPal) +
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
      labs(x = "REGION", y = "ACCESSIBILITY") + 
      geom_point(stat="summary", 
                 fun.y="mean", 
                 colour ="red", 
                 size=2)
    
    return(list(accP=tm, accBP=region_bxp))
  }
  
  
  output$accPlot <- renderPlot({
    plot_acc(input$family, 
             input$quantiles, 
             input$gridSize, 
             input$poiType, 
             input$exponent, 
             input$granularity == "Subzone", 
             input$gridShape == "Hexagon",
             input$colorPal,
             input$scale,
             input$cap_m)$accP
  })
  
  output$accBarPlot <- renderPlot({
    plot_acc(input$family, 
             input$quantiles, 
             input$gridSize, 
             input$poiType, 
             input$exponent, 
             input$granularity == "Subzone", 
             input$gridShape == "Hexagon",
             input$colorPal,
             input$scale,
             input$cap_m)$accBP
  })
  observeEvent(input$generate_button, {
    trigger(TRUE)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
