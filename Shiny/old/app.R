library(shiny)
library(leaflet)
library(maps)
library(mapproj)
library(geojsonio)
library(stringr)
library(htmltools)
library(parallel)

#block_msa <- block_msa[block_msa$`CBSA Code`!=46300,]
test_geo = readRDS("geo.rds")
test_utils = readRDS("utilities.rds")
utilities <- test_utils

source("runUtilityModel.R")
matched = readRDS("matched_all_premodel.rds")
block_msa = readRDS("block_msa_den.rds")
block_msa <- block_msa[block_msa$`CBSA Code`!=46300,]
puma_density = readRDS("puma_den.rds")
colnames(block_msa)[c(1,6)] <- c('cbsa', 'msa_density')

block_msa[block_msa$cbsa==39150,'cbsa'] <- 39140
block_msa[block_msa$cbsa==19430,'cbsa'] <- 19380

matched$h_geocode <- str_pad(matched$h_geocode, width = 15, pad =0)
matched_msa = merge(matched, block_msa, by.x='h_geocode', by.y='block1', all.x = TRUE)
saveRDS(matched_msa, "matched_msa.rds")
matched <- matched_msa
#matched = matched_msa
#matched = matched[1:30000,]

pumas = geojson_read("puma_shapes.geojson", what = "sp")
states = geojson_read("state_shapes.geojson", what = "sp")
msa = geojson_read("msa_shapes.geojson", what = "sp")
#pumas_s0601 = readRDS("C:/users/zae5o/desktop/stl/toyota av/data/s0601_puma.rds")
#states_s0601 = readRDS("C:/users/zae5o/desktop/stl/toyota av/data/s0601_state.rds")
fips_codes = list(
  "All States WARNING VERY SLOW" = "00",
  "Alabama" = "01",
  "Arkansas" = "05",
  "Arizona" = "04",
  "California" = "06",
  "Colorado" = "08",
  "Connecticut" = "09",
  "Washington DC" = "11",
  "Delaware" = "10",
  "Florida" = "12",
  "Georgia" = "13",
  "Iowa" = "19",
  "Idaho" = "16",
  "Illinois" = "17",
  "Indiana" = "18",
  "Kansas" = "20",
  "Kentucky" = "21",
  "Louisiana" = "22",
  "Massachusetts" = "25",
  "Maryland" = "24",
  "Maine" = "23",
  "Michigan" = "26",
  "Minnesota" = "27",
  "Missouri" = "29",
  "Mississippi" = "28",
  "Montana" = "30",
  "North Carolina" = "37",
  "North Dakota" = "38",
  "Nebraska" = "31",
  "New Hampshire" = "33",
  "New Jersey" = "34",
  "New Mexico" = "35",
  "Nevada" = "32",
  "New York" = "36",
  "Ohio" = "39",
  "Oklahoma" = "40",
  "Oregon" = "41",
  "Pennsylvania" = "42",
  "Rhode Island" = "44",
  "South Carolina" = "45",
  "South Dakota" = "46",
  "Tennessee" = "47",
  "Texas" = "48",
  "Utah" = "49",
  "Virginia" = "51",
  "Vermont" = "50",
  "Washington" = "53",
  "Wisconsin" = "55",
  "West Virginia" = "54",
  "Wyoming" = "56"
)

ui = fluidPage(
  titlePanel("Potential US AV Marketshare"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("aggregate_level", "Aggregation Level", choices = list("State" = 1,"PUMA" = 2, "MSA" = 3), selected = 1),
      selectInput("fips", "State for PUMA Evaluation", choices = fips_codes, selected = "01"),
      checkboxGroupInput("service_types", "Service Types", choices = list("Solo" = 1,"Pooled" = 2,"Solo+Transit" = 3), selected = c(1,2,3)),
      numericInput("service_wait_time", "Service Waiting Time (mins)", value = 5),
      numericInput("transit_wait_time", "Transit Waiting Time (mins)", value = 5),
      numericInput("service_price_srs_base", "Solo Service Base Price ($)", value = 5.50),
      numericInput("service_price_srs", "Solo Service Price ($/mile)", value = 1.31),
      numericInput("service_price_prs_base", "Pooled Service Base Price ($)", value = 4.75),
      numericInput("service_price_prs", "Pooled Service Price ($/mile)", value = 0.87),
      numericInput("cost_gallon", "Gas Price ($/gallon)", value = 2.79),
      numericInput("mpg", "Fuel Efficiency (mpg)", value = 25),
      actionButton("update_button", "Run Model")
    ),
    mainPanel(
      leafletOutput("mymap")
    )
  )
)

server = function(input, output) {
  #This object updates only when the Run Model button is pressed. Its output is a dataframe with the model results
  matched_reactive = eventReactive(input$update_button, {
    cat("Calculating Utilities...\n")
    car_utility = mcmapply(runUtilityModel, "car", matched$drvtime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, input$service_price_srs, input$service_price_srs_base, input$service_price_prs, input$service_price_prs_base, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, input$service_wait_time, input$transit_wait_time, input$mpg, input$cost_gallon)
    cat("car done..\n")
    transit_utility = mcmapply(runUtilityModel, "transit", matched$trstime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, input$service_price_srs, input$service_price_srs_base, input$service_price_prs, input$service_price_prs_base, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, input$service_wait_time, input$transit_wait_time, input$mpg, input$cost_gallon)
    cat("transit done..\n")
    transit_rs_utility = mcmapply(runUtilityModel, "transit_rs", matched$trstime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, input$service_price_srs, input$service_price_srs_base, input$service_price_prs, input$service_price_prs_base, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, input$service_wait_time, input$transit_wait_time, input$mpg, input$cost_gallon)
    cat("trs done..\n")
    solo_rs_utility = mcmapply(runUtilityModel, "solo_rs", matched$drvtime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, input$service_price_srs, input$service_price_srs_base, input$service_price_prs, input$service_price_prs_base, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, input$service_wait_time, input$transit_wait_time, input$mpg, input$cost_gallon)
    cat("solo done..\n")
    pooled_rs_utility = mcmapply(runUtilityModel, "pooled_rs", matched$drvtime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, input$service_price_srs, input$service_price_srs_base, input$service_price_prs, input$service_price_prs_base, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, input$service_wait_time, input$transit_wait_time, input$mpg, input$cost_gallon)
    cat("pooled done..\n")
    walk_utility = mcmapply(runUtilityModel, "walk", matched$wlktime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, input$service_price_srs, input$service_price_srs_base, input$service_price_prs, input$service_price_prs_base, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, input$service_wait_time, input$transit_wait_time, input$mpg, input$cost_gallon)
    cat("walk done..\n")
    bike_utility = mcmapply(runUtilityModel, "bike", matched$biktime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, input$service_price_srs, input$service_price_srs_base, input$service_price_prs, input$service_price_prs_base, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, input$service_wait_time, input$transit_wait_time, input$mpg, input$cost_gallon)
    cat("bike done..\n")
    cat("Finished Utility Calculation\n")
    utilities = data.frame(matched, car_utility, transit_utility, transit_rs_utility, solo_rs_utility, pooled_rs_utility, walk_utility, bike_utility)
    
    #This section turns the utilities from the model into probabilities of taking each mode
    cat("Calculating Mode Choice Probabilities...\n")
    utilities[utilities$mode == 1,]$transit_utility = -Inf
    utilities[utilities$mode != 1,]$car_utility = -Inf
    utilities$Pcar <- exp(utilities$car_utility)/rowSums(exp(utilities[,38:44]))
    utilities$Ptransit <- exp(utilities$transit_utility)/rowSums(exp(utilities[,38:44]))
    utilities$Ptransit_rs <- exp(utilities$transit_rs_utility)/rowSums(exp(utilities[,38:44]))
    utilities$Psolo_rs <- exp(utilities$solo_rs_utility)/rowSums(exp(utilities[,38:44]))
    utilities$Ppooled_rs <- exp(utilities$pooled_rs_utility)/rowSums(exp(utilities[,38:44]))
    utilities$Pwalk <- exp(utilities$walk_utility)/rowSums(exp(utilities[,38:44]))
    utilities$Pbike <- exp(utilities$bike_utility)/rowSums(exp(utilities[,38:44]))
    utilities$Pservice = 0
    cat("Finished Mode Choice Probabilities\n")
    saveRDS(utilities, "utilities.rds")
    return(utilities)
  }, ignoreNULL = TRUE)
  
  #This object updates automatically whenever a new aggregation level is selected, or when the utility object is updated
  aggregated_reactive = eventReactive(input$update_button, {
    if (input$update_button == 0) {
      return()
    }
    input$update_button
    utilities = matched_reactive()
    #Sum the probabilities for the service types that are selected
    if (1 %in% input$service_types) {
      utilities$Pservice = rowSums(data.frame(utilities$Pservice, utilities$Psolo_rs))
    }
    if (2 %in% input$service_types) {
      utilities$Pservice = rowSums(data.frame(utilities$Pservice, utilities$Ppooled_rs))
    }
    if (3 %in% input$service_types) {
      utilities$Pservice = rowSums(data.frame(utilities$Pservice, utilities$Ptransit_rs))
    }
    cat('line 169')
  
    #Aggregate the utilities to the geography level by setting the appropriate shapefile to geo variable
    if (input$aggregate_level == 2) {
      geo = pumas
      geo$NAME = geo$NAMELSAD10
      #Subset the sp to be only the selected state, as long as 'all states' is not selected
      if (input$fips != "00") {
        geo = geo[geo@data$STATEFP10 == input$fips,]
      }
      utilities$sample_geo = str_pad(utilities$sample_geo, 7, pad = 0)
      utilities$state_id = substring(utilities$sample_geo, 1, 2)
      agg_utils = aggregate(utilities$Pservice, by = list(utilities$sample_geo), FUN = mean)
      names(agg_utils) = c("puma_id","mean_probability")
      puma_order = data.frame(as.numeric(geo$GEOID10))
      names(puma_order) = c("puma_id")
      puma_order$puma_id = str_pad(puma_order$puma_id, 7, pad = 0) #5
      geo$PROB = merge(puma_order, agg_utils, by = "puma_id", all.x = TRUE, sort = FALSE)$mean_probability
    } else if (input$aggregate_level == 3) {
      geo = msa
      #utilities$sample_geo = str_pad(utilities$sample_geo, 7, pad = 0)
      #utilities$state_id = substring(utilities$sample_geo, 1, 2)
      utilities <- na.omit(utilities)
      cat("Aggregating msa utilities...\n")
      saveRDS(utilities, "umsa.rds")
      agg_utils = aggregate(utilities$Pservice, by = list(utilities$cbsa), FUN = mean)
      agg_utils = data.frame(agg_utils)
      names(agg_utils) = c("msa_id","mean_probability")
      saveRDS(agg_utils,'msa_agg_u.rds')
      msa_order = data.frame(as.numeric(geo$CBSAFP))
      names(msa_order) = c("msa_id")
      geo$PROB = merge(msa_order, agg_utils, by = "msa_id", all.x = TRUE, sort = FALSE)$mean_probability
    } else {
      geo = states
      utilities$sample_geo = str_pad(utilities$sample_geo, 7, pad = 0)
      utilities$state_id = substring(utilities$sample_geo, 1, 2)
      agg_utils = aggregate(utilities$Pservice, by = list(utilities$state_id), FUN = mean)
      agg_utils = data.frame(agg_utils)
      names(agg_utils) = c("state_id","mean_probability")
      state_order = data.frame(as.numeric(geo$STATEFP))
      names(state_order) = c("state_id")
      state_order$state_id = str_pad(state_order$state_id, 2, pad = 0)
      geo$PROB = merge(state_order, agg_utils, by = "state_id", all.x = TRUE, sort = FALSE)$mean_probability
    }
    cat("Finished Aggregation\n")
    saveRDS(geo, "geo.rds")
    return(geo)
  }, ignoreNULL = TRUE)
  
  output$mymap = renderLeaflet({
    if (input$update_button == 0) {
      return()
    }
    labels <- sprintf(
      "<strong>%s</strong><br/>%g Market share<br/>",
      aggregated_reactive()$NAME, aggregated_reactive()$PROB
    ) %>% lapply(htmltools::HTML)
    pal = colorBin("YlOrRd", domain = aggregated_reactive()$PROB)
    #pal = colorNumeric("Blues", domain = aggregated_reactive()$PROB)
    cat("Generating Map...\n")
    leaflet(aggregated_reactive()) %>%
    setView(lat = 38.921279, lng = -91.519416, zoom = 3) %>% 
    addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE)) %>%
    addPolygons(
      fillColor = ~pal(aggregated_reactive()$PROB),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")
    ) %>%
    addLegend(pal = pal, values = ~aggregated_reactive()$PROB, opacity= 0.7, title = NULL, position = "bottomright",
              labFormat = labelFormat(suffix = "%", 
                transform = function(x) 100 * x
              ))
  })
}

shinyApp(ui = ui, server = server)

