library(shiny)
library(leaflet)
library(maps)
library(mapproj)
library(geojsonio)
library(stringr)
library(htmltools)

#setwd("C:/users/zae5o/desktop/stl/toyota av/shiny")

source("runUtilityModel.R")

matched = readRDS("matched_den.rds")
#matched = matched[1:30000,]

pumas = geojson_read("puma_shapes.geojson", what = "sp")
states = geojson_read("state_shapes.geojson", what = "sp")
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
  titlePanel("Potential US Market for New Mobility Service"),
  sidebarLayout(
    sidebarPanel(
      numericInput("service_wait_time", "Service Waiting Time (mins)", value = 5),
      numericInput("transit_wait_time", "Transit Waiting Time (mins)", value = 5),
      numericInput("service_price_srs_base", "Solo Service Base Price ($)", value = 5.50),
      numericInput("service_price_srs", "Solo Service Price ($/mile)", value = 1.31),
      numericInput("service_price_prs_base", "Pooled Service Base Price ($)", value = 4.75),
      numericInput("service_price_prs", "Pooled Service Price ($/mile)", value = 0.87),
      numericInput("cost_gallon", "Gas Price ($/gallon)", value = 2.79),
      numericInput("mpg", "Fuel Efficiency (mpg)", value = 25),
      actionButton("save_results", "Save Results"),
      actionButton("update_button", "Run Model")
    ),
    mainPanel(
      leafletOutput("mymap"),
      column(6,
        selectInput("fips", "State for PUMA Evaluation", choices = fips_codes, selected = "01"),
        radioButtons("aggregate_level", "Aggregation Level", choices = list("State" = 1,"PUMA" = 2), selected = 2)
      ),
      column(6,
        checkboxGroupInput("service_types", "Service Types", choices = list("Solo" = 1,"Pooled" = 2,"Solo+Transit" = 3), selected = c(1,2,3))
      ),
      column(6,
        radioButtons("report_metric", "Report Metric", choices = list("Market share" = 1, "Trip density" = 2, "Accessibility" = 3), selected = 1)
      )
    )
  )
)

server = function(input, output, session) {
  
  #This object saves the individuals' utilities and information to a csv file when save results is pressed
  observeEvent(input$save_results, {
    cat("Saving results...\n")
    write.csv(matched_reactive(), "utils.csv", row.names = FALSE)
    cat("Results saved\n")
  })
  
  #If state is selected, disable the accessibility/density options (currently no population data)
  observeEvent(input$aggregate_level, {
    if (input$aggregate_level == 1) {
      updateRadioButtons(session, "report_metric", choices = list("Market share" = 1), selected = 1)
    } else {
      updateRadioButtons(session, "report_metric", choices = list("Market share" = 1, "Trip density" = 2, "Accessibility" = 3), selected = 1)
    }
  })
  
  #This object updates only when the Run Model button is pressed. Its output is a dataframe with the model results
  matched_reactive = eventReactive(input$update_button, {
    cat("Calculating Utilities...\n")
    car_utility = mapply(runUtilityModel, "car", matched$drvtime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, input$service_price_srs, input$service_price_srs_base, input$service_price_prs, input$service_price_prs_base, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, input$service_wait_time, input$transit_wait_time, input$mpg, input$cost_gallon)
    cat("car done..\n")
    transit_utility = mapply(runUtilityModel, "transit", matched$trstime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, input$service_price_srs, input$service_price_srs_base, input$service_price_prs, input$service_price_prs_base, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, input$service_wait_time, input$transit_wait_time, input$mpg, input$cost_gallon)
    cat("transit done..\n")
    transit_rs_utility = mapply(runUtilityModel, "transit_rs", matched$trstime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, input$service_price_srs, input$service_price_srs_base, input$service_price_prs, input$service_price_prs_base, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, input$service_wait_time, input$transit_wait_time, input$mpg, input$cost_gallon)
    cat("trs done..\n")
    solo_rs_utility = mapply(runUtilityModel, "solo_rs", matched$drvtime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, input$service_price_srs, input$service_price_srs_base, input$service_price_prs, input$service_price_prs_base, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, input$service_wait_time, input$transit_wait_time, input$mpg, input$cost_gallon)
    cat("solo done..\n")
    pooled_rs_utility = mapply(runUtilityModel, "pooled_rs", matched$drvtime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, input$service_price_srs, input$service_price_srs_base, input$service_price_prs, input$service_price_prs_base, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, input$service_wait_time, input$transit_wait_time, input$mpg, input$cost_gallon)
    cat("pooled done..\n")
    walk_utility = mapply(runUtilityModel, "walk", matched$wlktime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, input$service_price_srs, input$service_price_srs_base, input$service_price_prs, input$service_price_prs_base, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, input$service_wait_time, input$transit_wait_time, input$mpg, input$cost_gallon)
    cat("walk done..\n")
    bike_utility = mapply(runUtilityModel, "bike", matched$biktime, matched$drvDist, matched$hh_income/1000, matched$density, matched$age, matched$trsfare, input$service_price_srs, input$service_price_srs_base, input$service_price_prs, input$service_price_prs_base, matched$uber_tt_trs, matched$uber_tt_srs, matched$uber_tt_prs, matched$ridestrstime, matched$ridesDist, matched$parking_cost, input$service_wait_time, input$transit_wait_time, input$mpg, input$cost_gallon)
    cat("bike done..\n")
    cat("Finished Utility Calculation\n")
    utilities = data.frame(matched, car_utility, transit_utility, transit_rs_utility, solo_rs_utility, pooled_rs_utility, walk_utility, bike_utility)
    
    #This section turns the utilities from the model into probabilities of taking each mode
    cat("Calculating Mode Choice Probabilities...\n")
    utilities[utilities$mode == 1,]$transit_utility = -Inf
    utilities[utilities$mode != 1,]$car_utility = -Inf
    utilities$Pcar = exp(utilities$car_utility)/rowSums(exp(utilities[,40:46]))
    utilities$Ptransit = exp(utilities$transit_utility)/rowSums(exp(utilities[,40:46]))
    utilities$Ptransit_rs = exp(utilities$transit_rs_utility)/rowSums(exp(utilities[,40:46]))
    utilities$Psolo_rs = exp(utilities$solo_rs_utility)/rowSums(exp(utilities[,40:46]))
    utilities$Ppooled_rs = exp(utilities$pooled_rs_utility)/rowSums(exp(utilities[,40:46]))
    utilities$Pwalk = exp(utilities$walk_utility)/rowSums(exp(utilities[,40:46]))
    utilities$Pbike = exp(utilities$bike_utility)/rowSums(exp(utilities[,40:46]))
    utilities$Pservice = 0
    utilities$accessibility = log(rowSums(exp(utilities[,40:46])))
    cat("Finished Mode Choice Probabilities\n")
    return(utilities)
  }, ignoreNULL = TRUE)
  
  #This object updates automatically whenever a new aggregation level/view is selected, or when the utility object is updated
  aggregated_reactive = reactive({
    #Prevent this object from updating when the tool is first started
    if (input$update_button == 0) {
      return()
    }
    input$update_button
    input$report_metric
    utilities = matched_reactive()
    
    cat("Aggregating to selected level...\n")
    
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
    #Aggregate the utilities to the seleceted geography level by setting the appropriate shapefile to geo variable
    if (input$aggregate_level == 2) {
      geo = pumas
      geo$NAME = geo$NAMELSAD10
      #Subset the sp to be only the selected state, as long as 'all states' is not selected
      if (input$fips != "00") {
        geo = geo[geo@data$STATEFP10 == input$fips,]
      }
      utilities$sample_geo = str_pad(utilities$sample_geo, 7, pad = 0)
      utilities$state_id = substring(utilities$sample_geo, 1, 2)
      agg_utils = aggregate(utilities[,c(36:39,47,48,52,53,54,55)], by = list(utilities$sample_geo), FUN = mean)
      colnames(agg_utils)[1] = c("puma_id")
      agg_utils$trip_num = agg_utils$Pservice*agg_utils$puma_pop
      agg_utils$trip_den = agg_utils$Pservice*agg_utils$puma_density
      puma_order = data.frame(as.numeric(geo$GEOID10))
      names(puma_order) = c("puma_id")
      puma_order$puma_id = str_pad(puma_order$puma_id, 7, pad = 0)
      mergedprob = merge(puma_order, agg_utils, by = "puma_id", all.x = TRUE, sort = FALSE)
      geo$Pservice = mergedprob$Pservice
      geo$Pcar = mergedprob$Pcar
      geo$Ptransit = mergedprob$Ptransit
      geo$Pwalk = mergedprob$Pwalk
      geo$Pbike = mergedprob$Pbike
      geo$trip_num = mergedprob$trip_num
      geo$trip_den = mergedprob$trip_den
      geo$accessibility = mergedprob$accessibility
    } else {
      geo = states
      utilities$sample_geo = str_pad(utilities$sample_geo, 7, pad = 0)
      utilities$state_id = substring(utilities$sample_geo, 1, 2)
      agg_utils = aggregate(utilities[,c(36:39,47,48,52,53,54,55)], by = list(utilities$state_id), FUN = mean)
      colnames(agg_utils)[1] = c("state_id")
      state_order = data.frame(as.numeric(geo$STATEFP))
      names(state_order) = c("state_id")
      state_order$state_id = str_pad(state_order$state_id, 2, pad = 0)
      mergedprob = merge(state_order, agg_utils, by = "state_id", all.x = TRUE, sort = FALSE)
      geo$Pservice = mergedprob$Pservice
      geo$Pcar = mergedprob$Pcar
      geo$Ptransit =mergedprob$Ptransit
      geo$Pwalk = mergedprob$Pwalk
      geo$Pbike = mergedprob$Pbike
      geo$trip_num = NA
      geo$trip_den = NA
      geo$accessibility = NA
    }
    #Set the reported metric according to user input
    if (input$report_metric == 1 || input$aggregate_level == 1) {
      geo$report = geo$Pservice
    } else if (input$report_metric == 2) {
      geo$report = geo$trip_den
    } else if (input$report_metric == 3) {
      geo$report = geo$accessibility
    }
    cat("Finished Aggregation\n")
    return(geo)
  })
  
  output$mymap = renderLeaflet({
    if (input$update_button == 0) {
      return()
    }
    cat("Generating Map...\n")
    if (input$report_metric == 1) {
      label_report = sprintf("%g%% Market share: %s", round(aggregated_reactive()$report*100, 1),aggregated_reactive()$NAME)
      groupname = "Market share"
      legendformat = labelFormat(suffix = "%", transform = function(x) 100 * x)
    } else if (input$report_metric == 2) {
      label_report = sprintf("%g trips/sqmi: %s", round(aggregated_reactive()$report, 1),aggregated_reactive()$NAME)
      groupname = "Trip density"
      legendformat = labelFormat()
    } else if (input$report_metric == 3) {
      label_report = sprintf("%g Logsum Accessibility: %s", round(aggregated_reactive()$report, 1),aggregated_reactive()$NAME)
      groupname = "Accessibility"
      legendformat = labelFormat()
    } else {
      return()
    } %>%
    lapply(htmltools::HTML)
    pal_report = colorNumeric("YlOrRd", domain = aggregated_reactive()$report)
    cat("Generating Map...\n")
    leaflet(aggregated_reactive()) %>%
    setView(lat = 38.921279, lng = -91.519416, zoom = 3) %>% 
    addProviderTiles(providers$OpenStreetMap, options = providerTileOptions(noWrap = TRUE), group = "OSM (default)") %>%
    addPolygons(
      fillColor = ~pal_report(aggregated_reactive()$report),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(weight = 5, color = "#666", dashArray = "", fillOpacity = 0.7, bringToFront = TRUE),
      label = label_report,
      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "10px", direction = "auto")
    ) %>%
    addLegend(pal = pal_report, values = ~aggregated_reactive()$report, group = groupname, opacity= 0.7, title = NULL, position = "bottomright", labFormat = legendformat)
  })
}

shinyApp(ui = ui, server = server)


