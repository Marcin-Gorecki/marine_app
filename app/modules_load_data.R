

# input data reading and manipulations ------------------------------------


if (1) {
  # dt_marine = fread("input/ships.csv", encoding = "UTF-8")
  dt_marine = readRDS("input.rds")
  dt_ship_type = unique(dt_marine[, .(SHIPTYPE, ship_type)])[order(ship_type)]
  dt_ships = unique(dt_marine[, .(SHIPTYPE, SHIP_ID, SHIPNAME)])[order(SHIPNAME)]
  
  # data manipultaion
  dt_marine[, date_n := ymd_hms(DATETIME, tz = "UTC")]
  dt_marine = dt_marine[order(SHIP_ID, date)]
  dt_marine[, `:=`(
    lat_next = lead(LAT, 1, NA),
    lon_next = lead(LON, 1, NA),
    sailing_time = difftime(lead(date_n, 1, NA), date_n, units = "secs")
  ), by = "SHIP_ID"]
  dt_marine_dist = dt_marine[!is.na(lat_next)]
  # distance from LON, LAT
  dt_marine_dist[, dist := distHaversine(.SD[, .(LON, LAT)],
                                         .SD[, .(lon_next, lat_next)])]
}

# for leaflet
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# shiny modules -----------------------------------------------------------


# drop down lists - selecting ship
dropdownUI <- function(id, label = "drop_down") {
  ns <- NS(id)
  tagList(sidebar_layout(
    sidebar_panel(
      p("Select ship type:"),
      dropdown_input(
        input_id = ns("type"),
        choices = dt_ship_type$ship_type,
        choices_value = dt_ship_type$SHIPTYPE
        # default_text = "All"
      ),
      
      p("Select ship:"),
      dropdown_input(
        input_id = ns("ship"),
        choices = dt_ships$SHIPNAME,
        choices_value = dt_ships$SHIP_ID
        # default_text = "All"
      ),
      
      p(),
      
      p("Max distance ship sailed (between readings):"),
      textOutput(ns("dist_note")),
      
      width = 1
    ),
    # map
    main_panel(leafletOutput(ns("map")),
               width = 4)
  ))
}

mapUI <- function(id, label = "map") {
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map"))
  )
}

dropdownServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 observeEvent(input$type, {
                   cat(file = stderr(), "ship type start\n")
                   
                   cond = ifelse(input$type == "",
                                 dt_ship_type$SHIPTYPE,
                                 c(input$type)
                                 )
                   
                   update_dropdown_input(
                     session,
                     "ship",
                     choices = dt_ships[SHIPTYPE %in% cond]$SHIPNAME,
                     choices_value = dt_ships[SHIPTYPE  %in% cond]$SHIP_ID
                   )
                   cat(file = stderr(), "ship type: ", input$type, "\n")
                 })
               })
}


outputServer <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 
                 cat(file = stderr(), "output start", "\n")
                 
                 observeEvent(input$ship,{
                 })
                 
                 wanted_row <- eventReactive(input$ship, {
                   dt_marine_dist[SHIP_ID == input$ship][order(-dist)][1,]
                 }, ignoreNULL = FALSE)
                 
                 output$dist_note <- renderText({
                   paste0(paste0(round(wanted_row()$dist, 2), " m"), "\n")
                          # paste(wanted_row()[, .(LON, LAT, lon_next, lat_next)], collapse = "\n"))
                 })
                 
                 output$map <- renderLeaflet({
                   leaflet() %>%
                     addProviderTiles(providers$Stamen.TonerLite,
                                      options = providerTileOptions(noWrap = TRUE)) %>%
                     addMarkers(data = rbind(
                       c(wanted_row()$LON, wanted_row()$LAT),
                       c(wanted_row()$lon_next, wanted_row()$lat_next)
                     )) %>%
                     addPolylines(data = rbind(
                       c(wanted_row()$LON, wanted_row()$LAT),
                       c(wanted_row()$lon_next, wanted_row()$lat_next)
                     ))
                 })
               })
}
