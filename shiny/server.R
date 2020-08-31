function(input, output, session) {

  # functions ---------------------------------------------------------------
  
  # reset/default map
  leaflet.blank <- function() {
    leaflet() %>%
      #addProviderTiles(providers$CartoDB.Positron, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addLayersControl(
        baseGroups = c("Street Map", "Imagery")
      ) %>%
      setView(lng = -122.008546, lat = 47.549390, zoom = 9) %>%
      addEasyButton(
        easyButton(
          icon="fa-globe", 
          title="Zoom to Region",
          onClick=JS("function(btn, map){ 
                     map.setView([47.549390, -122.008546],9);}"))
          )
  }
  
  # show leaflet results for parcels
  leaflet.results <- function(proxy, selected.data, popup) {
    proxy %>% 
      clearMarkers() %>%
      addMarkers(data = selected.data,
                 ~lon,
                 ~lat,
                 popup = popup
      ) 
  }  
  
  # reset/default map
  leaflet.blank.blds <- function() {
    leaflet() %>%
      #addProviderTiles(providers$CartoDB.Positron, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addLayersControl(
        baseGroups = c("Street Map", "Imagery")
      ) %>%
      setView(lng = -122.008546, lat = 47.549390, zoom = 9) %>%
      addEasyButton(
        easyButton(
          icon="fa-globe", 
          title="Zoom to Region",
          onClick=JS("function(btn, map){ 
                     map.setView([47.549390, -122.008546],9);}"))
      ) %>% #### Adapted from https://redoakstrategic.com/geoshaper/
      addDrawToolbar(targetGroup='Selected',
                     polylineOptions=FALSE,
                     markerOptions = FALSE,
                     polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
                     rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
                     circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
                     editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())
      )
  }
  
  
  # show leaflet results for buildings
  leaflet.results.blds <- function(proxy, selected.data, popup, add=FALSE, cluster = FALSE, layer.id = NULL) {
    if(!add) proxy <- proxy %>% clearMarkers() %>% clearMarkerClusters()
    cluster.options <- NULL
    if(cluster) cluster.options <- markerClusterOptions()
    proxy <- proxy %>% addCircleMarkers(data = selected.data,
                               ~lon,
                               ~lat,
                               radius = 3,
                               popup = popup,
                               fillOpacity=0.4,
                               color = ~color, 
                               clusterOptions = cluster.options,
                               layerId = as.character(layer.id)
                )
    if(!add)
     proxy <- proxy %>% #### Adapted from https://redoakstrategic.com/geoshaper/
                addDrawToolbar(targetGroup='Selected',
                     polylineOptions=FALSE,
                     markerOptions = FALSE,
                     polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
                     rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
                     circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0,color = 'white',weight = 3)),
                     editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())
        )
    proxy
  }  
  
  # format table
  format.table <- function(sSelected) {
    sSelected %>%
      rename(parcel_fips = parcel_id_fips,
              cnty = county_id,
              tract_id = census_tract_id,
              block_group_id = census_block_group_id,
              block_id = census_block_id,
             census_2010_block = census_2010_block_id,
              LUtype = land_use_type_id,
      #        bldg_sqft = building_sqft, 
               nonres_sqft = non_residential_sqft,
               HH = households,
               Pop = population,
      #         Jobs = jobs,
      #        num_bldgs = number_of_buildings,
               DU = residential_units
      #        gwthctr_id = growth_center_id,
      #        area = AREA,
      #        shape_area = Shape_Area
       ) %>%
      mutate( #shape_area = round(shape_area, 10),
      #        area = round(area, 2),
      #        max_dua = round(max_dua, 2),
      #        max_far = round(max_far, 2),
              lat = round(lat, 4),
              lon = round(lon, 4)
       ) %>%
      select(cnty, parcel_id, parcel_fips, tract_id, block_group_id, block_id, census_2010_block, zone_id, faz_id, 
             parcel_sqft, LUtype, use_code, land_value, DU, HH, Pop, nonres_sqft, jobs, lat,lon)
  }

  # Search by Number -------------------------------------------------------- 

  # place holder for parcel_ids
  values <- reactiveValues(ids = NULL)
  values.cl <- reactiveValues(ids = NULL)
  
  # Query by:
  sQueryBy <- eventReactive(input$s_goButton, {
    input$s_queryBy  
  })
  
  # update place holder with user's parcel_ids
  observeEvent(input$s_goButton, {
    values$ids <- input$s_id
  })
  
  # clear map, table, and values in input text box
  observeEvent(input$s_clearButton, {
    values$ids = " "
  })
  
  observeEvent(input$s_clearButton, {
    updateTextInput(session, "s_id",
                    value = " ")
  })
  
  observeEvent(input$s_clearButton, {
    leafletProxy("map") %>% clearMarkers()
  })
  
  # filter for selected ids based on sQueryBy()
  sSelected <- reactive({
    if (is.null(values$ids)) return(NULL)
  
    rng <- grep(":", values$ids)

    if (length(rng) > 0) {
      rng.result <- scan(text = values$ids, sep = ":", quiet = TRUE)
      numItems <- rng.result[1]:rng.result[2]
    } else {
      numItems <- scan(text = values$ids, sep = ",", quiet = TRUE)
    }
    expr <- lazyeval::interp(~col %in% numItems, col = as.name(sQueryBy()))
    parcels.filter <- parcels.attr %>% filter_(expr) 
    
    if (nrow(parcels.filter) > 5000){
      parcels.filter %>% sample_n(5000)
    } else {
      parcels.filter
    }
  })
  
  # table manipulation
  sTable <- reactive({
    if (is.null(sSelected())) return(NULL)
    
    sSelected <- sSelected()
    format.table(sSelected) %>%
      mutate(loc = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', lon, '"><i class="fa fa-crosshairs"></i></a>', sep="")) %>%
      select(loc, everything())
  })
  
  observe({
    sSelected <- sSelected()
    if (is.null(sSelected) || values$ids == " ") return()
    marker.popup <- ~paste0("<strong>Parcel ID: </strong>", as.character(parcel_id))
    leaflet.results(leafletProxy("map"), sSelected, marker.popup)
  })
  
  # display parcel_ids
  output$map <- renderLeaflet({
    leaflet.blank()
  })
  
  # zoom to selected parcel in datatable when 'loc' icon is clicked
  # Adapted from https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      lat <- input$goto$lat
      lng <- input$goto$lng
      map %>% setView(lng, lat, zoom = 18)
    })
  })
  
  output$s_dt <- DT::renderDataTable({
    locate <- DT::dataTableAjax(session, sTable())
    DT::datatable(sTable(), 
                  extensions = 'Buttons', 
                  caption = "Click on icon in 'loc' field to zoom to parcel",
                  options = list(ajax = list(url = locate),
                                 dom = 'Bfrtip',
                                 buttons = c('csv', 'excel')
                                 ), 
                  escape = c(1))
  })

  output$sum_dt <- DT::renderDataTable({
    dat <- sSelected()
    if (is.null(dat) || nrow(dat) < 2) return(NULL) # don't show summary table if only one record was selected
    d <- data.table(dat)[, .(
                    id = isolate(input$s_id),
                    buildings = .N,
                    DU = sum(residential_units, na.rm = TRUE), 
                    HH = sum(households, na.rm = TRUE),
                    Pop = sum(population, na.rm = TRUE),
                    non_res_sf = sum(non_residential_sqft, na.rm = TRUE), 
                    Jobs = sum(jobs, na.rm = TRUE))
                  ]
    setnames(d, "id", isolate(input$s_queryBy))
    datatable(d, caption = "Summary", rownames = FALSE,
              options = list(paging = FALSE, searching = FALSE, columns.orderable = FALSE))
  })
  

  # Search by Click ---------------------------------------------------------  

    
  observe({
    event <- input$mapc_click
    if (is.null(event))
      return()
    isolate({
      x <- parcels.attr$lat
      y <- parcels.attr$lon
      dist <- sqrt((x-event$lat)^2 + (y-event$lng)^2)
      values.cl$ids <- parcels.attr$parcel_id[which.min(dist)]
    })
  })
  
  sSelectedcl <- reactive({
    if (is.null(values.cl$ids)) return(NULL)
    parcels.attr %>% filter(parcel_id %in% values.cl$ids)
  })
  
  observe({
    sSelected <- sSelectedcl()
    if (is.null(sSelected) || values.cl$ids == " ") return()
    marker.popup <- ~paste0("<strong>Parcel ID: </strong>", as.character(parcel_id))
    leaflet.results(leafletProxy("mapc"), sSelected, marker.popup)
  })
    
  # display parcel_ids by clicks on the map
  output$mapc <- renderLeaflet({
    leaflet.blank()
    })
  
  # table manipulation
  sTablecl <- reactive({
    if (is.null(sSelectedcl())) return(NULL)
    sSelected <- sSelectedcl()
    format.table(sSelected)
  })
  
  output$s_dtc <- DT::renderDataTable({
    DT::datatable(sTablecl(), 
                  extensions = 'Buttons', 
                  options = list(dom = 'Bfrtip',
                                 buttons = c('csv', 'excel')
                  ), 
                  escape = c(1))
  })
  
  #######
  # Buildings tab
  ##########
  # store selections for tracking
  data.of.click <- reactiveValues(clickedMarker = list(), #all parcels within boundaries (unfiltered)
                                  selected = list(), #parcels matching sub.data.deb() (filtered)
                                  showed = list(), # showed on the map
                                  selected.by.id = list()
  ) 
  
  
  # display initial map
  output$mapb <- renderLeaflet({
    leaflet.blank.blds()
  })
  
  subset.data <- reactive({
    if (is.null(values$ids_bld) || values$ids_bld == " ") return(NULL)
    rng <- grep(":", values$ids_bld)
    
    if (length(rng) > 0) {
      rng.result <- scan(text = values$ids_bld, sep = ":", quiet = TRUE)
      numItems <- rng.result[1]:rng.result[2]
    } else {
      numItems <- scan(text = values$ids_bld, sep = ",", quiet = TRUE)
    }
    expr <- lazyeval::interp(~col %in% numItems, col = as.name(bQueryBy()))
    subdata <- buildings %>% filter_(expr) 
    if (nrow(subdata)==0) return()
    #browser()
    subdata <- data.table(subdata)[generic_building_type_id %in% as.integer(input$BTfilter)]
    if (nrow(subdata)==0) return()
    if(input$color %in% c("sizeres", "sizenonres")) {
      values <- log(subdata[[color.attributes[input$color]]]+1)
      palette.size <- colorQuantile("YlOrRd", range(values), n=9)
      palette.name <- "palette.size"
    } else {
      palette.name <- paste0("palette.", input$color)
      values <- subdata[[color.attributes[input$color]]]
    }
    subdata[, color := NA]
    if(nrow(subdata) > 0) subdata[, color:= do.call(palette.name, list(values))]
    subdata
  })
  
  subset.data.deb <- subset.data %>% debounce(1000) # causes some delay for collecting inputs
  
  subset.data.no.id <- reactive({
    subdata <- buildings
    if (nrow(subdata)==0) return()
    #browser()
    subdata <- data.table(subdata)[generic_building_type_id %in% as.integer(input$BTfilter)]
    if (nrow(subdata)==0) return()
    if(input$color %in% c("sizeres", "sizenonres")) {
      values <- log(subdata[[color.attributes[input$color]]]+1)
      palette.size <- colorQuantile("YlOrRd", range(values), n=9)
      palette.name <- "palette.size"
    } else {
      palette.name <- paste0("palette.", input$color)
      values <- subdata[[color.attributes[input$color]]]
    }
    subdata[, color := NA]
    if(nrow(subdata) > 0) subdata[, color:= do.call(palette.name, list(values))]
    subdata
  })
  
  subset.data.no.id.deb <- subset.data.no.id %>% debounce(1000) # causes some delay for collecting inputs
  
  marker.popup <- function() ~paste0("Parcel ID:  ", parcel_id, 
                                     "<br>Bld ID:     ", as.integer(building_id), 
                                     "<br>Year built: ", as.integer(year_built),
                                     "<br>Bld type:   ", building_type_name,
                                     "<br>DU:         ", as.integer(residential_units),
                                     "<br>HHs:         ", as.integer(households),
                                     "<br>Non-res sf: ", as.integer(non_residential_sqft),
                                     "<br>Jobs: ", as.integer(jobs)
                            )
  # display markers
  observe({
    dat <- subset.data.deb()
    if(!is.null(dat) && values$ids_bld != " ") {
      leaflet.results.blds(leafletProxy("mapb"), dat, marker.popup(), 
                    cluster = input$cluster)
      data.of.click$selected.by.id <- dat
    } else {
      if(length(data.of.click$selected.by.id) > 0){ # previous selection needs to be removed
        leafletProxy("mapb") %>% clearMarkers() %>% clearMarkerClusters()
        data.of.click$selected.by.id <- NULL
      }
    }
    update.selection()
    if(length(data.of.click$showed) > 0) {
      if(length(data.of.click$selected.by.id) == 0) 
        leafletProxy("mapb") %>% clearMarkers() %>% clearMarkerClusters()
      leaflet.results.blds(leafletProxy("mapb"),data.of.click$showed, marker.popup(), add = TRUE,
                           cluster = input$cluster)
      
    }
  })
  
  # Query by:
  bQueryBy <- eventReactive(input$bld_goButton, {
    input$bld_queryBy  
  })
  
  # update place holder with user's parcel_ids
  observeEvent(input$bld_goButton, {
    values$ids_bld <- input$bld_id
  })
  
  # clear map, table, and values in input text box
  observeEvent(input$bld_clearButton, {
    values$ids_bld = " "
    updateTextInput(session, "bld_id",
                    value = " ")
    leafletProxy("mapb") %>% clearMarkers() %>% clearMarkerClusters()
    data.of.click$selected <- NULL # clears table
    data.of.click$selected.by.id <- NULL
    data.of.click$showed <- NULL
    data.of.click$clickedMarker <- NULL
  })
  
  # Color palette
  #palette.bt <- colorFactor(rainbow(nrow(building_types_selection)), 
  #                          levels=building_types_selection[,1])
  palette.bt <- colorFactor(rainbow(nrow(building_types)), 
                            levels=building_types[,building_type_id])
  
  
  # enable/disable color selection depending on clustering
  observeEvent(input$cluster, {
    shinyjs::toggleState("color", input$cluster == FALSE)
  })
  
  #### Adapted from https://redoakstrategic.com/geoshaper/ ---------------------  
  

  draw.selection.data <- reactive({
    if(is.null(input$mapb_draw_new_feature)) return(NULL)
    found_in_bounds <- findLocations(shape = input$mapb_draw_new_feature,
                                     location_coordinates = coordinates,
                                     location_id_colname = "parcel_id")
    clickedM <- isolate(data.of.click$clickedMarker)
    for(id in found_in_bounds){
      if(id %in%  clickedM){
        # don't add id
      } else {
        # add id
        clickedM <- append(clickedM, id, 0)
      }
    }
    # look up parcels by ids found
    dat <- subset.data.no.id.deb()
    if(is.null(dat)) return()
    subset(dat, parcel_id %in% clickedM)
  })
  
  update.selection <- reactive({
    dat <- draw.selection.data()
    if(is.null(dat)) return(NULL)
    if(nrow(dat) > 5000)
      sdat.to.mark <- dat[sample(1:nrow(dat), 5000),]
    else sdat.to.mark <- dat
    data.of.click$selected <- dat
    data.of.click$showed <- sdat.to.mark
    data.of.click$clickedMarker <- dat$parcel_id
  })
  
  observeEvent(input$mapb_draw_new_feature, {
    #Only add new layers for bounded locations
    update.selection()
  })
  
  observeEvent(input$mapb_draw_deleted_features,{
    # loop through list of one or more deleted features/ polygons
    bounded_layer_ids <- c()
    for(feature in input$mapb_draw_deleted_features$features){
       # get ids for locations within the bounding shape
       bounded_layer_ids <- c(bounded_layer_ids, 
                              findLocations(shape = feature,
                                          location_coordinates = coordinates,
                                          location_id_colname = "parcel_id"
                                          )
                              )
    }
    proxy <- leafletProxy("mapb")
    proxy %>% removeShape(layerId = as.character(bounded_layer_ids))
    if(length(data.of.click$clickedMarker) > 0)
      data.of.click$clickedMarker <- data.of.click$clickedMarker[!data.of.click$clickedMarker %in% bounded_layer_ids]
    if(length(data.of.click$selected) > 0)
      data.of.click$selected <- data.of.click$selected[!parcel_id %in% bounded_layer_ids]
    if(length(data.of.click$showed) > 0)
      data.of.click$showed <- data.of.click$showed[!parcel_id %in% bounded_layer_ids]
  })
  
  # Display summary table in the buildings tab
  output$bdt <- DT::renderDataTable({
    if (length(data.of.click$selected) == 0 && length(data.of.click$selected.by.id) == 0) return(NULL)
    data <- unique(rbind(data.of.click$selected, data.of.click$selected.by.id))
    d <- data[, .(buildings = .N,
                     DU = sum(residential_units), 
                     HH = sum(households),
                     Pop = sum(population),
                     non_res_sf = sum(non_residential_sqft), 
                     Jobs = sum(jobs))
              ]
    datatable(d, caption = "Summary", rownames = FALSE,
              options = list(paging = FALSE, searching = FALSE, columns.orderable = FALSE))
  })
  
  ### Table tab
  # table manipulation
  tTable <- reactive({
    dat <- parcels.attr
    if (is.null(dat)|| nrow(dat) < 1) return(NULL)

    d <- data.table(dat)[, .(
        DU = sum(residential_units, na.rm = TRUE), 
        HH = sum(households, na.rm = TRUE),
        Pop = sum(population, na.rm = TRUE),
        non_res_sf = sum(non_residential_sqft, na.rm = TRUE), 
        Jobs = sum(jobs, na.rm = TRUE)), 
      by = eval(input$tbl_queryBy)
      ]
    d
    #datatable(d, caption = "Summary", rownames = FALSE,
    #          options = list(paging = TRUE, searching = TRUE, columns.orderable = TRUE))
  })
  
  # Display table in the Table tab
  output$tdt <- DT::renderDataTable({
    #locate <- DT::dataTableAjax(session, tTable())
    DT::datatable(tTable(), 
                  extensions = 'Buttons', 
                  caption = "",
                  options = list(#ajax = list(url = locate),
                                 #dom = 'Bfrtip',
                                 pageLength = 50,
                                 buttons = c('csv', 'excel')
                  ))
  })
  
  # Polygon tab
  ##############
  map.layers <- function(proxy, shapefile, mappalette, indname, popup, digits = 0){
    proxy %>% clearShapes() %>% clearControls() %>% 
      addPolygons(data = shapefile,
                          fillColor = ~mappalette(shapefile[[indname]]), 
                          fillOpacity = 0.7,
                          stroke = TRUE,
                          color = "#8a8a95",
                          weight = 1,
                          popup = popup,
                          layerId = ~name_id) %>%
      addLegend("bottomright",
                pal = mappalette,
                values = mappalette(shapefile[[indname]]),
                title = indname,
                opacity =1,
                labFormat = labelFormat(big.mark = ",", digits = digits))
  }
  
  output$pol_map <- renderLeaflet({
    leaflet.blank()
  })
  map.colorBins <- function(indname, indvalues){
    max.bin <- max(abs(indvalues), na.rm = TRUE)
    digits <- 0
    if(indname %in% names(polmap.settings) && "breaks" %in% names(polmap.settings[[indname]])) {
      absbreaks <- polmap.settings[[indname]]$breaks
      if(max.bin > max(absbreaks)) absbreaks <- c(absbreaks, max.bin)
    } else {
      absbreaks <- (sqrt(max.bin)*c(0, 0.1, 0.2,0.4, 0.6, 0.8, 1.01))^2 # breaks on sqrt scale
    }
    if(indname %in% names(polmap.settings) && "digits" %in% names(polmap.settings[[indname]]))
      digits <- polmap.settings[[indname]]$digits
    return(list(color="Reds", bin=absbreaks, digits = digits))
  }
  
  get.polmap.popup <- function(shp, indname, digits) {
    popup <- paste0("<strong>ID: </strong>", shp$name_id,
           "<br><strong>", indname,": </strong>", prettyNum(round(shp[[indname]], digits), big.mark = ","))
    if(indname != "tot_households")
      popup <- paste0(popup,
           "<br><strong>", "total HH",": </strong>", prettyNum(round(shp$tot_households, digits), big.mark = ","))
    if(indname != "tot_population")
      popup <- paste0(popup,
           "<br><strong>", "total pop",": </strong>", prettyNum(round(shp$tot_population, digits), big.mark = ","))
    if(indname != "tot_jobs")
      popup <- paste0(popup,
           "<br><strong>", "total jobs",": </strong>", prettyNum(round(shp$tot_jobs, digits), big.mark = ","))
  }
  
  compute.indicator <- function(indicator, geo.id){
    select.columns <- unique(c("name_id", indicator, "tot_households", "tot_population", "tot_jobs"))
    result <- indicators.dt[[geo.id]][, select.columns, with = FALSE]
    #result <- dt[, .(ind = mean(eval(parse(text=indicator)))), by = list(name_id = eval(parse(text=geo.id)))]
    result
  }
  
  # GO button
  observeEvent(input$pol_goButton, {
    dt <- compute.indicator(input$pol_indicator, input$pol_queryBy)
    tshape <- merge(shapes[[input$pol_queryBy]], dt, by = "name_id")
    colorBinResult <- map.colorBins(input$pol_indicator, dt[[input$pol_indicator]])
    pal <- colorBin(palette = colorBinResult$color, bins = colorBinResult$bin)
    popup <- get.polmap.popup(tshape, input$pol_indicator, colorBinResult$digits)
    map.layers(leafletProxy("pol_map"), tshape, pal, input$pol_indicator, 
                      popup = popup, digits = colorBinResult$digits)
  })
  
  # clear map
  observeEvent(input$pol_clearButton, {
    leafletProxy("pol_map") %>% clearShapes() %>% clearControls()
  })
}# end server function






