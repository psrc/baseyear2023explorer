navbarPage(theme = shinytheme("simplex"),
           "Base Year 2018 Explorer",
           tabPanel("Parcels by Number",
                    tags$head(tags$script(src="gomap.js")),
                    fluidPage(
                      fluidRow(
                        column(width = 2,
                               selectInput(inputId = "s_queryBy",
                                           label = h4("Query for parcels by:"),
                                           choices = list("Parcel ID" = "parcel_id",
                                                          "TAZ" = "zone_id",
                                                          "2010 census block" = "census_2010_block_id",
                                                          "census block (int)"= "census_block_id",
                                                          "census block group (int)" = "census_block_group_id",
                                                          "census tract (int)" = "census_tract_id"
                                                          ),
                                           width = '100%'
                               ),
                               br(),
                               h4("Enter one or more ids"),
                               helpText("Separate multiple ids with commas or type as range (i.e. 1000:2000)"),
                               textInput(inputId = "s_id",
                                         label = "",
                                         width = '100%'
                                        ),
                               actionButton(inputId = "s_goButton",
                                            label = "Enter"),
                               actionButton(inputId = "s_clearButton",
                                            label = "Clear all"),
                               br(),
                               br(),
                               helpText("Geographic areas containing many parcels will be sampled to 5,000 parcels"),
                               br()
                               ), # end column
                        column(width = 10,
                               leafletOutput("map", height = "725px")
                               ) # end column
                      ), # end fluidRow
                      br(),
                      fluidRow(
                        column(width = 12,
                               DT::dataTableOutput("s_dt")
                               ) # end column
                      ) # end fluidRow
                    ) # end fluidPage
           ), # end tabPanel
           tabPanel("Parcels by Click",
              fluidPage(
                fluidRow(
                  h4("Click on map to identify parcel")
                ),
                br(),
                fluidRow(
                  leafletOutput("mapc", height = "725px")
                ), # end fluidRow
                br(),
                fluidRow(
                  DT::dataTableOutput("s_dtc")
                ) # end fluidRow
              ) # end fluidPage
           ),
           tabPanel("Buildings",
                    tags$head(tags$script(src="gomap.js")),
                    fluidPage(
                        fluidRow(
                            column(width = 2,
                                   selectInput(inputId = "bld_queryBy",
                                               label = h4("Query buildings by:"),
                                               choices = list("Building ID" = "building_id",
                                                              "Parcel ID" = "parcel_id",
                                                              "TAZ" = "zone_id",
                                                              "2010 census block" = "census_2010_block_id",
                                                              "census block (int)"= "census_block_id",
                                                              "census block group (int)" = "census_block_group_id"
                                                    ),
                                               width = '100%'
                                                ),
                                   br(),
                                   h4("Enter one or more ids"),
                                   helpText("Separate multiple ids with commas or type as range (i.e. 1000:2000)"),
                                   textInput(inputId = "bld_id",
                                             label = "",
                                             width = '100%'
                                   ),
                                   selectInput("BTfilter", "Generic Building Type", 
                                               structure(building_types_selection[,1],
                                                         names=rownames(building_types_selection)),
                                               multiple = TRUE, selected = building_types_selection[,1]),
                                   selectInput("color", "Color By", 
                                               c("Building type"="bt", 
                                                 "Size residential"="sizeres", 
                                                 "Size non-residential"="sizenonres"),
                                               selected = "bt"),
                                   checkboxInput("cluster", "Show in clusters", FALSE),
                                   actionButton(inputId = "bld_goButton",
                                                label = "Enter"),
                                   actionButton(inputId = "bld_clearButton",
                                                label = "Clear all"),
                                   br(),
                                   br()
                            ), # end column
                            column(width = 10,
                                   leafletOutput("mapb", height = "725px"),
                                   DT::dataTableOutput("bdt", height = "100px")
                            ) # end column
                        ) # end fluidRow
                    ) # end fluidPage
           ) # end tabPanel
) # end navbarPage
