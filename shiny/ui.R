navbarPage(theme = shinytheme("simplex"),
           "Base Year 2023 Explorer",
           tabPanel("Parcels by Number",
                    tags$head(tags$script(src="gomap.js")),
                    fluidPage(
                      fluidRow(
                        column(width = 2,
                               selectInput(inputId = "s_queryBy",
                                           label = h4("Query for parcels by:"),
                                           choices = list("Parcel ID" = "parcel_id",
                                                          "TAZ" = "zone_id",
                                                          "FAZ" = "faz_id",
                                                          "City ID" = "city_id",
                                                          #"Control HCT" = "control_hct_id",
                                                          "2020 census block" = "census_2020_block_id",
                                                          "2020 census block group" = "census_2020_block_group_id",
                                                          "Census block (int)"= "census_block_id",
                                                          "Census block group (int)" = "census_block_group_id",
                                                          "Census tract (int)" = "census_tract_id",
                                                          "Plan type ID" = "plan_type_id",
                                                          "School ID" = "school_id",
                                                          "School ID (catchment)" = "school_catchment_id"
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
                               br(),
                               checkboxInput("color_pcl_by_tod", "Color by TOD", FALSE),
                               br(),
                               helpText("Last data update: 2024-09-18")
                               ), # end column
                        column(width = 10,
                               leafletOutput("map", height = "725px")
                               ) # end column
                      ), # end fluidRow
                      br(),
                      fluidRow(
                          column(width = 12,
                                 DT::dataTableOutput("sum_dt")
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
                                                              "FAZ" = "faz_id",
                                                              "City ID" = "city_id",
                                                              #"Control HCT" = "control_hct_id",
                                                              "2020 census block" = "census_2020_block_id",
                                                              "2020 census block group" = "census_2020_block_group_id",
                                                              "Census block (int)"= "census_block_id",
                                                              "Census block group (int)" = "census_block_group_id",
                                                              "Census_tract (int)" = "census_tract_id",
                                                              #"Plan type ID" = "plan_type_id",
                                                              "School ID" = "school_id"
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
                                                 "Size non-residential"="sizenonres",
                                                 "Year built" = "yearbuilt",
                                                 "TOD" = "tod"),
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
           ), # end tabPanel
           tabPanel("Tables",
                    fluidPage(
                            selectInput(inputId = "tbl_queryBy",
                                        label = h4("Show summary by:"),
                                        choices = list("region" = "region_id",
                                                       "county" = "county_id",
                                                       "city" = "city_id",
                                                       #"control HCT" = "control_hct_id",
                                                       "FAZ" = "faz_id",
                                                       "TAZ" = "zone_id",
                                                       "2020 census block" = "census_2020_block_id",
                                                       "2020 census block group" = "census_2020_block_group_id",
                                                       "census block (int)"= "census_block_id",
                                                       "census block group (int)" = "census_block_group_id",
                                                       "census_tract (int)" = "census_tract_id"
                                        ),
                                        selected = "county_id",
                                        width = '30%'),
                            br(),
                            DT::dataTableOutput("tdt", height = "300px")
                        )
           ),
           tabPanel("Spatial Indicators",
                    tags$head(tags$script(src="gomap.js")),
                    fluidPage(
                        #fluidRow(
                        #    h4("Show spatial indicators by geography")
                        #),
                        fluidRow(
                            column(width = 2,
                                   selectInput(inputId = "pol_queryBy",
                                               label = h4("Geography:"),
                                               choices = list("FAZ" = "faz_id",
                                                              "TAZ" = "zone_id"
                                               ),
                                               width = '100%'
                                   ),
                                   br(),
                                   selectInput(inputId = "pol_indicator",
                                               label = h4("Indicator:"),
                                               choices = list("household income" = "median_income",
                                                              "household size" = "average_hh_size",
                                                              "average age" = "average_age",
                                                              "population density" = "population_per_acre",
                                                              "% low income" = "percent_low_income",
                                                              "% high income" = "percent_high_income",
                                                              "total households" = "tot_households",
                                                              "total population" = "tot_population",
                                                              "% white" = "percent_white",
                                                              "% black" = "percent_black",
                                                              "% asian" = "percent_asian",
                                                              "% hispanic" = "percent_hispanic",
                                                              "% other race" = "percent_other",
                                                              "total jobs" = "tot_jobs",
                                                              "non-home-based jobs" = "nonHB_jobs",
                                                              "home-based jobs" = "home_based_jobs",
                                                              "jobs density" = "jobs_per_acre",
                                                              "jobs per capita" = "jobs_per_capita",
                                                              "land value per sf"="land_value_per_sf",
                                                              "total dwelling units"="total_du"#,
                                                              #"total DU capacity"="total_du_capacity",
                                                              #"free DU capacity"="free_du_capacity",
                                                              #"% free DU capacity"="percent_free_du_capacity"
                                               ),
                                               width = '100%'
                                   ),
                                   actionButton(inputId = "pol_goButton",
                                                label = "Enter"),
                                   actionButton(inputId = "pol_clearButton",
                                                label = "Clear"),
                                   br(),
                                   br(),
                                   br()
                            ), # end column
                            column(width = 10, leafletOutput("pol_map", height = "725px")
                            ) # end column
                        ), # end fluidRow
                    ) # end fluidPage
            )#, # end tabPanel
            #tabPanel("Graphs",
            #         htmlOutput('graphgvis'),
            #         HTML("<br/><i><small>*If you don't see a graph above, make sure Adobe Flash Player is installed and enabled in your browser.</small></i>")
            #)
) # end navbarPage
