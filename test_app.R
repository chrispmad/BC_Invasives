library(shiny)

source('mods/areas_of_interest_module.R')
ui <- fluidPage(

  fluidRow(
    column(
      width = 2,
      radioButtons('aq_or_terr',
                   label = '',
                   choices = c("Aquatic","Terrestrial")),
      actionButton('search','Get Area(s)')
    ),
    column(
      width = 10,
      uiOutput('area_type_ui'),
      uiOutput('name_search_ui')
    )
  )
)

server <- function(input, output, session) {
  output$area_type_ui = renderUI({
    if(input$aq_or_terr == 'Terrestrial'){
      selectInput('area_type', label = 'Area Type',
                  choices = c("Natural Resource regions" = 'nr_regs',
                              "Natural Resource districts" = 'nr_dists',
                              "Natural Resource areas" = 'nr_areas',
                              "Park(s) by name" = 'names'
                  )
      )
    } else {
      selectInput('area_type', label = 'Area Type',
                  choices = c("Lake(s) by name" = 'names',
                              "Lakes in Region" = 'wb_in_regions'),
                  selected = "names"
      )
    }
  })

  output$name_search_ui = renderUI({
    req(list(input$aq_or_terr, input$area_type))

    ui_output = NULL

    if(input$aq_or_terr == 'Aquatic'){
      if(input$area_type == 'names'){
        ui_output = textInput('name_for_search','Waterbody Name(s)',placeholder = 'Name 1, Name 2, ...')
      }
      if(input$area_type == 'wb_in_regions'){
        ui_output = tagList(
          textInput('wb_region_name','Region Name(s)',placeholder = 'Name 1, Name 2, ...'),
          sliderInput('area_cutoff_wbs','Minimum Size', min = 1, max = 500, value = 100)
        )
      }
    } else {
      if(input$area_type == 'names'){
        ui_output = textInput('name_for_search','Park Name(s)',placeholder = 'Name 1, Name 2, ...')
      } else {
        ui_output = NULL
      }
    }
    ui_output
  })
}

shinyApp(ui, server)
