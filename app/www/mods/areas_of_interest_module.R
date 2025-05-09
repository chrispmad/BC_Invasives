areas_of_interest_UI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns('aq_or_terr'), label = 'Aquatic or Terrestrial',
                 choices = c("Aquatic","Terrestrial")),
    uiOutput('area_type_ui')
    )
}

areas_of_interest_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      output$area_type_ui = renderUI({
        if(input$aq_or_terr == 'Aquatic'){
          selectInput('area_type', label = 'Area Type',
                      choices = c("Natural Resource regions",
                                  "Natural Resource districts",
                                  "Natural Resource areas",
                                  "Park(s) by name"
                      )
          )
        } else {
          selectInput('area_type', label = 'Area Type',
                      choices = c("Natural Resource regions",
                                  "Natural Resource districts",
                                  "Natural Resource areas",
                                  "Park(s) by name"
                      )
          )
        }
      })
    }
  )
}
