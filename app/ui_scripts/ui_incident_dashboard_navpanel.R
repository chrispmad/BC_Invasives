incident_dashboard_navpanel = bslib::nav_panel(
  title = 'Incidence Reports',
  fluidRow(
    column(
      width = 4,
      card(
        card_header(
          h5("Number of Invasive Species over Time"),
          plotOutput('reports_over_time', height = '25vh')
        )
      ),
      card(
        card_header(
          h5("Top Reported Invasive Species"),
          plotOutput('top_reported_species', height = '25vh')
        )
      ),
      fluidRow(
        column(width = 6,
               bslib::value_box(
                 theme = 'success',
                 title = h5("Total Reports"),
                 value = textOutput('total_reports'),
                 max_height = 100
               )
        ),
        column(width = 6,
               bslib::value_box(
                 theme = 'warning',
                 title = h5("Distinct Species Reported"),
                 value = textOutput('distinct_sp_reports'),
                 max_height = 100
               )
        )
      )
    ),
    column(
      width = 4,
      card(
        card_header(
          h5("Incident Report Confirmation Status"),
          plotOutput('confirmation_status', height = '15vh')
        )
      ),
      card(
        card_header(
          h5("Incident Report Locations"),
          leafletOutput('inc_report_leaflet', height = '55vh')
        )
      )
    ),
    column(
      width = 4,
      card(
        card_header(
          h5("Outcome and/or Action")
        ),
        plotlyOutput('outcome_tree_plot', height = '40vh'),
        max_height = 300
      ),
      card(
        card_header(
          h5("Invasive Species Count by Region")
        ),
        plotlyOutput('region_column_bar', height = '30vh')
      )
    )
  )
)
