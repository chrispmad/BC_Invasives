incident_dashboard_navpanel = bslib::nav_panel(
  title = 'Incident Reports',
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
      card(
        card_header(
          h5("Something else?")
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
      ),
      fluidRow(
        column(width = 6,
               card(
                 card_header(
                   h5("Total Reports")
                 ),
                 textOutput('total_reports')
               )
        ),
        column(width = 6,
               card(
                 card_header(
                   h5("Distinct Invasive Species Reported"),
                 ),
                 textOutput('distinct_sp_reports')
               )
        )
      )
    ),
    column(
      width = 4,
      card(
        card_header(
          h5("Outcome and/or Action")
        ),
        plotlyOutput('outcome_tree_plot', height = '40vh')
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
