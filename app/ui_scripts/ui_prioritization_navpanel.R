prioritization_navpanel = bslib::nav_panel(
  title = 'Prioritization Model',
  tabsetPanel(
    tabPanel(
      title = 'Model Inputs',
      layout_column_wrap(
        1/3,
        card(
          card_title('Focal Species'),
          id = 'focal_species_input_card',
          style = 'overflow: visible;',
          card_body(
            spec_search_UI('p_model_layer'),
            actionButton(
              'add_occ_data_to_p_model',
              label = textOutput('occ_data_results_text')
            ),
            style = 'background:transparent;border:none;'
          )
        ),
        card(
          card_title('Area(s) of Interest'),
          id = 'geog_units_input_card',
          style = 'overflow: visible;',
          card_body(
            selectInput('geog_units_presets',
                        'Preset Options',
                        choices = c('Natural Resource Regions' = 'nr_regs',
                                    'Ecosections' = 'ecosecs',
                                    'Custom File' = 'custom_file')),
            uiOutput('geog_unit_upload_ui'),
            actionButton(
              'add_geog_units_to_p_model',
              label = 'Add Geographic Units to model'
            )
          )
        ),
        card(
          card_title('Risk Layers'),
          id = 'risk_layers_input_card',
          style = 'overflow: visible;',
          card_body(
            fileInput('risk_layers_input',
                      'Risk Layer(s)',
                      multiple = TRUE,
                      accept = c('.gpkg','.zip')),
            uiOutput('risk_layer_weights_ui'),
            actionButton(
              'add_risk_layers_to_p_model',
              label = 'Add Risk Layers to model'
            )
          )
        )
      ),
      layout_column_wrap(
        1/2,
        card(
          card_title('Wipe Current Model'),
          card_body(
            actionButton('wipe_model',
                         'Start Fresh!') |>
              bslib::tooltip('Start fresh with a new prioritization model')
          )
        ),
        card(
          card_title('Run Model'),
          id = 'run_model_card',
          style = 'overflow: visible;',
          card_body(
            actionButton(
              'run_p_model',
              'Run Model!'
            )
          )
        )
      )
    ),
    tabPanel(
      title = 'Model Output',
      DT::DTOutput('p_model_output_DT')
    )
  )
)
