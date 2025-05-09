red_ex_icon <- makeIcon(
  iconUrl = "https://cdn-icons-png.flaticon.com/512/1828/1828666.png",
  iconWidth = 10, iconHeight = 10,
  iconAnchorX = 5, iconAnchorY = 5
)

native_range_square_icon <- makeIcon(
  iconUrl = "https://cdn-icons-png.flaticon.com/512/5112/5112895.png",
  iconWidth = 10, iconHeight = 10,
  iconAnchorX = 5, iconAnchorY = 5
)

anecdotal_question_icon = makeIcon(
  iconUrl = "https://cdn-icons-png.flaticon.com/512/471/471664.png",
  iconWidth = 10, iconHeight = 10,
  iconAnchorX = 5, iconAnchorY = 5
)

legend_html <- HTML(
  paste0(
  "<div style='background: #ffffff00; padding: 0px; border-radius: 5px;'>
     <strong>Additional Record Types</strong>
     <li><img src='",red_ex_icon$iconUrl,"' height='24' style='vertical-align:middle;'> Eradicated
     <li><img src='",native_range_square_icon$iconUrl,"' height='24' style='vertical-align:middle;'> Native Range
     <li><img src='",anecdotal_question_icon$iconUrl,"' height='24' style='vertical-align:middle;'> Anecdotal Reports
   </div>"
  )
)

leaflet() |>
  addTiles() |>
  addControl(html = legend_html, layerId = 'custom_legend')
