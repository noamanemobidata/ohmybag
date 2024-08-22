

ui <- add_cookie_handlers(fluidPage(
  

  tags$meta(name="description", content="Author: miskowski85@hotmail.fr"),
  tags$meta(name="title", content="Oh My Bag: the heist of the century"),
  tags$audio(src = "audio/audio.mp3", type = "audio/mp3", autoplay = TRUE,loop=TRUE, controls = NA,style="display:none;"), 
  use_font("special-elite", "www/css/special-elite.css"),
  shiny::includeCSS("www/css/custom_style.css"), 
  useConfetti(),
  tags$head(
    tags$script(src = "js/cursor_update.js"),
    tags$script(src = "js/bag_selection.js")
  ),  
  uiOutput('timeleft'), 
  uiOutput("goal"), 
  uiOutput("goal_score"),
  br(), 
  div(style='padding-top:100px;position:absolute;', 
      
      
      uiOutput("play_area"), 
      br(), 
      
      div( style="left:10px;right:10px;bottom:12px;position:inherit;cursor:inherit;" , 
           
           uiOutput("score")
      )
      
  )
)
)