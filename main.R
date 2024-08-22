source("global.R")
source("server.R")
source("ui.R")


shiny::shinyApp(ui, server,options = list(port =3838 ,host = "0.0.0.0"))
