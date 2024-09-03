source("global.R")
source("server.R")
source("ui.R")

addResourcePath("www", "www/")


shiny::shinyApp(ui, server,options = list(port =3838 ,host = "0.0.0.0"))
