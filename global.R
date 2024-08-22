library(shiny)
library(reticulate)
library(fontawesome)
library(glue)
library(sortable)
library(typedjs)
library(gfonts)
library(confetti)
library(cookies)
library(uuid)
library(DBI)
library(pool)
library(odbc)
library(bslib)

source_python('knapsack_solver.py')


GAME_DURATION_SECONDS = 18
TIMER_GREEN_THRESHOLD= 15
TIMER_WARNING_THRESHOLD= 7

calculate_selection_stats = function(v, items_table_par){
  
  l=unlist(lapply(v, function(i){ substr(i, 1, 1) } ))
  
  list(value= sum(items_table_par[items_table_par$items%in%l, "values"]), 
       weight= sum(items_table_par[items_table_par$items%in%l, "weights"])
  )
  
}


generate_game_scenario = function(BAG_SIZE="M"){
  
  
  items_table <- data.frame(
    items= c("🖼","💍", "🎸", "📺" ,'🫖', "🖥", "🎧","🕰", "📱", "🔫","⚽", "📷", "💻", "🎮", "🧸"),
    values= sample(5:40, 15), 
    weights= sample(1:15, 15)  
  ) 
  
  bs <- ifelse(BAG_SIZE=="L",0.5,  
               ifelse(BAG_SIZE=="M",0.35,0.25)
  )
  
  TOTAL_WEIGHT = sum(items_table$weights)
  MAX_WEIGHT=as.integer( round(TOTAL_WEIGHT*bs) )
  TOTAL_VALUE = sum(items_table$values)
  
  SOLUTION= solve(values = items_table$values, weights = items_table$weights, capacities = MAX_WEIGHT)
  
  items_list <- lapply(1:nrow(items_table), function(i){ 
    HTML(glue("<div style='color:white;display:inline;'>{items_table[i, 'items']} :  {sprintf('%02d', items_table[i, 'values'])} $ - {sprintf('%02d',items_table[i, 'weights'])} kg</div>"))
    
  })
  
  list(
    items_table= items_table, 
    TOTAL_WEIGHT=TOTAL_WEIGHT, 
    MAX_WEIGHT=MAX_WEIGHT, 
    TOTAL_VALUE=TOTAL_VALUE, 
    SOLUTION=SOLUTION, 
    items_list=items_list
  )
  
}



pool <- pool::dbPool(odbc::odbc(),
                     Driver = "postgresql",
                     Server =ifelse(Sys.getenv("ENVIR")=="dev", Sys.getenv('PGIP'), paste0("/cloudsql/",Sys.getenv('cloud_sql_con_name')) ),
                     Database = Sys.getenv("dbname") ,
                     UID = Sys.getenv("user"),
                     PWD = Sys.getenv("password"),
                     sslmode="require",
                     Port = 5432)
