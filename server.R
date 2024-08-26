

server <- function(input, output, session) {
  
  

  
  
  observe({
  
    omg_uid <- get_cookie("omg_uid")


    if(is.null(omg_uid)){
      
      new_omg_uid <- UUIDgenerate()
      set_cookie(
        cookie_name = "omg_uid",
        cookie_value = new_omg_uid 
      )
      
      insert_string <- glue_sql("INSERT INTO game_scores (omg_id)
                             VALUES ({new_omg_uid}); ",.con = pool)

      dbExecute(pool, insert_string) 
      
    }


  })

  
  
  game_state = reactiveValues(items_table= NULL, 
                              TOTAL_WEIGHT=NULL, 
                              MAX_WEIGHT=NULL, 
                              TOTAL_VALUE=NULL, 
                              SOLUTION=NULL, 
                              items_list=NULL)
  
  observe({
    
    b= ifelse(is.null(input$bag_size), "M", input$bag_size)
    
    l=generate_game_scenario(BAG_SIZE = b)
    
    game_state$items_table= l$items_table
    game_state$TOTAL_WEIGHT=l$TOTAL_WEIGHT
    game_state$MAX_WEIGHT=l$MAX_WEIGHT
    game_state$TOTAL_VALUE=l$TOTAL_VALUE
    game_state$SOLUTION=l$SOLUTION
    game_state$items_list=l$items_list
    
  })
  
  observeEvent(input$restart, {
    
    removeModal()
    
    b= ifelse(is.null(input$bag_size), "M", input$bag_size)
    
    l=generate_game_scenario(BAG_SIZE = b)
    game_state$items_table= l$items_table
    game_state$TOTAL_WEIGHT=l$TOTAL_WEIGHT
    game_state$MAX_WEIGHT=l$MAX_WEIGHT
    game_state$TOTAL_VALUE=l$TOTAL_VALUE
    game_state$SOLUTION=l$SOLUTION
    game_state$items_list=l$items_list
    
    
    is_game_active(FALSE)
    timer(GAME_DURATION_SECONDS)
  })
  
  
  # Initialize the timer, 10 seconds, not active.
  timer <- reactiveVal(GAME_DURATION_SECONDS)
  is_game_active <- reactiveVal(FALSE)
  
  # Output the time left.
  output$timeleft <- renderUI({
    
    
    color_timer=  ifelse(timer()>= TIMER_GREEN_THRESHOLD, "green",
                         ifelse(timer()>= TIMER_WARNING_THRESHOLD, "gold","red"))
    
    cl = ifelse(timer()<TIMER_WARNING_THRESHOLD & timer()!=0, "shake-top","")
    
    

    
    absolutePanel(top = 10,left = 10,fixed = T, 
                  div(
                    class=cl, 
                    style=glue('border-radius:10px;background-color:{color_timer};width:142px;margin:1rem;font-size:38px;color:white;'),
                    div(
                      style="margin:1rem;text-align:center;padding-top:10px;",
                      HTML( paste0(icon("stopwatch"), "  :  ", timer() ))
                    )
                    
                  )
    )
    
  })
  
  # observer that invalidates every second. If timer is active, decrease by one.
  observe({
    req(game_state$items_table)
    invalidateLater(1000, session)
    isolate({
      if(is_game_active())
      {
        timer(timer()-1)
        if(timer()<1)
        {
          is_game_active(FALSE)
          
          tbl_sol=game_state$items_table[game_state$SOLUTION$is_packed, ]
          tw= sum(tbl_sol$weights)
          
          
          s=0
          EXEED_WEIGHT_LIMIT=F
          if(!is.null(input$rank_list_2)){
            m=calculate_selection_stats(input$rank_list_2,items_table_par = game_state$items_table )[["value"]]
            s <- round( m*100/game_state$SOLUTION$computed_value, 1)
            
    
            EXEED_WEIGHT_LIMIT= calculate_selection_stats(input$rank_list_2,items_table_par = game_state$items_table)[["weight"]] > game_state$MAX_WEIGHT
        
              
          }
          
          if(s>90 & !EXEED_WEIGHT_LIMIT){
            sendfireworks(duration = 5, particle_count = 100)
            message_cong <-glue( "Good job 🎉 !" )
          }else{
            message_cong <- ''
          }
          
          if(EXEED_WEIGHT_LIMIT){
            
            wmsg <- "You lost, the bag is torn!" 
            smsg <-  ''
            class_lights_container <- 'lights-container'
          }else{
            wmsg <- ''
            smsg <- glue("your reach <span style='font-size:26px;'>{m}$</span> ie <span style='font-size:26px;'>{s}%</span> of goal!")
            class_lights_container<- ''
          }          
          
          uid <- get_cookie("omg_uid")
          
          
          if(!EXEED_WEIGHT_LIMIT){
            
            upd_string <- glue_sql("UPDATE  game_scores SET nb_games=nb_games+1 , score=GREATEST(score,{s}) , date_last_game = CURRENT_TIMESTAMP WHERE omg_id={uid} ",.con = pool)
            dbExecute(pool, upd_string) 
            
          }
          
        
          
          showModal(modalDialog(
            title = NULL, 
            footer = actionButton(inputId = "restart",label = "New game",icon = icon('undo'),class="bag-size"),
            size = "l", fade = F, 
            easyClose = F,
            div(class=class_lights_container, 
                if(EXEED_WEIGHT_LIMIT){ tagList(
                  div( class="redlight"), 
                  div(class="bluelight")
                ) 
                }, 
            bucket_list(
              header =HTML(glue( "Countdown completed! {wmsg} { message_cong} {smsg} The optimal set of items is :")),
              group_name = "blg",
              orientation = "horizontal",
              options =sortable_options(sort = F,disabled = T, draggable = "dd",  animation=2000), 
              add_rank_list(
                text = HTML(glue("Total weight of :<span style='font-size:30px;'> {tw} Kg </span>")),
                labels = lapply(1:nrow(tbl_sol), function(i){ 
                  HTML(glue("<div style='color:white;display:inline;'>{tbl_sol[i, 'items']} :  {sprintf('%02d', tbl_sol[i, 'values'])} $ - {sprintf('%02d',tbl_sol[i, 'weights'])} kg</div>"))
                  
                }),
                input_id = "rank_list_sol"
                
              )
            )
            )
            
            
            
            
            
            
            
            
          ))
        }
      }
    })
  })
  
  
  output$init_message <- renderUI({
    
    HTML("<div style='font-size:16px;'> You're a skilled thief breaking into a house to steal valuable items.<br> <br>
                    Choose wisely (Drag & Drop), keeping your backpack's weight limit in mind. <br> <br>
                         Maximize the value of your loot before time runs out.<br><br>
                                  
                                    </div>")
  })
  
  output$init_message_goal <- renderUI({
    HTML("<div style='font-size:16px;'> Your score will be based on how close you get to the optimal solution (by Google OR-Tools Model) : <br> <br>  </div>")
  })
  
  
  
  observe({
    
    
    output$goal_message <- renderUI({
      
      
      typed(typeSpeed=4, 
            HTML(
              glue("<div style='font-size:16px;'><span style='font-size:26px;'>🎯 </span>  For this game, your target is :<span style='font-size:30px;'>  <u><b> {game_state$SOLUTION$computed_value} $</b></u></span> </div>")
            )
      )
      
    })
    
    showModal(
      modalDialog(title = NULL,size = "l",fade = FALSE, easyClose = F,
                  
                  
                  div(
                    style="color:white;",
                    
                    uiOutput("init_message"), 
                    uiOutput("init_message_goal"), 
                    uiOutput("goal_message")
                    
                    
                  )
                  ,
                  footer=
                    tagList(
                      uiOutput("bagchoiceui"), 
                      div(style="float:right;", 
                          
                          actionButton(inputId = "start",label = "Start",icon = icon("play"),class="bag-size")
                      )
                    )
                  
                  
      )
    )
    
  })
  
  
  observeEvent(input$restart, {
    
    #observe({
      
      
      output$goal_message <- renderUI({
        
        
        typed(typeSpeed=4, 
              HTML(
                glue("<div style='font-size:16px;'><span style='font-size:26px;'>🎯 </span> Here's your goal for this game  (optimized by Google OR-Tools Model) :<span style='font-size:30px;'>  <u><b> {game_state$SOLUTION$computed_value} $</b></u></span>  <br> <br></div>")
              )
        )
        
      })
      
      showModal(
        modalDialog(title = NULL,size = "l",fade = FALSE, easyClose = F,
                    
                    
                    div(
                      style="color:white;",
               
                      
                      uiOutput("init_message"), 
                      uiOutput("init_message_goal"), 
                      uiOutput("goal_message"), 
                      
                      
                    )
                    ,
                    footer=
                      tagList(
                        uiOutput("bagchoiceui"), 
                        div(style="float:right;", 
                            
                            actionButton(inputId = "start",label = "Start",icon = icon("play"),class="bag-size")
                        )
                      )
                    
                    
        )
      )
      
    #})
    
    
  })
  
  
  output$bagchoiceui<- renderUI({
    
    b= ifelse(is.null(input$bag_size), "M", input$bag_size)
    
    req(game_state$TOTAL_WEIGHT)
    
    BAG_L <- as.integer( round(game_state$TOTAL_WEIGHT*0.7) ) 
    BAG_M <- as.integer( round(game_state$TOTAL_WEIGHT*0.5) )
    BAG_S <- as.integer( round(game_state$TOTAL_WEIGHT*0.3) )
    
    if(b=="M"){
      div(style="float:left;",   
          div(class = "bag-size-container",
              div(class = "bag-size", `data-size` = "L", onclick = "selectBagSize(this)",
                  div(class = "bag-emoji", "🎒"),
                  div(style='position:fixed;bottom:16px;', paste0("L: ",BAG_L, "Kg" ))
              ),
              div(class = "bag-size selected", `data-size` = "M", onclick = "selectBagSize(this)",
                  div(class = "bag-emoji", "🎒"),
                  div(style='position:fixed;bottom:16px;',paste0("M: ",BAG_M, "Kg" ))
              ),
              div(class = "bag-size", `data-size` = "S", onclick = "selectBagSize(this)",
                  div(class = "bag-emoji", "🎒"),
                  div(style='position:fixed;bottom:16px;', paste0("S: ",BAG_S, "Kg" ))
              )
          )
      )
    }else{
      if(b=="L"){
        div(style="float:left;",   
            div(class = "bag-size-container",
                div(class = "bag-size selected", `data-size` = "L", onclick = "selectBagSize(this)",
                    div(class = "bag-emoji", "🎒"),
                    div(style='position:fixed;bottom:16px;',paste0("L: ",BAG_L, "Kg" ))
                ),
                div(class = "bag-size", `data-size` = "M", onclick = "selectBagSize(this)",
                    div(class = "bag-emoji", "🎒"),
                    div(style='position:fixed;bottom:16px;',paste0("M: ",BAG_M, "Kg" ))
                ),
                div(class = "bag-size", `data-size` = "S", onclick = "selectBagSize(this)",
                    div(class = "bag-emoji", "🎒"),
                    div(style='position:fixed;bottom:16px;',paste0("S: ",BAG_S, "Kg" ))
                )
            )
        )
        
      }else{
        div(style="float:left;",   
            div(class = "bag-size-container",
                div(class = "bag-size", `data-size` = "L", onclick = "selectBagSize(this)",
                    div(class = "bag-emoji", "🎒"),
                    div(style='position:fixed;bottom:16px;',paste0("L: ",BAG_L, "Kg" ))
                ),
                div(class = "bag-size", `data-size` = "M", onclick = "selectBagSize(this)",
                    div(class = "bag-emoji", "🎒"),
                    div(style='position:fixed;bottom:16px;',paste0("M: ",BAG_M, "Kg" ))
                ),
                div(class = "bag-size selected", `data-size` = "S", onclick = "selectBagSize(this)",
                    div(class = "bag-emoji", "🎒"),
                    div(style='position:fixed;bottom:16px;',paste0("S: ",BAG_S, "Kg" ))
                )
            )
        )
        
      }
      
    }
    
    
    
  })
  
  output$goal <- renderUI({
    req(input$start)
    
    absolutePanel(top = 10,right = 10,fixed = T, 
                  div(
                    style=glue('border-radius:10px;width:200px;margin:1rem;font-size:38px;color:white;'),
                    div(
                      style="margin:1rem;text-align:center;padding-top:10px;",
                      HTML(paste0( "🎯 : " ,  game_state$SOLUTION$computed_value , '$' ))
                    )
                    
                  )
                  
    )
    
  })
  
  observeEvent(input$start, {
    
    removeModal()
    is_game_active(TRUE)
    
    
  })
  
  output$goal_score <- renderUI({
    
    req(input$start)
    
    s=0
    if(!is.null(input$rank_list_2)){
      s <- round( calculate_selection_stats(input$rank_list_2,items_table_par = game_state$items_table )[["value"]]*100/game_state$SOLUTION$computed_value, 1)
    }
    
    
    absolutePanel(top = 10,right = "44%",fixed = T, 
                  div(
                    style=glue('border-radius:10px;width:300px;margin:1rem;font-size:38px;color:white;'),
                    div(
                      style="margin:1rem;text-align:center;padding-top:10px;",
                      HTML(paste0( "Score : " ,s  , '%' ))
                    )
                    
                  )
                  
    )
    
  })
  
  
  output$play_area <- renderUI({
    
    fluidRow(
      column(
        
        width = 12,
        bucket_list(
          header = "Drag the items in  the bag",
          group_name = "bucket_list_group",
          orientation = "horizontal",
          options =sortable_options(sort = F,disabled = T, animation=2000, multiDrag=T), 
          add_rank_list(
            text = "Items in this house",
            labels = game_state$items_list,
            input_id = "rank_list_1"
          ),
          add_rank_list(
            text = "Your Bag",
            labels = NULL,
            input_id = "rank_list_2"
          )
        )
      )
    )
    
    
  })
  
  observe({
    req(input$rank_list_1)
    session$sendCustomMessage(type = 'initializeUpdateFunction', message = list())
  })
  
  output$score_house <- renderUI({
    
    req(input$rank_list_1)
    
    k=calculate_selection_stats(input$rank_list_1,items_table_par = game_state$items_table)
    div(
      column(6,
             div(
               
               class='score_house', 
               HTML( paste0("$ :  ", k[["value"]],"/", game_state$TOTAL_VALUE ) )
               
             )
      ), 
      column(6,
             div(
               
               class='score_house', 
               HTML( paste0(icon("weight-scale"), "  :  ", k[["weight"]] , "/" , game_state$TOTAL_WEIGHT) )
               
             )
      )
    )
    
  })
  
  
  output$score_thief <- renderUI({
    
    req(input$rank_list_2)
    
    k2= calculate_selection_stats(input$rank_list_2,items_table_par = game_state$items_table)
    tot_weigth= k2[["weight"]] 
    pct_weight= 100*tot_weigth/game_state$MAX_WEIGHT
    
    color_progress <- ifelse(pct_weight<=60,"darkgreen", 
                             ifelse(pct_weight<=80, "orange", "red" )
    )
    
    cls = ifelse(pct_weight>100 & timer()!=0, "shake-top","")
    
    div(
      column(6,
             div(
               style='border-radius:10px;background-color:#43244e;width:100%;font-size: 36px;text-align: center;color:white;border: double;', 
               
               HTML( paste0("$  :  ", k2[["value"]]  ) )
               
             )
      ),
      column(6,
             
             div(class =glue("progress-container {cls}"),
                 div(style = glue("background-color: {color_progress};
                          height: 100%;
    border-radius: 10px;
    position: absolute;
    top: 0;
    left: 0;
    z-index: 1;width: {100*tot_weigth/game_state$MAX_WEIGHT}%;")),
                 div(class = "progress-content",
                     tags$i(class = "fas fa-weight-scale", `aria-label` = "weight-scale icon"), 
                     paste0( " :  ", tot_weigth , "/",game_state$MAX_WEIGHT)
                 )
             )
             
             
             
      )
      
    )
    
    
    
  })
  
  
  output$score<- renderUI({
    
    
    
    fluidRow(
      column(6, 
             uiOutput("score_house")
      ), 
      column(6, 
             uiOutput("score_thief")
      )
    )
    
  })
  
  
  
}
