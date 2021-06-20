# Server: Final Project: Startup Data Visualization

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
     mouse_data <- reactiveValues(
       x_coord = NULL,
       y_coord = NULL,
      labelled_points = NULL
     )

    output$distPlot <- renderPlot({
      
      if (input$location == "SF") {
        
        data <- SF_map
        g <- ggplot() +
           geom_polygon(data = data, aes(x = long, y = lat, group = group, alpha = .5),fill="skyblue", color="white") +
           coord_cartesian(ylim=c(37,38), xlim = c(-123,-121))+
          theme(legend.position = "none")
      }
       
      if(!is.null(input$data)) {
        
        if(input$size_option == "No") { 
          selected_data <- full_data %>%
          filter(type %in% input$data)
          
          g <- g + geom_point(data = selected_data, aes(x=long, y=lat, color = type, size = funding_total_usd))
          
        } else if(input$size_option == "Yes")  {
          selected_data <- full_data %>%
            filter(type %in% input$data) %>%
            filter(funding_total_usd >= (input$size)*1000000)
          g <- g + geom_point(data = selected_data, aes(x=long, y=lat, color = type, size = funding_total_usd))
        } 
        
        if (!is.null(mouse_data$labelled_points)){
            g <- g + geom_label(data=mouse_data$labelled_points,
                                mapping = aes(x = long, y = lat,label = paste(`Company Name`,":",type))) 
          }}
        g
     })
    
       observeEvent(input$click_point,{
         #selected_data <- full_data %>%
        #   filter(type %in% input$data) %>%
         #  filter(funding_total_usd >= (input$size)*1000000)
         
          if(!is.null(selected_data)) {
            clicked_row <- nearPoints(selected_data,input$click_point,
                                   threshold=50,maxpoints=1, allRows = F)
          }
       
         mouse_data$labelled_points <- rbind(mouse_data$labelled_points, clicked_row)
         mouse_data$x_coord <- input$click_point$x
         mouse_data$y_coord <- input$click_point$y
        
    })
    
    output$tableInfo <- renderTable({
      mouse_data$labelled_points
    })
    
    observeEvent(input$reset,{
      mouse_data$labelled_points = NULL
    })
    
#    observeEvent(input$zoom,{
#      mouse_data$zoom <- T
#      mouse_data$xlim <- c(input$brush_box$xmin, input$brush_box$xmax)
#      mouse_data$ylim <- c(input$brush_box$ymin, input$brush_box$ymax)
#      session$resetBrush("brush_box")
#    })
#    
 #   observeEvent(input$xVar,{
 #     mouse_data$zoom = F
 #     mouse_data$xlim = c(NA,NA)
 #     mouse_data$ylim = c(NA,NA)
 #     session$resetBrush("brush_box")
 #   })
 #   
 #   observeEvent(input$yVar,{
 #     mouse_data$zoom = F
 #     mouse_data$xlim = c(NA,NA)
 #     mouse_data$ylim = c(NA,NA)
 #     session$resetBrush("brush_box")
 #   })
#   
#   observeEvent(input$unzoom,{
#     mouse_data$zoom = F
#     mouse_data$xlim = c(NA,NA)
#     mouse_data$ylim = c(NA,NA)
#     session$resetBrush("brush_box")
#   })
    
#    output$brushedInfo <- renderDataTable({
#      mouse_data$brushed_data
#    })
})
