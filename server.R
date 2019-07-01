#
# This is the server logic of a Shiny web application.
# load pacakages required
library(shiny) # to run the app
library(dplyr) # to filter data
library(xts) # to manipulate time-series object
library(dygraphs) # to create a time-series graph
library(ggplot2)

# read the input file
Visualisationdata_final <- read.csv("Newyork_data.csv")
# define the latitude na dlongitude for all the locations.
x<-data.frame("borough"=c('Manhattan','Bronx','Brooklyn','Queens'),"long"=c(-73.97125,-73.86483,-73.94416,-73.79485),"lat"=c(40.78306,40.84478,40.67818,40.72822))
shinyServer(function(input, output) {
  # leaflet plot for QUEENS borough
    output$map<- renderLeaflet({ 
            # filter the data based on time stamp
            if (input$TimeStamp==5)
            {
                leaf_df <- Visualisationdata_final [ which((Visualisationdata_final$borough=="Queens")),]
                
            }
            else
            { 
                leaf_df <- Visualisationdata_final [ which((Visualisationdata_final$borough=="Queens") & (Visualisationdata_final$TimeOfDay==input$TimeStamp)),]
            } 
        df<- leaf_df %>% group_by(borough) %>% summarize(n=n()) # calulate thenumber of trips for the given borough
        df_merged<-merge(x=x,y=df,by="borough")
        if (input$TimeStamp %in%  c(1,2,5))
        { # if the time stamp is morning or afternoon show the day theme
            
            leaflet(leaf_df) %>%
                addProviderTiles(providers$OpenStreetMap.Mapnik)  %>% # specify type of map
                addCircles(~pickup_longitude, ~pickup_latitude,color = "orange",data = leaf_df,fillOpacity = 0.000001) %>%
                setView(-73.98, 40.75, zoom = 11)%>% # set view
                addMarkers( # add markers
                    ~long,~lat,data=df_merged,
                    label =~borough,
                    labelOptions = labelOptions(noHide = T,textsize = 30),popup = ~ paste("Average Number of Trips",n))
            
        }
        else
        { #if the time stamp is evening  or night show the day theme
            leaflet(leaf_df) %>%
                addProviderTiles(providers$Esri.WorldImagery)  %>% # specify type of map
                addCircles(~pickup_longitude, ~pickup_latitude,color = "orange",data = leaf_df,fillOpacity = 0.000001) %>%
                setView(-73.98, 40.75, zoom = 11)%>%
                addMarkers(
                    ~long,~lat,data=df_merged,
                    label =~borough,
                    labelOptions = labelOptions(noHide = T,textsize = 30),popup = ~ paste("Average Number of Trips",n))
        }
        
    })
    #########################
    # fuction to filter data and display number of trips in a given borough
    searchResult<- reactive({
        
          # filter according to time stamp
          if (input$TimeStamp==5)
            {
                leaf_df <- Visualisationdata_final [ which(Visualisationdata_final$borough=="Queens"),]
                
            }
            else
            {
                leaf_df <- Visualisationdata_final [ which((Visualisationdata_final$borough=="Queens" )& (Visualisationdata_final$TimeOfDay==input$TimeStamp)),]
            }
        # find total number of trips
        if (length(leaf_df)!=0)
        {
            df<- leaf_df %>% group_by(borough) %>% summarize(n=n())
        }
        else
        {
            leaf_df <- Visualisationdata_final 
            df<- leaf_df %>% group_by(borough) %>% summarize(n=n())
        }
        colnames(df)[1]<-"Borough"
        colnames(df)[2]<-"Total Number Of taxi-trips Over the years"
        
        df
    })
    ###############################
    # print the dataframe obatined from the above search result function
    output$table <- renderTable(
        {
            
            searchResult()
        })
    ##########################
    # bar chart to see the trend
    output$barchart <- renderPlotly({ 
      #filter the data according to time
    if (input$TimeStamp==5)
    {
    ploty_data <- Visualisationdata_final %>% filter(borough=="Queens") %>% group_by(year) %>% summarize(n=n())
    }
    else
    {
    ploty_data <- Visualisationdata_final %>% filter(borough=="Queens" & TimeOfDay == input$TimeStamp) %>% group_by(year) %>% summarize(n=n())
        
    }
      # call plotly function to make it interactive
    plot_ly(x = ploty_data$year, y=ploty_data$n,type = "bar")
    })
    #####################
    #time series graph
    output$timeseries <- renderDygraph({
      # filter dat abased on number of trips in a given time .
        Visualisationdata_final$pickup_time <- as.POSIXct(Visualisationdata_final$pickup_time,format="%H:%M:%OS") 
        sss<-Visualisationdata_final %>% filter(borough=="Queens") %>% group_by(pickup_time) %>% summarize(n=n())
        don=xts(x = sss$n, order.by = sss$pickup_time) # convert it to time-series
        # dygraph function toplot time series
        dygraph(don) %>%
            dyOptions(labelsUTC = FALSE, fillGraph=TRUE, fillAlpha=0.5, drawGrid = FALSE, colors="#009999") %>%
            dyRangeSelector() %>%
            dyCrosshair(direction = "vertical") %>%
            dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
            dyRoller(rollPeriod = 1)
    })
    #############################
    # value box to render  time at which traffic is maximum
    output$box2Q<- renderValueBox({
      #convert to date time
        Visualisationdata_final$pickup_time <- as.POSIXct(Visualisationdata_final$pickup_time,format="%H:%M:%OS") 
        # filter data according to borough
        sss<-Visualisationdata_final %>% filter(borough=="Queens") %>% group_by(pickup_time) %>% summarize(n=n())
        val <- sss [ which(sss$n==max(sss$n)),]
  
        
        valueBox(
            value = strftime(val$pickup_time, format="%H:%M:%S") 
            ,subtitle = "Time at which taxi-traffice is Maximum"
            ,color = "green"
            ,icon = icon("time")
        )})
        
    ###############################
    # value box to render  time at which traffic is minimum
    output$box2q2<- renderValueBox({
        Visualisationdata_final$pickup_time <- as.POSIXct(Visualisationdata_final$pickup_time,format="%H:%M:%OS") 
        sss<-Visualisationdata_final %>% filter(borough=="Queens") %>% group_by(pickup_time) %>% summarize(n=n())
        val <- sss [ which(sss$n==min(sss$n)),]
        
        
        valueBox(
            value = strftime(val$pickup_time, format="%H:%M:%S") 
            ,subtitle = "Time at which taxi-traffice is Minimum"
            ,color = "green"
            ,icon = icon("time")
        )})
    ###################################
    # prediction plot based on parameters selected
    output$farepredictionq <- renderPlot({
   
    if(input$Parameters==2)
    { # ggplot to plot scatter plot based on time stamp and the measure to be ploted against
      if (input$TimeStamp_q == 5)
      {
        ggplot(Visualisationdata_final%>% filter(fare_amount<300  & borough=="Queens" ),aes(x=fare_amount,y=Travel_time,alpha =0.1))+
          geom_point()+geom_smooth(method = input$Smoothers) + geom_jitter() +theme(legend.position="none")
      }
      else
      {
        ggplot(Visualisationdata_final%>% filter(fare_amount<300  & borough=="Queens" & TimeOfDay ==input$TimeStamp_q),aes(x=fare_amount,y=Travel_time,alpha =0.1))+
          geom_point()+geom_smooth(method = input$Smoothers) + geom_jitter() +theme(legend.position="none")
      } 
    }
      else
      {
        
          if (input$TimeStamp_q == 5)
          {
            ggplot(Visualisationdata_final%>% filter(fare_amount<300 &  trip_distance<100 & borough=="Queens" ),aes(x=fare_amount,y=trip_distance,color='red',alpha =0.1))+
              geom_point()+geom_smooth(method = input$Smoothers,color="black") + geom_jitter() +theme(legend.position="none")
          }
          else
          {
            ggplot(Visualisationdata_final%>% filter(fare_amount<300 &  trip_distance<100 & borough=="Queens" & TimeOfDay ==input$TimeStamp_q),aes(x=fare_amount,y=trip_distance,color='red',alpha =0.1))+
              geom_point()+geom_smooth(method = input$Smoothers,color="black") + geom_jitter() +theme(legend.position="none")
          } 
        
        
      }
    })
    ################################
    #value box to render  time at which traffic is minimum
    output$boxfairpred<- renderValueBox({
      pred1<-Visualisationdata_final %>% filter(borough=="Queens") 
      model = lm(fare_amount~trip_distance, data = pred1) #Create the linear regression
      distPred <- predict(model, data.frame("trip_distance"=c(input$integer))) # predict value based on value s;ected by the user
      
      
      valueBox(
        value = distPred[1]
        ,subtitle = "Predict fare :"
        ,color = "green"
        ,icon = icon("dollar")
      )})
    
    ###################################
    #the same above function are repeated for different boroughs
    #map for Brooklyn
    output$mapb<- renderLeaflet({ 
      if (input$TimeStampb==5)
      {
        leaf_df <- Visualisationdata_final [ which((Visualisationdata_final$borough=="Brooklyn")),]
        
      }
      else
      {
        leaf_df <- Visualisationdata_final [ which((Visualisationdata_final$borough=="Brooklyn") & (Visualisationdata_final$TimeOfDay==input$TimeStampb)),]
      } 
      df<- leaf_df %>% group_by(borough) %>% summarize(n=n())
      df_merged<-merge(x=x,y=df,by="borough")
      if (input$TimeStampb %in%  c(1,2,5))
      {
        
        leaflet(leaf_df) %>%
          addProviderTiles(providers$OpenStreetMap.Mapnik)  %>%
          addCircles(~pickup_longitude, ~pickup_latitude,color = "orange",data = leaf_df,fillOpacity = 0.000001) %>%
          setView(-73.98, 40.75, zoom = 11)%>%
          addMarkers(
            ~long,~lat,data=df_merged,
            label =~borough,
            labelOptions = labelOptions(noHide = T,textsize = 30),popup = ~ paste("Average Number of Trips",n))
        
      }
      else
      {
        leaflet(leaf_df) %>%
          addProviderTiles(providers$Esri.WorldImagery)  %>%
          addCircles(~pickup_longitude, ~pickup_latitude,color = "orange",data = leaf_df,fillOpacity = 0.000001) %>%
          setView(-73.98, 40.75, zoom = 11)%>%
          addMarkers(
            ~long,~lat,data=df_merged,
            label =~borough,
            labelOptions = labelOptions(noHide = T,textsize = 30),popup = ~ paste("Average Number of Trips",n))
      }
      
    })
    # fuction to filter data and display number of trips in a given borough
    searchResultb<- reactive({
      
      
      if (input$TimeStampb==5)
      {
        leaf_df <- Visualisationdata_final [ which(Visualisationdata_final$borough=="Brooklyn"),]
        
      }
      else
      {
        leaf_df <- Visualisationdata_final [ which((Visualisationdata_final$borough=="Brooklyn" )& (Visualisationdata_final$TimeOfDay==input$TimeStampb)),]
      }
      
      if (length(leaf_df)!=0)
      {
        df<- leaf_df %>% group_by(borough) %>% summarize(n=n())
      }
      else
      {
        leaf_df <- Visualisationdata_final 
        df<- leaf_df %>% group_by(borough) %>% summarize(n=n())
      }
      colnames(df)[1]<-"Borough"
      colnames(df)[2]<-"Total Number Of taxi-trips Over the years"
      
      df
    })
    #################################################
    # # print the dataframe obatined from the above search result function
    output$tableb <- renderTable(
      {
        
        searchResultb()
      })  
     #######################
    #interactive bar chart
    output$barchartb <- renderPlotly({ 
      if (input$TimeStampb==5)
      {
        ploty_data <- Visualisationdata_final %>% filter(borough=="Brooklyn") %>% group_by(year) %>% summarize(n=n())
      }
      else
      {
        ploty_data <- Visualisationdata_final %>% filter(borough=="Brooklyn" & TimeOfDay == input$TimeStampb) %>% group_by(year) %>% summarize(n=n())
        
      }
      plot_ly(x = ploty_data$year, y=ploty_data$n,type = "bar")
    })
    ############################
    #time series graph
    output$timeseriesb <- renderDygraph({
  Visualisationdata_final$pickup_time <- as.POSIXct(Visualisationdata_final$pickup_time,format="%H:%M:%OS") 
  sss<-Visualisationdata_final %>% filter(borough=="Brooklyn") %>% group_by(pickup_time) %>% summarize(n=n())
  don=xts(x = sss$n, order.by = sss$pickup_time)
  dygraph(don) %>%
    dyOptions(labelsUTC = FALSE, fillGraph=TRUE, fillAlpha=0.5, drawGrid = FALSE, colors="#009999") %>%
    dyRangeSelector() %>%
    dyCrosshair(direction = "vertical") %>%
    dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
    dyRoller(rollPeriod = 1)
})
   ########### 
    # value box to render  time at which traffic is maximum
    output$box2Qb<- renderValueBox({
      Visualisationdata_final$pickup_time <- as.POSIXct(Visualisationdata_final$pickup_time,format="%H:%M:%OS") 
      sss<-Visualisationdata_final %>% filter(borough=="Brooklyn") %>% group_by(pickup_time) %>% summarize(n=n())
      val <- sss [ which(sss$n==max(sss$n)),]
      
      
      valueBox(
        value = strftime(val$pickup_time, format="%H:%M:%S") 
        ,subtitle = "Time at which taxi-traffice is Maximum"
        ,color = "green"
        ,icon = icon("time")
      )})
    #############
    # value box to render  time at which traffic is minimum
    output$box2q2b<- renderValueBox({
      Visualisationdata_final$pickup_time <- as.POSIXct(Visualisationdata_final$pickup_time,format="%H:%M:%OS") 
      sss<-Visualisationdata_final %>% filter(borough=="Brooklyn") %>% group_by(pickup_time) %>% summarize(n=n())
      val <- sss [ which(sss$n==min(sss$n)),]
      
      
      valueBox(
        value = strftime(val$pickup_time, format="%H:%M:%S") 
        ,subtitle = "Time at which taxi-traffice is Minimum"
        ,color = "green"
        ,icon = icon("time")
      )})
    ####################3
    # prediction plot based on parameters selected
    output$farepredictionqb <- renderPlot({
      
      if(input$Parametersb==2)
      {
        if (input$TimeStamp_qb == 5)
        {
          ggplot(Visualisationdata_final%>% filter(fare_amount<300  & borough=="Brooklyn" ),aes(x=fare_amount,y=Travel_time,alpha =0.1))+
            geom_point()+geom_smooth(method = input$Smoothersb) + geom_jitter() +theme(legend.position="none")
        }
        else
        {
          ggplot(Visualisationdata_final%>% filter(fare_amount<300  & borough=="Brooklyn" & TimeOfDay ==input$TimeStamp_qb),aes(x=fare_amount,y=Travel_time,alpha =0.1))+
            geom_point()+geom_smooth(method = input$Smoothersb) + geom_jitter() +theme(legend.position="none")
        } 
      }
      else
      {
        
        if (input$TimeStamp_q == 5)
        {
          ggplot(Visualisationdata_final%>% filter(fare_amount<300 &  trip_distance<100 & borough=="Brooklyn" ),aes(x=fare_amount,y=trip_distance,color='red',alpha =0.1))+
            geom_point()+geom_smooth(method = input$Smoothersb,color="black") + geom_jitter() +theme(legend.position="none")
        }
        else
        {
          ggplot(Visualisationdata_final%>% filter(fare_amount<300 &  trip_distance<100 & borough=="Brooklyn" & TimeOfDay ==input$TimeStamp_qb),aes(x=fare_amount,y=trip_distance,color='red',alpha =0.1))+
            geom_point()+geom_smooth(method = input$Smoothersb,color="black") + geom_jitter() +theme(legend.position="none")
        } 
        
        
      }
    })
    ################
    #value box to render  time at which traffic is minimum
    output$boxfairpredb<- renderValueBox({
      pred1<-Visualisationdata_final %>% filter(borough=="Brooklyn") 
      model = lm(fare_amount~trip_distance, data = pred1) #Create the linear regression
      distPred <- predict(model, data.frame("trip_distance"=c(input$integerb)))
      
      
      valueBox(
        value = distPred[1]
        ,subtitle = "Predict fare :"
        ,color = "green"
        ,icon = icon("dollar")
      )})
    ####################
    # repeat the same for Bronx
    #map for Bronx
    output$mapbro<- renderLeaflet({ 
      if (input$TimeStampb==5)
      {
        leaf_df <- Visualisationdata_final [ which((Visualisationdata_final$borough=="Bronx")),]
        
      }
      else
      {
        leaf_df <- Visualisationdata_final [ which((Visualisationdata_final$borough=="Bronx") & (Visualisationdata_final$TimeOfDay==input$TimeStampbro)),]
      } 
      df<- leaf_df %>% group_by(borough) %>% summarize(n=n())
      df_merged<-merge(x=x,y=df,by="borough")
      if (input$TimeStampbro %in%  c(1,2,5))
      {
        
        leaflet(leaf_df) %>%
          addProviderTiles(providers$OpenStreetMap.Mapnik)  %>%
          addCircles(~pickup_longitude, ~pickup_latitude,color = "orange",data = leaf_df,fillOpacity = 0.000001) %>%
          setView(-73.98, 40.75, zoom = 11)%>%
          addMarkers(
            ~long,~lat,data=df_merged,
            label =~borough,
            labelOptions = labelOptions(noHide = T,textsize = 30),popup = ~ paste("Average Number of Trips",n))
        
      }
      else
      {
        leaflet(leaf_df) %>%
          addProviderTiles(providers$Esri.WorldImagery)  %>%
          addCircles(~pickup_longitude, ~pickup_latitude,color = "orange",data = leaf_df,fillOpacity = 0.000001) %>%
          setView(-73.98, 40.75, zoom = 11)%>%
          addMarkers(
            ~long,~lat,data=df_merged,
            label =~borough,
            labelOptions = labelOptions(noHide = T,textsize = 30),popup = ~ paste("Average Number of Trips",n))
      }
      
    })
    #############################
    # fuction to filter data and display number of trips in a given borough
    searchResultbro<- reactive({
      
      
      if (input$TimeStampbro==5)
      {
        leaf_df <- Visualisationdata_final [ which(Visualisationdata_final$borough=="Bronx"),]
        
      }
      else
      {
        leaf_df <- Visualisationdata_final [ which((Visualisationdata_final$borough=="Bronx" )& (Visualisationdata_final$TimeOfDay==input$TimeStampbro)),]
      }
      
      if (length(leaf_df)!=0)
      {
        df<- leaf_df %>% group_by(borough) %>% summarize(n=n())
      }
      else
      {
        leaf_df <- Visualisationdata_final 
        df<- leaf_df %>% group_by(borough) %>% summarize(n=n())
      }
      colnames(df)[1]<-"Borough"
      colnames(df)[2]<-"Total Number Of taxi-trips Over the years"
      
      df
    })
    ################################
    # display table obatined from searchResultbro
    output$tablebro <- renderTable({
        
        searchResultbro()
      })  
    #################
    #interactive bar cahrt
    output$barchartbro <- renderPlotly({ 
      if (input$TimeStampbro==5)
      {
        ploty_data <- Visualisationdata_final %>% filter(borough=="Bronx") %>% group_by(year) %>% summarize(n=n())
      }
      else
      {
        ploty_data <- Visualisationdata_final %>% filter(borough=="Bronx" & TimeOfDay == input$TimeStampbro) %>% group_by(year) %>% summarize(n=n())
        
      }
      plot_ly(x = ploty_data$year, y=ploty_data$n,type = "bar")
    })
    ############################
    #time series graoh
    output$timeseriesbro <- renderDygraph({
      Visualisationdata_final$pickup_time <- as.POSIXct(Visualisationdata_final$pickup_time,format="%H:%M:%OS") 
      sss<-Visualisationdata_final %>% filter(borough=="Bronx") %>% group_by(pickup_time) %>% summarize(n=n())
      don=xts(x = sss$n, order.by = sss$pickup_time)
      dygraph(don) %>%
        dyOptions(labelsUTC = FALSE, fillGraph=TRUE, fillAlpha=0.5, drawGrid = FALSE, colors="#009999") %>%
        dyRangeSelector() %>%
        dyCrosshair(direction = "vertical") %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
        dyRoller(rollPeriod = 1)
    })
    ########### 
    # time at which trip count is maximum
    output$box2Qbro<- renderValueBox({
      Visualisationdata_final$pickup_time <- as.POSIXct(Visualisationdata_final$pickup_time,format="%H:%M:%OS") 
      sss<-Visualisationdata_final %>% filter(borough=="Bronx") %>% group_by(pickup_time) %>% summarize(n=n())
      val <- sss [ which(sss$n==max(sss$n)),]
      
      
      valueBox(
        value = strftime(val$pickup_time, format="%H:%M:%S") 
        ,subtitle = "Time at which taxi-traffice is Maximum"
        ,color = "green"
        ,icon = icon("time")
      )})
    #############
    # time at which trip count is minimum
    output$box2q2bro<- renderValueBox({
      Visualisationdata_final$pickup_time <- as.POSIXct(Visualisationdata_final$pickup_time,format="%H:%M:%OS") 
      sss<-Visualisationdata_final %>% filter(borough=="Bronx") %>% group_by(pickup_time) %>% summarize(n=n())
      val <- sss [ which(sss$n==min(sss$n)),]
      
      
      valueBox(
        value = strftime(val$pickup_time, format="%H:%M:%S") 
        ,subtitle = "Time at which taxi-traffice is Minimum"
        ,color = "green"
        ,icon = icon("time")
      )})
    ####################3
    #scatter plot for fare prediction
    output$farepredictionqbro <- renderPlot({
      
      if(input$Parametersbro==2)
      {
        if (input$TimeStamp_qbro == 5)
        {
          ggplot(Visualisationdata_final%>% filter(fare_amount<300  & borough=="Bronx" ),aes(x=fare_amount,y=Travel_time,alpha =0.1))+
            geom_point()+geom_smooth(method = input$Smoothersbro) + geom_jitter() +theme(legend.position="none")
        }
        else
        {
          ggplot(Visualisationdata_final%>% filter(fare_amount<300  & borough=="Bronx" & TimeOfDay ==input$TimeStamp_qbro),aes(x=fare_amount,y=Travel_time,alpha =0.1))+
            geom_point()+geom_smooth(method = input$Smoothersbro) + geom_jitter() +theme(legend.position="none")
        } 
      }
      else
      {
        
        if (input$TimeStamp_qbro == 5)
        {
          ggplot(Visualisationdata_final%>% filter(fare_amount<300 &  trip_distance<100 & borough=="Bronx" ),aes(x=fare_amount,y=trip_distance,color='red',alpha =0.1))+
            geom_point()+geom_smooth(method = input$Smoothersbro,color="black") + geom_jitter() +theme(legend.position="none")
        }
        else
        {
          ggplot(Visualisationdata_final%>% filter(fare_amount<300 &  trip_distance<100 & borough=="Bronx" & TimeOfDay ==input$TimeStamp_qbro),aes(x=fare_amount,y=trip_distance,color='red',alpha =0.1))+
            geom_point()+geom_smooth(method = input$Smoothersbro,color="black") + geom_jitter() +theme(legend.position="none")
        } 
        
        
      }
    })
    ################
    # predicted fare value
    output$boxfairpredbro<- renderValueBox({
      pred1<-Visualisationdata_final %>% filter(borough=="Bronx") 
      model = lm(fare_amount~trip_distance, data = pred1) #Create the linear regression
      distPred <- predict(model, data.frame("trip_distance"=c(input$integerbro)))
      
      
      valueBox(
        value = distPred[1]
        ,subtitle = "Predict fare :"
        ,color = "green"
        ,icon = icon("dollar")
      )})
    ################
    ##repeat same for Manhattan
    # map for Manhattan
    output$mapmah<- renderLeaflet({ 
      if (input$TimeStampman==5)
      {
        leaf_df <- Visualisationdata_final [ which((Visualisationdata_final$borough=="Manhattan")),]
        
      }
      else
      {
        leaf_df <- Visualisationdata_final [ which((Visualisationdata_final$borough=="Manhattan") & (Visualisationdata_final$TimeOfDay==input$TimeStampman)),]
      } 
      df<- leaf_df %>% group_by(borough) %>% summarize(n=n())
      df_merged<-merge(x=x,y=df,by="borough")
      if (input$TimeStampman %in%  c(1,2,5))
      {
        
        leaflet(leaf_df) %>%
          addProviderTiles(providers$OpenStreetMap.Mapnik)  %>%
          addCircles(~pickup_longitude, ~pickup_latitude,color = "orange",data = leaf_df,fillOpacity = 0.000001) %>%
          setView(-73.98, 40.75, zoom = 11)%>%
          addMarkers(
            ~long,~lat,data=df_merged,
            label =~borough,
            labelOptions = labelOptions(noHide = T,textsize = 30),popup = ~ paste("Average Number of Trips",n))
        
      }
      else
      {
        leaflet(leaf_df) %>%
          addProviderTiles(providers$Esri.WorldImagery)  %>%
          addCircles(~pickup_longitude, ~pickup_latitude,color = "orange",data = leaf_df,fillOpacity = 0.000001) %>%
          setView(-73.98, 40.75, zoom = 11)%>%
          addMarkers(
            ~long,~lat,data=df_merged,
            label =~borough,
            labelOptions = labelOptions(noHide = T,textsize = 30),popup = ~ paste("Average Number of Trips",n))
      }
      
    })
    #######################
    # search results(trip count according to filer)
    searchResultman<- reactive({
      
      
      if (input$TimeStampman==5)
      {
        leaf_df <- Visualisationdata_final [ which(Visualisationdata_final$borough=="Manhattan"),]
        
      }
      else
      {
        leaf_df <- Visualisationdata_final [ which((Visualisationdata_final$borough=="Manhattan" )& (Visualisationdata_final$TimeOfDay==input$TimeStampman)),]
      }
      
      if (length(leaf_df)!=0)
      {
        df<- leaf_df %>% group_by(borough) %>% summarize(n=n())
      }
      else
      {
        leaf_df <- Visualisationdata_final 
        df<- leaf_df %>% group_by(borough) %>% summarize(n=n())
      }
      colnames(df)[1]<-"Borough"
      colnames(df)[2]<-"Total Number Of taxi-trips Over the years"
      
      df
    })
    #################################################
    # print datfarme obtained from searchResultman
    output$tableman <- renderTable({
        
        searchResultman()
      })  
    #######################
    # interactive bar graph
    output$barchartman <- renderPlotly({ 
      if (input$TimeStampman==5)
      {
        ploty_data <- Visualisationdata_final %>% filter(borough=="Manhattan") %>% group_by(year) %>% summarize(n=n())
      }
      else
      {
        ploty_data <- Visualisationdata_final %>% filter(borough=="Manhattan" & TimeOfDay == input$TimeStampman) %>% group_by(year) %>% summarize(n=n())
        
      }
      plot_ly(x = ploty_data$year, y=ploty_data$n,type = "bar")
    })
    ############################
    #time series graph
    output$timeseriesman <- renderDygraph({
      Visualisationdata_final$pickup_time <- as.POSIXct(Visualisationdata_final$pickup_time,format="%H:%M:%OS") 
      sss<-Visualisationdata_final %>% filter(borough=="Manhattan") %>% group_by(pickup_time) %>% summarize(n=n())
      don=xts(x = sss$n, order.by = sss$pickup_time)
      dygraph(don) %>%
        dyOptions(labelsUTC = FALSE, fillGraph=TRUE, fillAlpha=0.5, drawGrid = FALSE, colors="#009999") %>%
        dyRangeSelector() %>%
        dyCrosshair(direction = "vertical") %>%
        dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
        dyRoller(rollPeriod = 1)
    })
    ########### 
    #time at which traffic is maximum
    output$box2Qman<- renderValueBox({
      Visualisationdata_final$pickup_time <- as.POSIXct(Visualisationdata_final$pickup_time,format="%H:%M:%OS") 
      sss<-Visualisationdata_final %>% filter(borough=="Manhattan") %>% group_by(pickup_time) %>% summarize(n=n())
      val <- sss [ which(sss$n==max(sss$n)),]
      
      
      valueBox(
        value = strftime(val$pickup_time, format="%H:%M:%S") 
        ,subtitle = "Time at which taxi-traffice is Maximum"
        ,color = "green"
        ,icon = icon("time")
      )})
    #############
    #time at which traffic is minimum
    output$box2q2man<- renderValueBox({
      Visualisationdata_final$pickup_time <- as.POSIXct(Visualisationdata_final$pickup_time,format="%H:%M:%OS") 
      sss<-Visualisationdata_final %>% filter(borough=="Manhattan") %>% group_by(pickup_time) %>% summarize(n=n())
      val <- sss [ which(sss$n==min(sss$n)),]
      
      
      valueBox(
        value = strftime(val$pickup_time, format="%H:%M:%S") 
        ,subtitle = "Time at which taxi-traffice is Minimum"
        ,color = "green"
        ,icon = icon("time")
      )})
    ####################3
    #scatter plot
    output$farepredictionqman <- renderPlot({
      
      if(input$Parametersman==2)
      {
        if (input$TimeStamp_qman == 5)
        {
          ggplot(Visualisationdata_final%>% filter(fare_amount<300  & borough=="Manhattan" ),aes(x=fare_amount,y=Travel_time,alpha =0.1))+
            geom_point()+geom_smooth(method = input$Smoothersman) + geom_jitter() +theme(legend.position="none")
        }
        else
        {
          ggplot(Visualisationdata_final%>% filter(fare_amount<300  & borough=="Manhattan" & TimeOfDay ==input$TimeStamp_qman),aes(x=fare_amount,y=Travel_time,alpha =0.1))+
            geom_point()+geom_smooth(method = input$Smoothersman) + geom_jitter() +theme(legend.position="none")
        } 
      }
      else
      {
        
        if (input$TimeStamp_qman == 5)
        {
          ggplot(Visualisationdata_final%>% filter(fare_amount<300 &  trip_distance<100 & borough=="Manhattan" ),aes(x=fare_amount,y=trip_distance,color='red',alpha =0.1))+
            geom_point()+geom_smooth(method = input$Smoothersman,color="black") + geom_jitter() +theme(legend.position="none")
        }
        else
        {
          ggplot(Visualisationdata_final%>% filter(fare_amount<300 &  trip_distance<100 & borough=="Manhattan" & TimeOfDay ==input$TimeStamp_qman),aes(x=fare_amount,y=trip_distance,color='red',alpha =0.1))+
            geom_point()+geom_smooth(method = input$Smoothersman,color="black") + geom_jitter() +theme(legend.position="none")
        } 
        
        
      }
    })
    ################
    # predicted fare value
    output$boxfairpredman<- renderValueBox({
      pred1<-Visualisationdata_final %>% filter(borough=="Manhattan") 
      model = lm(fare_amount~trip_distance, data = pred1) #Create the linear regression
      distPred <- predict(model, data.frame("trip_distance"=c(input$integerman)))
      
      
      valueBox(
        value = distPred[1]
        ,subtitle = "Predict fare :"
        ,color = "green"
        ,icon = icon("dollar")
      )})
    ###############
    #Circular plot
    output$circular <- renderPlot(
      {
       
       val=""
        
      #Filter data based on parameterd slected
        if (input$polar==1)
        { val="Travel Time :"
          data1<-Visualisationdata_final %>% group_by(borough,year) %>% summarise(fare=mean(Travel_time))
        }
        else if (input$polar==2)
        { val="fare:"
          data1<-Visualisationdata_final %>% group_by(borough,year) %>% summarise(fare=mean(fare_amount))
        }
       else if (input$polar == 4 )
       {
         val="Distance :"
         data1<-Visualisationdata_final %>% group_by(borough,year) %>% summarise(fare=mean(trip_distance))
       }
        else
        { val="Trip Count :"
            data1<-Visualisationdata_final %>% group_by(borough,year) %>% summarise(fare=n()/90)
        }
       
        data1<-data.frame("borough"=data1$borough,"year"=data1$year,"fare"=data1$fare)
        # Set a number of 'empty bar' to add at the end of each borough
        empty_bar=3
        to_add = data.frame( matrix(NA, empty_bar*nlevels(Visualisationdata_final$borough), ncol(data1)) )
        colnames(to_add) = colnames(data1)
        to_add$borough=rep(levels(Visualisationdata_final$borough), each=empty_bar)
        data=rbind(data1, to_add)
        data=data %>% arrange(borough)
        data$id=seq(1, nrow(data))
        
        # Get the name and the y position of each label
        label_data=data
        number_of_bar=nrow(label_data)
        angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
        label_data$hjust<-ifelse( angle < -90, 1, 0)
        label_data$angle<-ifelse(angle < -90, angle+180, angle)
        
        # prepare a data frame for base lines
        base_data=data %>% 
          group_by(borough) %>% 
          summarize(start=min(id), end=max(id) - empty_bar) %>% 
          rowwise() %>% 
          mutate(title=mean(c(start, end)))
        
        # prepare a data frame for grid (scales)
        grid_data = base_data
        grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
        grid_data$start = grid_data$start - 1
        grid_data=grid_data[-1,]
        
        # Make the plot
        p = ggplot(data, aes(x=as.factor(id), y=fare, fill=borough)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
          
          geom_bar(aes(x=as.factor(id), y=fare, fill=borough), stat="identity", alpha=0.5) +
          
          # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
          geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
          geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
          geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
          geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
          
          # Add text showing the value of each 100/75/50/25 lines
          annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
          
          geom_bar(aes(x=as.factor(id), y=fare, fill=borough), stat="identity", alpha=0.5) +
          ylim(-100,120) +
          theme_minimal() +
          theme(
            legend.position = "none",
            axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank(),
            plot.margin = unit(rep(-1,4), "cm") 
          ) +
          coord_polar() + 
          geom_text(data=label_data, aes(x=id, y=fare, label=paste0(year  ,"\n",val,round(fare,0)), hjust=hjust), color="black", fontface="bold",alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) +
          
          # Add base line information
          geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
          geom_text(data=base_data, aes(x = title, y = -18, label=borough), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
          p
        
        
      })
 })
###############################





