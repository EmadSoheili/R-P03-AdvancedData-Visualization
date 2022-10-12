
# R-P03-AdvancedDataVisualization

# Step 1: Work Directory -----------------------------------------------------------------------

setwd("E:/Mine/Maktab Khooneh/Data Analysis/3 - Advanced Data Visualization/Extras/Final Project")
getwd()

# Step 2: Importing Data -----------------------------------------------------------------------

data1 = read.csv("data_example_base.csv", header = T)
head (data1)
tail (data1)
colnames(data1) = c("id","lat","lon","time")
summary (data1)
      # No NA

tr_rst_zone = read.csv("restrictedzone.csv", header = T)
head (tr_rst_zone)

pl_rst_zone = read.csv("restrictedzone2.csv", header = T)
head (tr_pol_zone)

shdate = read.csv("calendar.csv", header = T)
head (shdate)

# Step 3: Function - polyFinder ----------------------------------------------------------------

polyFinder <- function(pointLat, pointLong, zone_data){
  polyLong <- zone_data$lon    #It's equivalent to x axis in cartesian coordinate
  polyLat <- zone_data$lat    #It's equivalent to y axis in cartesian coordinate
  i = 1
  j = dim(zone_data)[1]    #Number of polygon corners
  nodes <- 0
  
  #***Note: start point and end point should be put together successively
  #**Note: The limit is set on points with higher latitude
  #**Note: if we can sort these coordinates in clockwise we can remove the former condition in first if statement
  
  while(i <= dim(zone_data)[1]){
    if ((polyLat[i] < pointLat & pointLat <= polyLat[j]) 
        | (polyLat[j] < pointLat & pointLat <= polyLat[i])
        & (polyLong[i] <= pointLong | polyLong[j] <= pointLong)){
      if ( (((pointLat - polyLat[i])/(polyLat[j] - polyLat[i]))*(polyLong[j]-polyLong[i])) + polyLong[i] < pointLong) nodes <- nodes + 1
    }
    j = i
    i = i+1
  }
  if (nodes%%2 != 0) {u <- 1}
  else {u <- 0}
  u
}

# Step 4: Data Preparation ---------------------------------------------------------------------

data2 = data1
      # I want to process data2 and data1 remains as the source data

head (data2)
tail (data2)
data2$date = as.Date(data2$time, "%m/%d/%Y")
class(data2$date)
data2$wday = format(data2$date,"%w")

head(shdate)
shdate$date = as.Date(shdate$Ch_Date, "%m/%d/%Y")
class(shdate$date)

data2$wday[data2$wday == 0] = "Sun"
data2$wday[data2$wday == 1] = "Mon"
data2$wday[data2$wday == 2] = "Tue"
data2$wday[data2$wday == 3] = "Wed"
data2$wday[data2$wday == 4] = "Thu"
data2$wday[data2$wday == 5] = "Fri"
data2$wday[data2$wday == 6] = "Sat"

data2$hour = format(as.POSIXct(data2$time,tryFormat="%m/%d/%Y %H:%M"),"%H")


# Step 5: Zone Separation ------------------------------------------------------------------------

head(data2,200)

for (i in 1:nrow(data2)) {
  data2$tr[i] = polyFinder(data2$lat[i], data2$lon[i],tr_rst_zone)
  data2$pl[i] = polyFinder(data2$lat[i], data2$lon[i],pl_rst_zone)
}

sum(data2$tr == 1)
sum(data2$pl == 1)

data2$zone = NA

for (i in 1:nrow(data2)) {
  if (data2$tr[i] == 1){
    data2$zone[i] = 1
    } else {
      data2$zone[i] = ifelse(data2$pl[i] == 1,2,3)
        }
}

# Step 6: Add Shamsi Date ---------------------------------------------------------------------

head(data2)
head(shdate)

data2$shdate = shdate[match(data2$date,shdate$date),'Sh_Date']
data2$wnum = shdate[match(data2$date,shdate$date),'Week_Num']


# Step 7: Frequency Calculation ---------------------------------------------------------------

head(data2)

    # Zone = 1 : Inside the traffic zone
    # Zone = 2 : Inside the air pollution zone and outside the traffic zone
    # Zone = 3 : Outside the air pollution zone
head(data2)
hfreqw15z1 = as.data.frame(table(data2$hour[data2$wnum == "W15" & data2$zone == 1]))
hfreqw15z1$wnum = "W15"
hfreqw15z1$zone = 1

hfreqw15z2 = as.data.frame(table(data2$hour[data2$wnum == "W15" & data2$zone == 2]))
hfreqw15z2$wnum = "W15"
hfreqw15z2$zone = 2

hfreqw15z3 = as.data.frame(table(data2$hour[data2$wnum == "W15" & data2$zone == 3]))
hfreqw15z3$wnum = "W15"
hfreqw15z3$zone = 3

hfreqw16z1 = as.data.frame(table(data2$hour[data2$wnum == "W16" & data2$zone == 1]))
hfreqw16z1$wnum = "W16"
hfreqw16z1$zone = 1

hfreqw16z2 = as.data.frame(table(data2$hour[data2$wnum == "W16" & data2$zone == 2]))
hfreqw16z2$wnum = "W16"
hfreqw16z2$zone = 2

hfreqw16z3 = as.data.frame(table(data2$hour[data2$wnum == "W16" & data2$zone == 3]))
hfreqw16z3$wnum = "W16"
hfreqw16z3$zone = 3


hfreqw17z1 = as.data.frame(table(data2$hour[data2$wnum == "W17" & data2$zone == 1]))
hfreqw17z1$wnum = "w17"
hfreqw17z1$zone = 1

hfreqw17z2 = as.data.frame(table(data2$hour[data2$wnum == "W17" & data2$zone == 2]))
hfreqw17z2$wnum = "W17"
hfreqw17z2$zone = 2

hfreqw17z3 = as.data.frame(table(data2$hour[data2$wnum == "W17" & data2$zone == 3]))
hfreqw17z3$wnum = "W17"
hfreqw17z3$zone = 3

      # Percentage calculation:

hfreqw15z1$prc = round(hfreqw15z1$Freq/sum(hfreqw15z1$Freq)*100,1)
hfreqw15z2$prc = round(hfreqw15z2$Freq/sum(hfreqw15z2$Freq)*100,1)
hfreqw15z3$prc = round(hfreqw15z3$Freq/sum(hfreqw15z3$Freq)*100,1)

hfreqw16z1$prc = round(hfreqw16z1$Freq/sum(hfreqw16z1$Freq)*100,1)
hfreqw16z2$prc = round(hfreqw16z2$Freq/sum(hfreqw16z2$Freq)*100,1)
hfreqw16z3$prc = round(hfreqw16z3$Freq/sum(hfreqw16z3$Freq)*100,1)

hfreqw17z1$prc = round(hfreqw17z1$Freq/sum(hfreqw17z1$Freq)*100,1)
hfreqw17z2$prc = round(hfreqw17z2$Freq/sum(hfreqw17z2$Freq)*100,1)
hfreqw17z3$prc = round(hfreqw17z3$Freq/sum(hfreqw17z3$Freq)*100,1)

hfreq = rbind(hfreqw15z1,hfreqw15z2,hfreqw15z3,hfreqw16z1,hfreqw16z2,hfreqw16z3,hfreqw17z1,hfreqw17z2,hfreqw17z3)
colnames (hfreq) = c("hour","rides","wnum","zone", "prc")
head (hfreq)

# Step 8: Question A: Plotly - Hourly Distribution by Week and Zone -----------------------------

library("ggplot2")
library("plotly")

head(hfreq)

      # Rides - Hour Frequency Chart

ch1 = ggplot(data = hfreq, aes(x = hour, y= rides, color = interaction(wnum, zone))) +
  geom_line(aes(group = interaction(wnum, zone))) +
  ggtitle("Chart 1 \n Rides - Hour Frequency Chart") +
  theme(
    plot.title = element_text(color = "dark blue", size = 14, face = "bold"),
    panel.background = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank()
  )
pl1 = ggplotly(ch1)

      # Rides - Hour Percentage Chart

ch2 = ggplot(data = hfreq, aes(x = hour, y= prc, color = interaction(wnum, zone))) +
  geom_line(aes(group = interaction(wnum, zone))) +
  ggtitle("Chart 2 \n Rides - Hour Percentage Chart") +
theme(
  plot.title = element_text(color = "dark blue", size = 14, face = "bold"),
  panel.background = element_blank(),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank()
)

pl2 = ggplotly(ch2)

      # The campaign (which launched during the 15th week) cause a considerable increase in the number of rides in the morning and noon, Especially outside the traffic zone.
      # Since most inside zone 1 is necessary, campaign had less impact on the number of rides in this zone.
      # The first chart has illustrated that individuals are more inclined to take rides in the zone 3 in the morning.This impact is huge since most of zone 3 rides are unnecessary and people are not inclined to take a cab for it. On the other hand because of the incentive, these ride seems more cost efficient and people became more inclined towards taking rides.
      # Chart 2 shows that the distribution (especially in the zone 2 and 3) is more constant during daytime in the campaign week. (the possible reason has been explained in a couple of lines above)

      
      # My suggestions:
          # I think with launching a campaign, ride number inside zone 1 and 2 will experience a lesser increase compared to zone 3. So I think drivers should be distributed all across the city.
          # Also, I reckon that they should consider an incentive for drivers to accept rides, not only to fulfill the request, but also to distributed evenly between morning, noon and evening during campaign days.
          
      # Requirements for further investigations:
          # Destination's location
          # Acceptance rate
          # Special data like technical issues, internet issues, etc.


# Step 9: Shiny -------------------------------------------------------------------------------

library("shiny")
library("leaflet")

ui = fluidPage(titlePanel("Final Project - Emad Soheili"),
    sidebarLayout(
    sidebarPanel(h2("Input"),
                 sliderInput("znumber","Zone Number",min = 1, max = 3,step = 1, value = 1, animate = T),
                 radioButtons("wnumber","Week Number",choices = c("W15","W16","W17")),
                 selectInput("day", "Day", choices = c("Sat","Sun","Mon","Tue","Wed","Thu","Fri"))),
    mainPanel(h2("Output"),
              plotOutput("plot"),
              br(),br(),
              leafletOutput("map"),
              br(),br(),
              tableOutput("table")
              )
  ))


server = function(input, output) {
  output$plot <- renderPlot({
    
    hfreq2 = hfreq[hfreq$zone == input$znumber &
                     hfreq$wnum == input$wnumber,]
    
    ggplot(data = hfreq2, aes(x = hour, y= prc)) +
      geom_line(aes(group = interaction(wnum, zone))) +
      ggtitle("Chart 2 \n Rides - Hour Percentage Chart") +
      theme(
        plot.title = element_text(color = "dark blue", size = 14, face = "bold"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()
      )})

  
  output$map = renderLeaflet({

    fdata = data2[data2$zone == input$znumber & data2$wday == input$day & data2$wnum == input$wnumber,]
    
    
    if(input$znumber == 1){
      map = leaflet () %>%
      addTiles() %>%
      setView(lat = 35.6892, lng = 51.3890, zoom = 11) %>%
      addPolygons(lat = tr_rst_zone$lat, lng = tr_rst_zone$lon, color = "red") %>%
      addPolygons(lat = pl_rst_zone$lat, lng = pl_rst_zone$lon, color = "green") %>%
      addPopups(lat = 35.7015, lng = 51.4191, paste("Zone Number: ", input$znumber,br(), "Number of Rides: ", nrow(fdata)),
                options = popupOptions(closeButton = F, closeOnClick = FALSE))} else {
                  
                  if(input$znumber == 2){
                    map = leaflet () %>%
                      addTiles() %>%
                      setView(lat = 35.6892, lng = 51.3890, zoom = 11) %>%
                      addPolygons(lat = tr_rst_zone$lat, lng = tr_rst_zone$lon, color = "red") %>%
                      addPolygons(lat = pl_rst_zone$lat, lng = pl_rst_zone$lon, color = "green") %>%
                      addPopups(lat = 35.7409, lng = 51.4606, paste("Zone Number: ", input$znumber,br(), "Number of Rides: ", nrow(fdata)),
                                options = popupOptions(closeButton = F, closeOnClick = FALSE))
                    
                  } else {
                    map = leaflet () %>%
                      addTiles() %>%
                      setView(lat = 35.6892, lng = 51.3890, zoom = 11) %>%
                      addPolygons(lat = tr_rst_zone$lat, lng = tr_rst_zone$lon, color = "red") %>%
                      addPolygons(lat = pl_rst_zone$lat, lng = pl_rst_zone$lon, color = "green") %>%
                      addPopups(lat = 35.7590, lng = 51.4276, paste("Zone Number: ", input$znumber,br(), "Number of Rides: ", nrow(fdata)),
                                options = popupOptions(closeButton = F, closeOnClick = FALSE))
                    
                  }}})        
                       
    output$table = renderTable({
      
      
      fdata2 = hfreq[hfreq$zone == input$znumber & hfreq$wnum == input$wnumber,c("hour","rides","prc")]
      colnames(fdata2) = c("Hour","Rides","Percentage")
      fdata2})
  
  }

shinyApp (ui = ui, server = server)
