library(tidyverse)
library(shiny)
library(openxlsx)
library(dplyr)
library(data.table)
#--------------Girl Baby Name File Transformation-------------------------
data= read.xlsx("Top100_Popular_Baby_Names.xlsx", sheet=1)

#Create Vector Year
year <- data[2,3]

for (i in 5:193)
{
  if(!is.na(data[2,i])){
    year <- append(year, data[2,i])
  }
  
  
}
#Convert Year to Numeric
year <- as.numeric(year)


#Create new empty Df 
girl_name.df <- data.frame("Year"=integer(),"Rank"=integer(),"Name"=character(),"No"=integer(), stringsAsFactors = FALSE)

#To Paste Year in new DataFrame

j=0
for(k in 1:length(year))
{
  
  for (i in 1:100) 
  {
    girl_name.df[(j+i),1] <- year[k]
    
  }
  j=j+100
}

#To Paste Ranks in new DataFrame

j=0
for(k in 1:length(year))
{
  
  for (i in 1:100) 
  {
    girl_name.df[(j+i),2] <- i
    
  }
  j=j+100
}


#To Paste Name in new Dataframe
i=0
j=0
k=0
repeat
{
  if (k == 93) {
    k<-k+2
  } else {
    k<-k+3
  }
  for (i in 1:100) 
  {
    girl_name.df[(j+i),3] <- data[(i+3),k]
    
  }
  j=j+100
  
  if (k >= 194){
    break
  }
  
}

#To paste No in new Dataframe
i=0
j=0
k=1
repeat
{
  if (k == 94) {
    k<-k+2
  } else {
    k<-k+3
  }
  for (i in 1:100) 
  {
    girl_name.df[(j+i),4] <- data[(i+3),k]
    
  }
  j=j+100
  
  if (k >= 195){
    break
  }
  
}

#--------------------Boy Baby Names File Transformation----------------------

data= read.xlsx("Top100_Popular_Baby_Names.xlsx", sheet=2)

#Create new DF
boy_name.df <- data.frame("Year"=integer(),"Rank"=integer(),"Name"=character(),"No"=integer(), stringsAsFactors = FALSE)

#To Paste Year in new DataFrame

j=0
for(k in 1:length(year))
{
  
  for (i in 1:100) 
  {
    boy_name.df[(j+i),1] <- year[k]
    
  }
  j=j+100
}

#To Paste Ranks in new DataFrame

j=0
for(k in 1:length(year))
{
  
  for (i in 1:100) 
  {
    boy_name.df[(j+i),2] <- i
    
  }
  j=j+100
}

#To Paste Name in new Dataframe
i=0
j=0
k=0
repeat
{
  
  k<-k+3
  
  for (i in 1:100) 
  {
    boy_name.df[(j+i),3] <- data[(i+3),k]
    
  }
  j=j+100
  
  if (k >= 195){
    break
  }
  
}

#To paste No in new Dataframe
i=0
j=0
k=1
repeat
{
  
  k<-k+3
  for (i in 1:100) 
  {
    boy_name.df[(j+i),4] <- data[(i+3),k]
    
  }
  j=j+100
  
  if (k >= 196){
    break
  }
  
}
#Remove unncessary Variables
remove(year)
remove(i)
remove(j)
remove(k)
remove(data)


girl_name.df$Year <- as.factor(girl_name.df$Year)

boy_name.df$Year <- as.factor(boy_name.df$Year)

#Create new DF for top 10 girl names 
girl_top10<-setorder(setDT(girl_name.df), -Rank)[, tail(.SD,10), keyby = Year]

#Sort the girl_top10 descending
girl_top10 <- transform.data.frame(girl_top10, Year = rev(Year), Rank = rev(Rank), Name = rev(Name), No = rev(No))

#Create new DF for top 10 boy names 
boy_top10<-setorder(setDT(boy_name.df), -Rank)[, tail(.SD,10), keyby = Year]

#Sort the boy_top10 descending
boy_top10 <- transform.data.frame(boy_top10, Year = rev(Year), Rank = rev(Rank), Name = rev(Name), No = rev(No))





#----------------------Shiny App----------------
ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(
      #Select Year input from drop down
      selectInput(inputId = "Year",
                  label = "Select a Year from dropdown for top 10 names",
                  choices = girl_top10$Year,
                  selected =girl_top10[1,1]
      ),
      
      selectInput(inputId = "GirlName",
                  label = "Select a girl name to view Popularity overtime",
                  choices = sort(girl_name.df$Name),
                  selected = girl_name.df[1,3]),
      selectInput(inputId = "BoyName",
                  label = "Select a Boy name to view Popularity overtime",
                  choices = boy_name.df$Name,
                  selected = boy_name.df[1,3])
    ),
    
    #output
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("Top 10 Girl Names",tableOutput("Table_girl")),
                  tabPanel("Top 10 Boy Names", tableOutput("Table_boy")),
                  tabPanel("Trend of Girl Names over time", plotOutput("GirlChart")),
                  tabPanel("Trend of Boy Names over time", plotOutput("BoyChart")))
    )))
server <- function(input, output) {
  
  output$Table_girl <- renderTable({
    yearfilter1 <- subset(girl_top10, girl_top10$Year == input$Year)
  })
  
  output$Table_boy <- renderTable({
    yearfilter2 <- subset(boy_top10, boy_top10$Year == input$Year)
  })
  
  output$GirlChart <- renderPlot({
    name <- subset(girl_name.df, girl_name.df$Name == input$GirlName)
    ggplot(data=name, aes(x=Year, y=No, group=1))+
      geom_line()+
      geom_point()
  })
  
  output$BoyChart <- renderPlot({
    name1 <- subset(boy_name.df, boy_name.df$Name == input$BoyName)
    ggplot(data=name1, aes(x=Year, y=No, group=1))+
      geom_line()+
      geom_point()
  })
  
  
}
shinyApp(ui = ui, server = server)


