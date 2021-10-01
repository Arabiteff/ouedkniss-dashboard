#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(summaryBox)
library(rvest)
library(stringr)
library(tm)
library(tidyr)
library(dplyr)
library(shinyauthr)
ouedknissFuncion<-function(input){
  car_name_real<-c()
  car_marque<-c()
  car_year<-c()
  car_price<-c()
  car_state<-c()
  car_type<-c()
  car_energy<-c()
  car_boite<-c()
  car_couleur<-c()
  car_kilo<-c()
  car_moteur<-c()
  car_url_clean<-c()
  willaya_clean<-c()
  
  
  for(i in 1:input){
    url <- paste0('https://www.ouedkniss.com/annonces/index.php?c=automobiles&prix=1&p=',input)
    car_name <- read_html(url) %>% 
      html_nodes('li') %>%
      html_nodes('a') %>%
      html_nodes('h2') %>% 
      html_text()
    ## car name
    car_name_real<-c(car_name_real,word(car_name,2,-2)) 
    ## car_marque
    car_marque<-c(car_marque,word(car_name,1))
    ## car_year
    car_year<-c(car_year,word(car_name,-1))
    
    car_desc_data<-read_html(url) %>% 
      html_nodes('li') %>%
      html_nodes('.annonce_get_description') %>%
      html_text()%>%
      str_split(pattern = ":",simplify = T)
    ##car price
    car_price_state<-read_html(url)%>%
      html_nodes('.annonce_prix')%>%
      html_nodes('span')%>%
      html_text()
    car_price_state<-str_replace_all(car_price_state," Millions","")
    car_price<-c(car_price,str_split(car_price_state," ",simplify = T)[,1])
    ### car state
    car_state<-c(car_state,str_split(car_price_state," ",simplify = T)[,2])
    ## car type
    car_type<-c(car_type,removeNumbers(str_replace_all(car_desc_data[,1],pattern = "km Energie ","")))
    
    ## car energie
    car_energy<-c(car_energy,str_remove_all(car_desc_data[,2],"Moteur"))
    ## car boit
    car_boite<-c(car_boite,str_remove_all(car_desc_data[,4],"Couleur "))
    ##car couleur
    car_couleur<-c(car_couleur,word(str_trim(car_desc_data[,5],"left"),1))
    
    ## car_kilo
    car_kilo<-c(car_kilo,str_remove_all(extract_numeric(car_desc_data[,1]),"-"))
    
    ## car_moteur 
    car_moteur<-c(car_moteur,str_remove_all(car_desc_data[,3],"Boite"))
    ## car url
    car_url<-read_html(url)%>%
      html_nodes("a") %>%
      html_attr('href')
    car_url<-str_subset(car_url,"algerie")
    car_url<-as.data.frame(car_url)
    car_url<-distinct(car_url)
    car_url_clean<-c(car_url_clean,paste0("https://www.ouedkniss.com/",car_url$car_url))
    ##willaya
    willaya<-read_html(url)%>%
      html_nodes("div")%>%
      html_nodes("p")%>%
      html_nodes(".titre_wilaya")%>%
      html_text()
    willaya<- str_trim(str_remove_all(removeNumbers(willaya),"-"),"both")
    willaya<-as.data.frame(willaya)
    willaya<-willaya%>%filter(!grepl("Annonces",willaya))
    willaya<-willaya$willaya
    willaya_clean<-c(willaya_clean,willaya)
    
  }
  
  ouedkniss_car_data<-data.frame(
    Marque=car_marque,
    Name=car_name_real,
    Year=car_year,
    Type=car_type,
    Energie=car_energy,
    boite=car_boite,
    Colour=car_couleur,
    KM=as.numeric(car_kilo),
    Moteur=car_moteur,
    Prix=as.numeric(car_price),
    Status=car_state,
    Url=car_url_clean,
    Willaya<-willaya_clean
    
    
    
  )
  
  return(ouedkniss_car_data)
}


theme <- bslib::bs_theme(version = 4)


# Define UI for application that draws a histogram
ui <- fluidPage(
  title = "some stats",
  fluidRow(
  column(2,numericInput("page","number of pages that you want to scrape",
                        value = 1
                        ,min=1,
                        max=10))
  ),
  br(),
  actionButton("show","show pages selected"),
    theme = theme,
    br(),
  uiOutput("summaryBox_1"),
  fluidRow(
    tableOutput("name_mean")
  ),
  fluidRow(
    tableOutput("all")
  )
  
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
  
  
  ouedkniss_car_data<-eventReactive(input$show,{
    withProgress(message = "scraping the data,wait please",{
      incProgress(input$page*3)
      ouedknissFuncion(input$page)
    }
      
    )
  })
  ###brand metrics
  page_updated <- eventReactive(input$show,{
    req(input$page)
    ouedkniss_car_data()%>%
      group_by(Marque)%>%
      count(sort = T)
  })
    
  summary_updated_1<-eventReactive(input$show,summaryBox(page_updated()[1,1],
                                                         page_updated()[1,2],
                                                          width = 3,
                                                          icon = "fas fa-dollar-sign",
                                                          style = "success"))
  summary_updated_2<-eventReactive(input$show,summaryBox(page_updated()[2,1],
                                                         page_updated()[2,2], width = 3,
                                                         icon = "fas fa-dollar-sign",
                                                         style = "primary"))
  summary_updated_3<-eventReactive(input$show,summaryBox(page_updated()[3,1],
                                                         page_updated()[3,2],
                                                         width = 3,
                                                         icon = "fas fa-dollar-sign",
                                                         style = "warning"))
  summary_updated_4<-eventReactive(input$show,summaryBox(page_updated()[4,1],
                                                         page_updated()[4,2],
                                                         width = 3,
                                                         icon = "fas fa-dollar-sign",
                                                         style = "danger"))
  output$summaryBox_1<-renderUI({
      fluidRow(summary_updated_1(),
               summary_updated_2(),
               summary_updated_3(),
               summary_updated_4())
  })
  
  ### model metrics
  
  page_updated_name <- eventReactive(input$show,{
    req(input$page)
    prix_total<-ouedkniss_car_data()%>%
      group_by(Marque,Name)%>%
      count(sort=T,wt = Prix)%>%
      rename(total_price=n)
    top_name<-ouedkniss_car_data()%>%
      group_by(Marque,Name)%>%
      count(sort=T)
    average_name_price<-right_join(prix_total,top_name,by=c("Marque","Name"))%>%
      mutate(mean_price=total_price/n)%>%
      select(-total_price)%>%
      rename(total_number=n)%>%
      ungroup()%>%
      top_n(4,wt=total_number)%>%
      arrange(desc(total_number))
    average_name_price
    
  })
  
  output$name_mean<-renderTable({
    page_updated_name()
    
  })
  output$all<-renderTable({
    ouedkniss_car_data()
  })
  
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)
