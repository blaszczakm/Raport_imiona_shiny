#Autor : Marek Błaszczak

#install.packages("shiny")
#install.packages("shinydashboard")
#install.packages("rvest")
#install.packages("tidyverse")
#install.packages(DT)
#install.packages("dplyr")

library(dplyr)
library(tidyverse) #do utworzenia 2 kolumn z jednej
library(rvest) #do scrapowania html
library(shiny)
library(shinydashboard)
library(DT)


header<-dashboardHeader(title = "Imiona 2018")

sidebar<-dashboardSidebar(sidebarMenu(
  
  menuItem("Imiona dla dziewczynek",tabName = "dziewczynki"),
  
  menuItem("Imiona dla chłopców", tabName = "chlopcy"),
  
  menuItem("Pełne dane",
           menuSubItem("Dziewczynki", tabName = "pelne_d"),
           menuSubItem("Chłopcy", tabName = "pelne_ch")),
  
  menuItem("Autor raportu",
           menuSubItem("Marek Błaszczak"))
  
))

body<-dashboardBody(tabItems(
  
  tabItem("dziewczynki",
          uiOutput("ui_1"),
          infoBoxOutput("ilosc_d"),
          infoBoxOutput("imie_d_max"),
          infoBoxOutput("imie_d_u"),
          box(plotOutput("obszar_d_l")),
          box(plotOutput("obszar_d_p")),
          uiOutput("ui_3"),
          uiOutput("spr_d")),
  
  tabItem("chlopcy",
          uiOutput("ui_2"),
          infoBoxOutput("ilosc_ch"),
          infoBoxOutput("imie_ch_max"),
          infoBoxOutput("imie_ch_u"),
          box(plotOutput("obszar_ch_l")),
          box(plotOutput("obszar_ch_p")),
          uiOutput("ui_4"),
          uiOutput("spr_ch")),
  
  tabItem("pelne_d",
          textOutput("zrodlo1"),
          br(),
          DTOutput("tab_d")),
  
  tabItem("pelne_ch",
          textOutput("zrodlo2"),
          br(),
          DTOutput("tab_ch"))
  
))

ui <- dashboardPage(header, sidebar, body)

server <- function(session, input, output){
  

  output$zrodlo1 <- renderText("Źródło danych: https://www.gov.pl/web/cyfryzacja/imiona")
  output$zrodlo2 <- renderText("Źródło danych: https://www.gov.pl/web/cyfryzacja/imiona")
  
  #imiona w poszczegolnych wojewodztwach
  
  woj <- c("ranking-ogolnopolski","woj-dolnoslaskie", "woj-kujawsko-pomorskie", "woj-lubelskie", 
           "woj-lubuskie","woj-lodzkie","woj-malopolskie","woj-mazowieckie","woj-opolskie",
           "woj-podkarpackie","woj-podlaskie","woj-pomorskie","woj-slaskie","woj-swietokrzyskie",
           "woj-warminsko-mazurskie","woj-wielkopolskie","woj-zachodniopomorskie")
  
  plec <- c("dziewczynek","chlopcow")
  
  
  ### imioma dziewczynek - wojewodztwa ###
  
  woj_dz<-seq(length(woj))
  woj_ch<-seq(length(woj))

  
  for (w in seq(length(woj))) {
    
    page<- paste0("https://www.gov.pl/web/cyfryzacja/najpopularniejsze-imiona-dla-",
                  plec[1],"-2018-",woj[w])
    
    dane <- read_html(page)
    imiona <- dane %>% html_nodes("div.paragraph.article_section_content")%>%
      html_nodes("p") %>% 
      gsub("<p>","",.)%>% #pozbycie sie znacznika otw. <p>
      gsub("</p>","",.)%>% #pozbycie sie znacznika zamykajacego  </p>
      gsub("<br>",'\n',.) %>% #zamiana <br> na enter
      gsub(" ", "",.) %>%
      strsplit(split = '\n') #rozdzielenie ciagu na podciagi za pomoca entera
    woj_dz[w] <- as.data.frame(imiona) #utworzenie ramki danych
    #length(woj_dz) #16
    #length(woj_dz[w]) #wyszlo 1 
    #length(woj_dz[[w]])
    woj_dz[[w]]<-as.data.frame(woj_dz[[w]])
    ncol(woj_dz[[w]])
    colnames(woj_dz[[w]])<-"x" # zamiana nazwy kolumna na x
    
    #rodzielnie kolumny x na kolumne "imie" "ilosc" za pomoca -
    woj_dz[[w]] <- woj_dz[[w]] %>% separate("x",c("Imie","Ilosc"),sep = "-") 
    #colnames(woj_dz[w])
    woj_dz[[w]]$Imie<-as.character(woj_dz[[w]]$Imie)
    woj_dz[[w]]$Ilosc<-as.numeric(woj_dz[[w]]$Ilosc)
  
    # a<-sum(woj_dz[[w]]$Ilosc)
    # woj_dz[[w]]<-woj_dz[[w]] %>% 
    #   mutate("Procent" = round((Ilosc/a)*100,2))  
    
    ### dla chlopcow ###
    page<- paste0("https://www.gov.pl/web/cyfryzacja/najpopularniejsze-imiona-dla-",
                  plec[2],"-2018-",woj[w])
    
    dane <- read_html(page)
    imiona <- dane %>% html_nodes("div.paragraph.article_section_content")%>%
      html_nodes("p") %>% 
      gsub("<p>","",.)%>% #pozbycie sie znacznika otw. <p>
      gsub("</p>","",.)%>% #pozbycie sie znacznika zamykajacego  </p>
      gsub("<br>",'\n',.) %>% #zamiana <br> na enter
      gsub(" ", "",.) %>%
      strsplit(split = '\n') #rozdzielenie ciagu na podciagi za pomoca entera
    woj_ch[w] <- as.data.frame(imiona) #utworzenie ramki danych
    #length(woj_ch) #16
    #length(woj_ch[w]) #wyszlo 1 
    #length(woj_ch[[w]])
    woj_ch[[w]]<-as.data.frame(woj_ch[[w]])
    ncol(woj_ch[[w]])
    colnames(woj_ch[[w]])<-"x" # zamiana nazwy kolumna na x
    
    #rodzielnie kolumny x na kolumne "imie" "ilosc" za pomoca -
    woj_ch[[w]] <- woj_ch[[w]] %>% separate("x",c("Imie","Ilosc"),sep = "-") 
    #colnames(woj_ch[w])
    woj_ch[[w]]$Imie<-as.character(woj_ch[[w]]$Imie)
    woj_ch[[w]]$Ilosc<-as.numeric(woj_ch[[w]]$Ilosc)
    
  }
  
  # jedna duza data frame dla dziewczat
  m_d <- matrix(NA, nrow = nrow(woj_dz[[1]]), ncol = length(woj))
  m_d <- as.data.frame(m_d)
  #m_d[,1] <- woj_dz[[1]]$Imie
  colnames(m_d)<-woj
  rownames(m_d)<-woj_dz[[1]]$Imie
  
  #jedna data frame dla chlopcow
  m_ch <- matrix(NA, nrow = nrow(woj_ch[[1]]), ncol = length(woj))
  m_ch <- as.data.frame(m_ch)
  #m_ch[,1] <- woj_ch[[1]]$Imie
  colnames(m_ch)<-woj
  rownames(m_ch)<-woj_ch[[1]]$Imie
  
  for(i in seq(length(woj))){
    
    for(j in seq(nrow(m_d))){
      
      imie_wiersz <- rownames(m_d[j,])
      ktory_wiersz <- which(woj_dz[[i]]$Imie==imie_wiersz)
      ilosc_imie <- woj_dz[[i]]$Ilosc[ktory_wiersz]
      
      if(length(ilosc_imie)==0){
        m_d[j,i]<-NA
      }
      
      else{
        m_d[j,i]<-ilosc_imie
      }
      
    }
  }
  
  
  #dodanie kolumny z imionami
  
  m_d <- m_d %>%
    mutate(.,Imie=rownames(m_d)) %>%
    select(., c(Imie,woj))
  
  output$tab_d <- renderDT(m_d, options = list(scrollX=T,scrollY=T))
  
  ### wypelnianie tabeli dla chlopcow ###
  
  for(i in seq(length(woj))){
    
    for(j in seq(nrow(m_ch))){
      
      imie_wiersz <- rownames(m_ch[j,])
      ktory_wiersz <- which(woj_ch[[i]]$Imie==imie_wiersz)
      ilosc_imie <- woj_ch[[i]]$Ilosc[ktory_wiersz]
      
      if(length(ilosc_imie)==0){
        m_ch[j,i]<-NA
      }
      
      else{
        m_ch[j,i]<-ilosc_imie
      }
      
    }
  }
  
  
  
  # dodanie kolumny z imionami
  m_ch <- m_ch %>%
    mutate(.,Imie=rownames(m_ch)) %>%
    select(.,c(Imie,woj))
  
  output$tab_ch <- renderDT(m_ch, options = list(scrollX=T,scrollY=T))
  
  
  output$ui_1<-renderUI(
    selectInput("woj_input_d","Wybierz obszar",
                choices = woj,
                selected = woj[1])
  )
  
  output$ui_2<-renderUI(
    selectInput("woj_input_ch","Wybierz obszar",
                choices = woj,
                selected = woj[1])
  )
  
  output$ui_3<-renderUI(
    textInput("imie_input_d","",
              placeholder = "Wpisz imie dla dziewczynki")
  )
  
  output$ui_4<-renderUI(
    textInput("imie_input_ch","",
              placeholder = "Wpisz imie dla chłopca")
  )
  
  
  observe({
    
  library(ggplot2)
    
    if(!is.null(input$woj_input_d)){
      
      woj_wybor_d <- as.character(input$woj_input_d)
  
      v_d <- m_d %>%
        select(.,Imie,woj_wybor_d) %>%
        na.omit()
      
        v_d<-arrange(v_d,desc(v_d[,2]))
      
      v10_d <- as.data.frame(head(v_d,10))
      suma_d <- sum(v_d[,2])
      k_d <- which.max(v_d[,2])
      top_imie_d <- v_d$Imie[k_d]
      unikalne_d <- length(v_d$Imie)
      
      wykres_d_l<-ggplot(v10_d)+
        geom_col(aes(x=Imie, y=v10_d[,2], fill=Imie))+
        labs(title = paste("Top 10 imion dla dziewczynek: ",woj_wybor_d)
             , y="Liczba")+
        theme(axis.text.x = element_blank())
      
      a<- sum(v_d[,2])
      
      wykres_d_p<-ggplot(v10_d)+ 
        coord_flip() +
        geom_col(aes(x=Imie, 
                     y=round((v10_d[,2]/a)*100,2), fill=Imie))+
        labs(title = paste("Top 10 imion dla dziewczynek: ", woj_wybor_d), y="%")+
        theme(legend.position = "none")
      
      ## wykresy dla danych obszarow ###
      
      output$obszar_d_l<-renderPlot(wykres_d_l)
      output$obszar_d_p <- renderPlot(wykres_d_p)
      
     ### info boxy dla danych obszarow ###
     output$ilosc_d <- renderInfoBox(
       infoBox("Urodzonych dziewczynek w 2018", suma_d, color = "olive"))
     
     output$imie_d_max <- renderInfoBox(
       infoBox("Najpopularniejsze imie dziewczęce",top_imie_d,color = "olive"))
     
     output$imie_d_u <- renderInfoBox(
       infoBox("Ilosc unikalnych imion",unikalne_d,color = "olive"))
     
    } # zamyka if
    
    else
      {return(NULL)}
    
    if(!is.null(input$woj_input_ch)){
      
      woj_wybor_ch <- as.character(input$woj_input_ch)
      
      v_ch <- m_ch %>%
        select(.,Imie, woj_wybor_ch) %>%
        na.omit()
      
      v_ch<-arrange(v_ch,desc(v_ch[,2]))
      
      v10_ch <- as.data.frame(head(v_ch,10))
      suma_ch <- sum(v_ch[,2])
      k_ch <- which.max(v_ch[,2])
      top_imie_ch <- v_ch$Imie[k_ch]
      unikalne_ch <- length(v_ch$Imie)
      
      wykres_ch_l<-ggplot(v10_ch)+
        geom_col(aes(x=Imie, y=v10_ch[,2], fill=Imie))+
        labs(title = paste("Top 10 imion dla chlopcow: ",woj_wybor_ch), 
             y="Liczba")+
        theme(axis.text.x = element_blank())
      
      b <- sum(v_ch[,2])
      
      wykres_ch_p<-ggplot(v10_ch)+ 
        coord_flip() +
        geom_col(aes(x=Imie, 
                     y=round((v10_ch[,2]/b)*100,2), fill=Imie))+
        labs(title = paste("Top 10 imion dla chlopcow: ", woj_wybor_ch), y="%")+
        theme(legend.position = "none")
      
      ## wykresy dla danych obszarow ###
      
      output$obszar_ch_l<-renderPlot(wykres_ch_l)
      output$obszar_ch_p <- renderPlot(wykres_ch_p)
      
      ### info boxy dla danych obszarow ###
      output$ilosc_ch <- renderInfoBox(
        infoBox("Urodzonych chlopcow w 2018", suma_ch, color = "blue"))
      
      output$imie_ch_max <- renderInfoBox(
        infoBox("Najpopularniejsze imie chlopiece",top_imie_ch,color = "blue"))
      
      output$imie_ch_u <- renderInfoBox(
        infoBox("Ilosc unikalnych imion",unikalne_ch,color = "blue"))
      
    } #zamyka if
    
    
    if(length(input$imie_input_d)>0 && input$imie_input_d!=""){
      
      imie_wybor_d <- toupper(as.character(input$imie_input_d))
      i_d <- m_d %>%
        filter(., Imie == imie_wybor_d)
      
      i_d$Imie <- NULL
      i_d <- as.data.frame(t(i_d))
      
      i_d <- i_d %>%
        mutate(.,Woj=woj)
      
      ktore_d <- which(is.na(i_d[,1]))
      i_d[ktore_d, 1] <- 0
      
      if(!is.null(i_d$V1)){
        
       x <-ggplot(i_d)+
         geom_col(aes(x=Woj, y=V1, fill="red"))+
         labs(title = paste("Statystyki dla imienia ", imie_wybor_d),
              y="Ilość", x= "")+
         theme(legend.position = "none")+
         coord_flip()
  
       output$spr_d <- renderUI(box(plotOutput("spr_p_d")))
       output$spr_p_d <- renderPlot(x)
       
      }
      
      else{
        output$spr_d <- NULL
      }
      
    } #zamyka if dla wykresu sprawdzajacego dane imie
    
    # ### sprawdz imie dla chlopca - wykres
     
      if(length(input$imie_input_ch)>0 && input$imie_input_ch!=""){
      
        imie_wybor_ch <- toupper(as.character(input$imie_input_ch))
        i_ch <- m_ch %>%
          filter(., Imie == imie_wybor_ch)
      
        i_ch$Imie <- NULL
        i_ch <- as.data.frame(t(i_ch))
      
        i_ch <- i_ch %>%
          mutate(.,Woj=woj)
      
        ktore_ch <- which(is.na(i_ch[,1]))
        i_ch[ktore_ch, 1] <- 0
      
        if(!is.null(i_ch$V1)){
      
          y <-ggplot(i_ch)+
            geom_col(aes(x=Woj, y=V1, fill="aqua"))+
            labs(title = paste("Statystyki dla imienia ", imie_wybor_ch),
                 y="Ilość", x= "")+
            theme(legend.position = "none")+
            coord_flip()
      
          output$spr_ch <- renderUI(box(plotOutput("spr_p_ch")))
          output$spr_p_ch <- renderPlot(y)
      
        }
      
        else{
          output$spr_ch <- NULL
        }
      
      } #zamyka if dla wykresu sprawdzajacego dane imie
    

    
  }) #zamyka observe
  
}

shinyApp(ui=ui, server = server)