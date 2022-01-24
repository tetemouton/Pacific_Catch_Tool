library(shiny)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(shinyWidgets)
library(shinyMatrix)
library(magrittr)
library(grid)
library(gridExtra)
library(rnaturalearth)
library(rnaturalearthdata)
library(plotly)
library(sf)
library(rgeos)


theme_set(theme_bw())

latest.yr <- 2019


  cnt.nms <- data.frame(sht.nm = c("all.cnt","HS","AS","AU","BZ","CA","CN","CK","EC","SV","EU","FM","FJ","GU","ID","JP","KI","KR","MH","NR","NC","NZ","NU",
                                   "MP","PW","PG","PH","PF","WS","SB","TW","TK","TO","TV","US","VN","VU","WF"),
                        lng.nm = c("All","High Seas","American Samoa","Australia","Belize","Canada","China","Cook Islands","Ecuador","El Salvador","European Union","FSM","Fiji","Guam","Indonesia","Japan",
                                   "Kiribati","Korea","Marshall Islands","Nauru","New Caledonia","New Zealand (Godzone)","Niue","Nthrn Mariana Is.",
                                   "Palau","PNG","Philippines","Polynesie Francais","Samoa","Solomon Is.","Taiwan","Tokelau","Tonga","Tuvalu","USA",
                                   "Vietnam","Vanuatu","Wallis/Futuna"))

  gr.cols <- data.frame(sht.nm = c("S","L","P","T","Z"),
                        grcols = c("royalblue2","darkseagreen2","peachpuff","tomato","lightgoldenrod2"))

  # Load data and assign EEZ's to higher level countries
  acedat <- read.table(file="./Data/ACE_BY_FLAG.TXT", header = TRUE, sep = ",") %>%
                       rename(Year = yy) %>%
                       mutate(tot_mt = alb_mt + bet_mt + skj_mt + yft_mt,
                              ez_ag = ifelse(eez %in% c("AU","NF"), "AU", eez),
                              ez_ag = ifelse(ez_ag %in% c("GL","KI","LN","PX"), "KI", ez_ag),
                              ez_ag = ifelse(ez_ag %in% c("NC","MA"), "NC", ez_ag),
                              ez_ag = ifelse(ez_ag %in% c("HB","HW","JT","JV","PY","WK"), "US", ez_ag),
                              ez_ag = ifelse(ez_ag %in% c("H4","H5","I0","I1","I2","I3","I4","I5","I6","I7","I8","I9","IW"), "HS", ez_ag),
                              flag = ifelse(flag %in% c("ES","PT"), "EU", flag))


  
  mapdat <- read.table(file="./Data/AGGREGATE_RAISED_5X5_S-L-P-T-Z.TXT", header = TRUE, sep = ",")
  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  mappl <- mapdat %>% mutate(tot_mt = alb_mt + bet_mt + skj_mt + yft_mt) %>% group_by(lond, latd) %>% summarise(Catch = sum(tot_mt))
  
  #cnt.keep <- read.csv(file="./EEZs/Data/PacCountries.csv", header=TRUE)
  
  # eez <- st_read("./EEZs/Data/World_EEZ_Files/World_EEZ_v10_2018_0_360.shp")
  # 
  # pac.eez <- eez[eez$Territory1 %in% cnt.keep$Ctry,]
  
#____________________________________________________________________________________________________________
# User interface

ui <- navbarPage(
  title = "Fishy fishy",
  tabPanel("Catch consequences",
           sidebarLayout(
             sidebarPanel(width = 3,
                          selectInput("country", "Select the country of interest:",
                                      c("All" = "all.cnt", "High Seas" = "HS",
                                        "American Samoa" = "AS", "Australia" = "AU", "Belize" = "BZ", "Canada" = "CA", "China" = "CN", "Cook Islands" = "CK", "Ecuador" = "EC",
                                        "El Salvador" = "SV", "European Union" = "EU", "FSM" = "FM", "Fiji" = "FJ", "Guam" = "GU", "Indonesia" = "ID",
                                        "Japan" = "JP", "Kiribati" = "KI", "Korea" = "KR", "Marshall Islands" = "MH", "Nauru" = "NR", "New Caledonia" = "NC",
                                        "New Zealand (Godzone)" = "NZ", "Niue" = "NU", "Nthrn Mariana Is." = "MP", "Palau" = "PW", "PNG" = "PG", "Philippines" = "PH",
                                        "Polynesie Francais" = "PF", "Samoa" = "WS", "Solomon Is." = "SB", "Taiwan" = "TW", "Tokelau" = "TK", "Tonga" = "TO",
                                        "Tuvalu" = "TV", "USA" = "US", "Vietnam" = "VN", "Vanuatu" = "VU", "Wallis/Futuna" = "WF")),
                          br(),
                          selectInput("sum.type", "Look at all fishing in your EEZ or fishing by your vessels:",
                                      c("Fishing in your EEZ" = "EEZ",
                                        "Fishing by your flagged/charter vessels" = "flag")),
                          checkboxGroupInput("grs", "Fishing methods to display:",
                                             c("Purse Seine (S)" = "S", "Longline (L)" = "L", "Pole and Line (P)" = "P", "Troll (T)" = "T", "Other gears (Z)" = "Z"),
                                             selected = c("S","L","P","T","Z")),
                          br(),
                          # selectInput("spatial.ext", "Choose the region:",
                          #             c("WCPO - WCPFC convention area" = "wcpo",
                          #               "Whole of Pacific" = "full.pacific")),
                          # br(),
                          checkboxGroupInput("species", "Choose your species:",
                                             c("Skipjack" = "skj_mt", "Yellowfin" = "yft_mt", "Bigeye" = "bet_mt", "Albacore" = "alb_mt"),
                                             selected = c("skj_mt","yft_mt","bet_mt","alb_mt")),
                          br(),
                          sliderInput("sliderrng", "Choose reference catch period",  min = 1990, max = latest.yr, value = c(latest.yr - 9,latest.yr),
                                      width = "400px", ticks = TRUE, sep = ""),
                          br()
             ),
             mainPanel(
               fluidRow(column(9, plotOutput("TotCatch")), column(3, plotlyOutput("Gear.pie"))),
               br(),
               br(),
               #plotOutput("SelCatch"),
               fluidRow(column(9, plotOutput("SelCatch")), column(3, plotlyOutput("Country.pie"))),
               #tableOutput("Cattbil")
               #plotlyOutput("PlotlyPlot")
             )
           )
  ),
  tabPanel("Catch map",
           mainPanel(
             plotOutput("MapPlot.all")
           )
  ),
  tabPanel("Catch map species",
           mainPanel(
             plotOutput("MapPlot.4spp")
           )
  )
  
)


#____________________________________________________________________________________________________________
# The server


server <- function(input, output) {
  
  
  scen.scl2 <- reactive({
    if(input$scenarios == "TRPOvr"){
      sclr <- 0.0275
    } else {
      sclr <- NA
    }
  })
  
  scen.isLL <- reactive({
    if(input$scenarios == "TRPLLon"){
      sclr <- NA
    } else {
      sclr <- "Apply.to.troll"
    }
  })
  
  
#____________________________________________________________________________________________________________
# Do the total catch calculations  
  
  plot.dat <- reactive({
    
    if(is.null(input$grs)){
      
      acedat.gr <- acedat
      
    } else{
      
      acedat.gr <- acedat %>% filter(gear %in% c(input$grs))
      
    }
    
    if(input$sum.type == "EEZ"){
      
      acedat.gr %<>% mutate(resp.var = ez_ag)
      
    } else{
      
      acedat.gr %<>% mutate(resp.var = flag)
      
    }
    
    tmpdat <- acedat.gr %>% select(input$species) %>% mutate(TotCat = rowSums(.))
    

    if(input$country == "all.cnt"){
      acepl <- acedat.gr %>% mutate(Category = "Other", TotCat = tmpdat$TotCat)
      acepl$Category <- factor(acepl$Category, levels = "Other")
    } else{
      
      acepl <- acedat.gr %>% mutate(Category = ifelse(resp.var == input$country, cnt.nms$lng.nm[cnt.nms$sht.nm == input$country], "Other"), TotCat = tmpdat$TotCat)
      acepl$Category <- factor(acepl$Category, levels = c("Other", cnt.nms$lng.nm[cnt.nms$sht.nm == input$country]))
    }
    
    acepl.yr <- acepl %>% group_by(Year, Category) %>% summarise(Catch = sum(TotCat))
    
    ace.pc.gr <- acepl %>% filter(Year >= input$sliderrng[1], Year <= input$sliderrng[2]) %>% group_by(Gear = gear) %>%
                           summarise(Catch = sum(TotCat)) %>% mutate(CatP = Catch/length(input$sliderrng[1]:input$sliderrng[2]))
     
    ace.pc.cnt <- acepl %>% filter(Year >= input$sliderrng[1], Year <= input$sliderrng[2]) %>% group_by(Category) %>%
                            summarise(Catch = sum(TotCat)) %>% mutate(CatP = Catch/length(input$sliderrng[1]:input$sliderrng[2]))
    
    if(input$country == "all.cnt"){
      selpl <- acepl %>% group_by(Year) %>% summarise(Catch = sum(TotCat))
    } else{
      
      if(input$country %in% acepl$resp.var){
        selpl <- filter(acepl, Category == cnt.nms$lng.nm[cnt.nms$sht.nm == input$country]) %>% group_by(Year) %>% summarise(Catch = sum(TotCat))
        tmp.yr <- data.frame(Year = 1990:latest.yr)
        selpl <- left_join(tmp.yr, selpl, by = "Year")
      } else{
        selpl <- data.frame(Year = 1990:latest.yr, Catch = 0)
      }
    }
    
    dat <- list(acepl.yr, ace.pc.gr, selpl, ace.pc.cnt)
    
  })
  
  
  #____________________________________________________________________________________________________________
  # Plot total longline calculations  
  
  output$TotCatch <- renderPlot({
    
    # if(length(unique(plot.dat()[[1]])) > 1){
    #   cnt.cols <- c("slategray4", "steelblue3")
    # } else{
    #   cnt.cols <- c("slategray4")
    # }
    cnt.cols <- c("slategray4", "magenta2")
    
    mu.catch <- plot.dat()[[1]] %>% filter(Year >= input$sliderrng[1], Year <= input$sliderrng[2]) %>% summarise(Catch = sum(Catch))
    
    pl1 <- ggplot(plot.dat()[[1]], aes(x = as.character(Year), y = Catch/1000)) +
                  geom_rect(aes(xmin = length(1990:latest.yr) - (latest.yr - input$sliderrng[1]) - 0.5,
                            xmax = length(1990:latest.yr) - (latest.yr - input$sliderrng[2]) + 0.5,
                            ymin = -Inf, ymax = Inf), fill = "palegreen1") +
                  geom_bar(aes(fill = Category), stat = "identity", width = 0.8) +
                  xlab("Year") + ylab("Catch (1,000's mt)") + guides(fill = guide_legend(nrow = 1)) +
                  scale_y_continuous(limits = c(0,3000)) +
                  scale_x_discrete(breaks = seq(1, latest.yr, by = 1)) +
                  scale_fill_manual(values = cnt.cols) +
                  theme_few() +
                  theme(axis.text.x = element_text(size = 11, angle = 90), axis.text.y = element_text(size = 14), axis.title = element_text(size = 18),
                        legend.position = "top", legend.title = element_blank(), legend.text = element_text(size = 12), legend.key.size = unit(0.8, "cm"),
                        legend.spacing.x = unit(0.2, "cm"), axis.title.x = element_blank(), panel.border = element_blank(),
                        axis.line = element_line(colour = "grey"))
    
    print(pl1)
    
    # pl.cols <- gr.cols$grcols[match(unique(plot.dat()[[2]]$Gear),gr.cols$sht.nm)]
    # 
    # pl2 <- ggplot(plot.dat()[[2]], aes(x = "", y = Catch, fill = Gear)) +
    #               geom_bar(width = 1, stat = "identity", color = "white") +
    #               coord_polar("y", start = 0) +
    #               #geom_text(aes(y = lab.ypos, label = prop), color = "white")+
    #               scale_fill_manual(values = pl.cols) +
    #               theme_void()
    # 
    # grid.arrange(pl1, pl2, ncol = 2, widths = c(3,1))
    
  })
  
  output$SelCatch <- renderPlot({
    
    pl1 <- ggplot(plot.dat()[[3]], aes(x = as.character(Year), y = Catch/1000)) +
                  geom_rect(aes(xmin = length(1990:latest.yr) - (latest.yr - input$sliderrng[1]) - 0.5,
                            xmax = length(1990:latest.yr) - (latest.yr - input$sliderrng[2]) + 0.5,
                            ymin = -Inf, ymax = Inf), fill = "palegreen1") +
                  geom_bar(stat = "identity", fill = "slategray4", width = 0.8) +
                  xlab("Year") + ylab("Catch (1,000's mt)") +
                  scale_x_discrete(breaks = seq(1, latest.yr, by = 1)) +
                  theme_few() +
                  theme(axis.text.x = element_text(size = 11, angle = 90), axis.text.y = element_text(size = 14), axis.title = element_text(size = 18),
                        legend.position = "top", legend.title = element_blank(), legend.text = element_text(size = 12), legend.key.size = unit(0.8, "cm"),
                        legend.spacing.x = unit(0.2, "cm"), axis.title.x = element_blank(), panel.border = element_blank(),
                        axis.line = element_line(colour = "grey"))
    
    print(pl1)
    
    # cnt.cols <- c("slategray4", "steelblue3")
    # 
    # pl2 <- ggplot(plot.dat()[[4]], aes(x = "", y = Catch, fill = Category)) +
    #               geom_bar(width = 1, stat = "identity", color = "white") +
    #               coord_polar("y", start = 0) +
    #               scale_fill_manual(values = cnt.cols) +
    #               geom_text(aes(y = Catch, label = round(CatP*100, 1)), color = "black", size = 9)+
    #               #scale_fill_manual(values = mycols) +
    #               theme_void() +
    #               theme(legend.title = element_blank())
    # 
    # grid.arrange(pl1, pl2, ncol = 2, widths = c(3,1))
    
  })
  
  output$Cattbil <- renderTable({
    
    tmpdat <- plot.dat()[[2]]
    
    # tmpdat <- acedat %>% filter(gear %in% c(input$grs)) %>% group_by(gear) %>% summarise(tmp = sum(tot_mt))
    # 
    # if(is.null(input$grs)) tmpdat = data.frame(a = 0, b = 1)
    
    head(tmpdat)

    
  })
  
  
  
  #____________________________________________________________________________________________________________
  # Plot total longline calculations  
  
  
  map.dat <- reactive({
    

    if(input$sum.type == "flag" & input$country != "all.cnt"){
      
      mapdat.cnt <- filter(mapdat, flag == input$country, yy >= input$sliderrng[1], yy <= input$sliderrng[2])
      
    } else{
      
      mapdat.cnt <- mapdat %>% filter(yy >= input$sliderrng[1], yy <= input$sliderrng[2])
      
    }
    
    
    if(is.null(input$grs)){
      
      mappl <- mapdat %>% filter(yy >= input$sliderrng[1], yy <= input$sliderrng[2]) %>% mutate(tot_mt = alb_mt + bet_mt + skj_mt + yft_mt) %>% group_by(lond, latd) %>%
                 summarise(Catch = sum(tot_mt), Catskj = sum(skj_mt), Catyft = sum(yft_mt), Catbet = sum(bet_mt), Catalb = sum(alb_mt))
      
      mappl.cnt <- mapdat.cnt %>% mutate(tot_mt = alb_mt + bet_mt + skj_mt + yft_mt) %>% group_by(lond, latd) %>%
                     summarise(Catch = sum(tot_mt), Catskj = sum(skj_mt), Catyft = sum(yft_mt), Catbet = sum(bet_mt), Catalb = sum(alb_mt))
      
    } else{
      
      mappl <- mapdat %>% filter(gear %in% c(input$grs), yy >= input$sliderrng[1], yy <= input$sliderrng[2]) %>% mutate(tot_mt = alb_mt + bet_mt + skj_mt + yft_mt) %>%
                          group_by(lond, latd) %>%
                 summarise(Catch = sum(tot_mt), Catskj = sum(skj_mt), Catyft = sum(yft_mt), Catbet = sum(bet_mt), Catalb = sum(alb_mt))
      
      mappl.cnt <- mapdat.cnt %>% filter(gear %in% c(input$grs)) %>% mutate(tot_mt = alb_mt + bet_mt + skj_mt + yft_mt) %>%
                                  group_by(lond, latd) %>%
                     summarise(Catch = sum(tot_mt), Catskj = sum(skj_mt), Catyft = sum(yft_mt), Catbet = sum(bet_mt), Catalb = sum(alb_mt))
      
    }
    
    
    regmap <- ggplot() +
      geom_sf(data = world,
              #color = "black",
              fill = "wheat") +
      coord_sf(
        xlim = c(100, 245),
        ylim = c(-50, 55),
        expand = FALSE
      ) +
      theme(
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
      )
    
    list(mappl, mappl.cnt, regmap)
    
  })
  
  output$MapPlot.all <- renderPlot({
    mappl <- map.dat()[[1]]
    pl <- map.dat()[[3]] + geom_point(data = mappl, aes(x = lond, y = latd, size = Catch/1000), colour = alpha("dodgerblue", .7)) + scale_size(range = c(0, 20)) +
                           annotate("text", x = 240, y = 52, label = "Catch - all species (1,000's mt)", hjust = 1, size = 12, colour = grey(0.6))
          #geom_sf(data = pac.eez, fill = alpha("blue", 0.1)) + coord_sf(xlim = c(100, 245), ylim = c(-50, 55), expand = FALSE)
    
    if(input$sum.type == "flag" & input$country != "all.cnt") pl <- pl + geom_point(data = map.dat()[[2]], aes(x = lond, y = latd, size = Catch/1000), shape = 1, colour = alpha("magenta", .99))
    
    print(pl)
  }, height = 900, width = 1100)
  
  output$MapPlot.skj <- renderPlot({
    mappl <- map.dat()[[1]]
    map.dat()[[3]] + geom_point(data = mappl, aes(x = lond, y = latd, size = Catskj/1000), colour = alpha("dodgerblue", .7)) + scale_size(range = c(0, 12))
  })#, height = 1000, width = 1200)
  
  output$MapPlot.yft <- renderPlot({
    mappl <- map.dat()[[1]]
    map.dat()[[3]] + geom_point(data = mappl, aes(x = lond, y = latd, size = Catyft/1000), colour = alpha("goldenrod1", .7)) + scale_size(range = c(0, 12))
  })#, height = 1000, width = 1200)
  
  output$MapPlot.bet <- renderPlot({
    mappl <- map.dat()[[1]]
    map.dat()[[3]] + geom_point(data = mappl, aes(x = lond, y = latd, size = Catbet/1000), colour = alpha("red", .7)) + scale_size(range = c(0, 12))
  })#, height = 1000, width = 1200)
 
  output$MapPlot.alb <- renderPlot({
    mappl <- map.dat()[[1]]
    map.dat()[[3]] + geom_point(data = mappl, aes(x = lond, y = latd, size = Catalb/1000), colour = alpha("forestgreen", .7)) + scale_size(range = c(0, 12))
  })#, height = 1000, width = 1200)
  
  
  output$MapPlot.4spp <- renderPlot({
    mappl <- map.dat()[[1]]
    
    pl.skj <- map.dat()[[3]] + geom_point(data = mappl, aes(x = lond, y = latd, size = Catskj/1000), colour = alpha("dodgerblue", .7)) + scale_size(range = c(0, 12)) +
                               annotate("text", x = 240, y = 52, label = "Catch - skipjack (1,000's mt)", hjust = 1, size = 7, colour = grey(0.6)) +
                               guides(size = guide_legend(title="Catch"))
    pl.yft <- map.dat()[[3]] + geom_point(data = mappl, aes(x = lond, y = latd, size = Catyft/1000), colour = alpha("goldenrod1", .85)) + scale_size(range = c(0, 12)) +
                               annotate("text", x = 240, y = 52, label = "Catch - yellowfin (1,000's mt)", hjust = 1, size = 7, colour = grey(0.6)) +
                               guides(size = guide_legend(title="Catch"))
    pl.bet <- map.dat()[[3]] + geom_point(data = mappl, aes(x = lond, y = latd, size = Catbet/1000), colour = alpha("red", .7)) + scale_size(range = c(0, 12)) +
                               annotate("text", x = 240, y = 52, label = "Catch - bigeye (1,000's mt)", hjust = 1, size = 7, colour = grey(0.6)) +
                               guides(size = guide_legend(title="Catch"))
    pl.alb <- map.dat()[[3]] + geom_point(data = mappl, aes(x = lond, y = latd, size = Catalb/1000), colour = alpha("forestgreen", .7)) + scale_size(range = c(0, 12)) +
                               annotate("text", x = 240, y = 52, label = "Catch - albacore (1,000's mt)", hjust = 1, size = 7, colour = grey(0.6)) +
                               guides(size = guide_legend(title="Catch"))
    
    lay.mat <- rbind(c(1,2),
                     c(3,4))
    
    grid.newpage()
    grid.arrange(pl.skj, pl.yft, pl.bet, pl.alb, layout_matrix=lay.mat, nrow=2)
    #pl.alb
    
    }, height = 1000, width = 1200)
  
  
  # pl.cols <- gr.cols$grcols[match(unique(plot.dat()[[2]]$Gear),gr.cols$sht.nm)]
  # 
  # pl2 <- ggplot(plot.dat()[[2]], aes(x = "", y = Catch, fill = Gear)) +
  #               geom_bar(width = 1, stat = "identity", color = "white") +
  #               coord_polar("y", start = 0) +
  #               #geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  #               scale_fill_manual(values = pl.cols) +
  #               theme_void()
  # 
  # grid.arrange(pl1, pl2, ncol = 2, widths = c(3,1))
  
  output$Gear.pie <- renderPlotly({
    
    # if(dim(plot.dat()[[4]])[1] == 1) cnt.cols <- c("#6C7B8B")
    # if(dim(plot.dat()[[4]])[1] == 2) cnt.cols <- c("#6C7B8B","#EE00EE")   # These are the hexadecimals for slategray4 and magenta2 from gplots::col2hex()
    
    plot_ly(plot.dat()[[2]], labels=~Gear, values=~round(CatP), type = "pie", #marker = list(colors = cnt.cols), sort = FALSE,
            textposition = "ouside", textinfo = "label+value", width = 400, height = 400) %>% config(displayModeBar = FALSE) %>%
      layout(title = "Proportion of catch by gear", #plot_bgcolor='transparent', paper_bgcolor='transparent',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  
  
  
  
  
  output$Country.pie <- renderPlotly({
    
    if(dim(plot.dat()[[4]])[1] == 1) cnt.cols <- c("#6C7B8B")
    if(dim(plot.dat()[[4]])[1] == 2) cnt.cols <- c("#6C7B8B","#EE00EE")   # These are the hexadecimals for slategray4 and magenta2 from gplots::col2hex()
    
    plot_ly(plot.dat()[[4]], labels=~Category, values=~round(CatP), type = "pie", marker = list(colors = cnt.cols), sort = FALSE,
            textposition = "ouside", textinfo = "label+value", width = 400, height = 400) %>% config(displayModeBar = FALSE) %>%
      layout(title = "Proportion of catch for chosen country", #plot_bgcolor='transparent', paper_bgcolor='transparent',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  
 
}

shinyApp(ui = ui, server = server)




























