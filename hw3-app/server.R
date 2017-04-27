library(shiny)
library(lubridate)
library(tidyr)
library(plyr)
library(dplyr)
library(ggvis)

shinyServer(function(input, output){
  
  # Functions
  normalize <- function(x){
    return((x-min(x))/(max(x)-min(x)))}
    
  # scatter plot function
  # http://stackoverflow.com/questions/30858337/how-to-customize-lines-in-ggpairs-ggally
  lowerFn <- function(data, mapping) {
    ggplot(data = data, mapping = mapping) +
      geom_point(colour = "black") +
      geom_smooth(method = "lm", color = "darkblue", size=0.75)}
  
  # Data import - diabetes
  # Emergency Admissions Only
  df <- read.csv("diabetic_data.csv") %>%
    filter(discharge_disposition_id %in% c(1,11)) %>%
    mutate(outcome = ifelse(discharge_disposition_id==1, 1, 0)) %>%
    filter(admission_type_id == 1) %>%
    mutate(age = mapvalues(age, 
                           from = c("[0-10)","[10-20)","[20-30)","[30-40)","[40-50)","[50-60)","[60-70)",
                                    "[70-80)","[80-90)","[90-100)"), 
                           to = c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95))) %>%
    mutate(age = as.numeric(as.character(age))) %>%
    mutate(gender = mapvalues(gender, 
                              from = c('Female','Male','Unknown/Invalid'), 
                              to = c(1,0,99))) %>%
    mutate(gender = as.numeric(as.character(gender))) %>%
    filter(gender != 99) %>%
    select(c(one_of('encounter_id','discharge_disposition_id','admission_type_id', 'outcome','gender','age',
                    'time_in_hospital','num_lab_procedures','num_procedures',
                    'num_medications','number_outpatient','number_inpatient',
                    'number_emergency'))) %>%
    rename(Outcome = outcome,Gender =gender,Age=age,
           Stay.Length=time_in_hospital,Lab.Procedures=num_lab_procedures,
           Non.Lab.Procedures=num_procedures,
           Medications=num_medications,Outpatient=number_outpatient,
           Inpatient=number_inpatient,Emergency=number_emergency) %>%
    group_by(Outcome) %>% 
    sample_n(100) %>%
    ungroup()
  
  # Data import - facebook
  df_fb <- read.csv("dataset_Facebook.csv", sep=';')
  df_fb_pp <- df_fb %>%
    mutate(Post.Weekday = recode(Post.Weekday, 
                                 `1` ='Sunday',
                                 `2` ='Monday',
                                 `3` ='Tuesday',
                                 `4` ='Wednesday',
                                 `5` ='Thursday',
                                 `6` ='Friday',
                                 `7` ='Saturday')) %>%
    select(one_of(c('Post.Month','Post.Weekday','Post.Hour','Type',
                    'comment','like','share'))) %>%
    dplyr::group_by(Post.Month, Post.Weekday, Post.Hour, Type) %>%
    mutate(num_posts = n()) %>%
    dplyr::group_by(Post.Month, Post.Weekday, Post.Hour, Type, num_posts) %>%
    summarize_each(funs(sum)) %>%
    mutate(likepp = like/num_posts) %>%
    mutate(sharepp = share/num_posts) %>%
    mutate(commentpp = comment/num_posts)
  
  # Parallel coordinates dataframes
  df_par_1 <- df[c('encounter_id','Outcome')]
  df_par_2 <- df %>%
    select(5:13) %>%
    mutate_each(funs(normalize))
  
  df_par <- bind_cols(df_par_1, df_par_2) %>%
    gather('Var','Value',3:11) %>%
    mutate(Outcome = factor(ifelse(Outcome==1, 'Discharged Home','Died')))
  
  # Parallel Coords Plots
  v_par <- reactive({
    df_par  %>%
      filter(Var %in% input$par_vars) %>%
      ggvis(x = ~Var, y = ~Value, stroke = ~Outcome) %>%
      group_by(encounter_id) %>%
      layer_paths(stroke.hover := "#FFCC00", 
                  strokeWidth.hover := 4,
                  opacity.hover := 0.8,
                  strokeWidth := 1,
                  opacity := 0.3) %>%
      layer_text(x:= 0, y = 0, text:='Max', stroke:='black', fontWeight:='lighter') %>%
      layer_text(x:= 0, y = 1, text:='Min', stroke:='black', fontWeight:='lighter') %>%
      scale_nominal("stroke", label = "Outcome", range = c('#CC0066','#3399FF')) %>%
      add_axis("x", 
               title = "",
               properties = axis_props(grid = list(strokeWidth = 2),
                                       axis = list(stroke = NULL),
                                       ticks = list(stroke = NULL))) %>%
      add_axis("y", 
               title = "",
               values=c(0,1),
               properties = axis_props(grid = list(stroke = NULL),
                                       axis = list(stroke = NULL),
                                       ticks = list(stroke = NULL),
                                       labels = list(text = c(''))))
  })
  
  v_par %>% bind_shiny("parallel")
  
  # Scatterplot
  output$multiscatter <- renderPlot({
    p <- df  %>%
      select(one_of(input$multi_vars)) %>%
      ggpairs(lower = list(continuous = wrap(lowerFn))) +
      theme_minimal(base_size = 14)
    print(p)
  })
  
  # Heatmap
  # Plot theme
  t <- theme(legend.position = c(0.55, 0.3),
             legend.justification = c(0, 1))
  
  # Heat map
  output$heatmap <- renderPlot({
    p <- df_fb_pp %>%
      filter((Post.Month >= input$months[1]) & (Post.Month <= input$months[2])) %>%
      ggplot(aes(x=Post.Hour, y=Post.Weekday)) +
      geom_tile(aes_string(fill=input$opacity_control)) +
      scale_fill_continuous(low='snow1', high='dodgerblue4', na.value='white', 
                            trans = "log", 
                            breaks= signif(exp(seq(0,
                                                   1.1*log(max(df_fb_pp[input$opacity_control], na.rm=TRUE)), 
                                                   length.out = 5)),1)) +
      scale_x_continuous(breaks=seq(1,24), limits=c(0.5,24.5), expand=c(0,0)) +
      scale_y_discrete(limits=c('Sunday','Monday','Tuesday','Wednesday','Thursday',
                                'Friday','Saturday')) +
      labs(x="Hour", y="Weekday") +
      theme_minimal(base_size = 14) +
      theme(legend.title = element_blank(),
            panel.grid.major = element_blank())
    
    if (input$is_split) {
      p <- p + facet_grid(Type ~., drop=FALSE)
    }
    print(p)
  })
  
})