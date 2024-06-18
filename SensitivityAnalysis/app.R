library(shiny)
library(readr)
HERMES_INCLUDED_sensitivity <- read_csv("HERMES_INCLUDED_sensitivity.csv")




ui <- fluidPage(

    # Application title
    titlePanel("Sensitivity Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("rob",
                        "Risk of bias item:",
                        choices = unique(HERMES_INCLUDED_sensitivity$subset_sensitivity))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  HERMES_INCLUDED_sensitivity%>%
    filter(subset_sensitivity==input$rob) %>% 
    mutate(Tested.drug.s.=str_split(Tested.drug.s., ',')) %>% 
    unnest(Tested.drug.s.) %>% 
    mutate(Tested.drug.s.=trimws(Tested.drug.s.)) %>% 
    ungroup() 
  
  HERMES_INCLUDED_splitting_species <- HERMES_INCLUDED%>%
    mutate(Species = str_split(Species_in_study, ", ")) %>% 
    unnest(Species) %>%
    mutate(Species = case_when(
      Species == "monkeys" ~ "monkey",
      TRUE ~ Species)) #this should run only if we decide to split the species column
  
  
  Drug_CV_Data <- read_excel("MS_translation_extraction.xlsx", 
                             sheet = "Drug_CV", na="")%>%
    mutate_all(~replace(., . == "NA", "NR")) %>%
    mutate_all(as.character) %>%
    mutate_all(~replace(., . == "NR", NA))%>%
    rename(`First in human trial`=First_human) %>% 
    filter(!is.na(`First in human trial`))
  
  
  drug_info_df<-merge(rbind(
    HERMES_INCLUDED%>% 
      filter(exclude_duplicate_study=="no") %>% group_by(Tested.drug.2,status2) %>% count() %>% rename(drugs=1,status=2),
    HERMES_INCLUDED %>% 
      filter(exclude_duplicate_study=="no") %>% group_by(Tested.drug.s.,status) %>% count() %>% rename(drugs=1,status=2)) %>% 
      rename(drug_status=status,Name=drugs) %>% 
      select(!(n)) %>% unique() %>% drop_na(),
    Drug_CV_Data %>% 
      rename(`FDA Approval for MS`=FDA_approval,Name=Drug) %>% 
      select(Name,`FDA Approval for MS`,`First in human trial`),by="Name")
  
  df_fig_6<-merge(drug_info_df,
                  HERMES_INCLUDED %>% 
                    filter(exclude_duplicate_study=="no")%>%
                    pivot_longer(cols = starts_with("Tested.drug"),
                                 names_to = "Drug",
                                 values_to = "Drug name") %>% 
                    rename(Name=`Drug name`) %>%
                    drop_na(Name),by="Name")
  
  df_row1<-rbind(HERMES_INCLUDED_splitting_species%>%
                   filter(exclude_duplicate_study=="no") %>% 
                   mutate(Species=gsub("mosue","mouse",Species)) %>% 
                   group_by(Species) %>%
                   summarise(Frequency = n()) %>%
                   ungroup() %>% 
                   mutate(Species=str_to_title(ifelse(Frequency<=5,"Other",Species))) %>% 
                   group_by(Species) %>% 
                   summarise(Frequency=sum(Frequency)) %>% 
                   mutate(item="A. Species") %>% 
                   rename(Category=Species,n=Frequency),
                 HERMES_INCLUDED %>%
                   filter(exclude_duplicate_study=="no") %>% 
                   group_by(Animal.model) %>%
                   summarise(Frequency = n()) %>%
                   mutate(Animal.model=str_split(Animal.model, ',')) %>% 
                   unnest(Animal.model) %>% 
                   mutate(Animal.model=trimws(Animal.model)) %>% 
                   ungroup() %>% 
                   group_by(Animal.model) %>%
                   summarise(Frequency = sum(Frequency)) %>% 
                   mutate(Animal.model=ifelse(Frequency<=5,"Other",Animal.model)) %>%   
                   ungroup() %>% 
                   group_by(Animal.model) %>%
                   summarise(Frequency = sum(Frequency)) %>% 
                   mutate(Animal.model=case_when(
                     Animal.model=="cuprizone"~"Cuprizone",
                     Animal.model=="lysolecithin"~"Lysolecithin",
                     Animal.model=="ethidium bromide"~"Ethidium bromide",
                     TRUE~Animal.model
                   ))%>% 
                   mutate(item="B. Animal model") %>% 
                   rename(Category=Animal.model,n=Frequency))
  
  labels_row1<-list("A. Species"=bquote(bold("A.")~"Species"),"B. Animal model"=bquote(bold("B.")~"Animal model"))
  labeller_row1 <- function(variable,value){
    return(labels_row1[value])}
  
  plot_row1<-
    df_row1%>% 
    mutate(Category=gsub("TMEV-IDD","TMEV",Category)) %>% 
    ggplot(aes(x = reorder(Category, -n), y = n)) +
    geom_bar(stat = "identity", fill = "#b6b6b6",color="#929292") +
    labs(x = "Animal Model", y = "Number of studies") +
    ylim(0, 440) +
    common_theme+
    facet_wrap(.~item,scales = "free_x",labeller = labeller_row1)+
    theme(
      strip.text.x = element_text(hjust=0,size=12),
      panel.spacing = unit(1.5, "lines"))
  
  
  df_row2<-rbind(HERMES_INCLUDED %>%
                   filter(exclude_duplicate_study=="no") %>% 
                   mutate(Sex=str_to_title(gsub("\\bboth\\b","both sexes",Sex))) %>% 
                   group_by(Sex) %>%
                   summarise(Frequency = n()) %>%
                   ungroup() %>% 
                   mutate(Sex=gsub("Na",NA,Sex),Sex=gsub("Both Sexes","Both sexes",Sex))%>% 
                   mutate(item="C. Sex") %>% 
                   rename(Category=Sex,n=Frequency),
                 HERMES_INCLUDED %>%
                   filter(exclude_duplicate_study=="no") %>% 
                   group_by(Strain) %>%
                   summarise(Frequency = n()) %>%
                   mutate(Strain=str_split(Strain, ',')) %>% 
                   unnest(Strain) %>% 
                   mutate(Strain=trimws(Strain)) %>% 
                   ungroup() %>% 
                   group_by(Strain) %>%
                   summarise(Frequency = sum(Frequency)) %>% 
                   mutate(Strain=ifelse(Frequency<=5,"Other",Strain)) %>%   
                   ungroup() %>% 
                   group_by(Strain) %>%
                   summarise(Frequency = sum(Frequency)) %>% 
                   mutate(Strain=gsub("-"," ",Strain)) %>% 
                   mutate(item="D. Strain") %>% 
                   rename(Category=Strain,n=Frequency))
  
  labels_row2<-list("C. Sex"=bquote(bold("C.")~"Sex"),"D. Strain"=bquote(bold("D.")~"Strain"))
  labeller_row2 <- function(variable,value){
    return(labels_row2[value])}
  
  plot_row2<-df_row2%>% 
    mutate(Category=gsub("TMEV-IDD","TMEV",Category)) %>% 
    ggplot(aes(x = reorder(Category, -n), y = n)) +
    geom_bar(stat = "identity", fill = "#b6b6b6",color="#929292") +
    labs(x = "Animal Model", y = "Number of studies") +
    ylim(0, 320) +
    common_theme+
    facet_grid(. ~ item,scales = "free", labeller=labeller_row2)+
    theme(
      strip.text.x = element_text(hjust=0,size=12),
      panel.spacing = unit(1.5, "lines"))
  
  p3<-rbind(HERMES_INCLUDED %>% 
              filter(exclude_duplicate_study=="no") %>% group_by(Tested.drug.2,status2) %>% count() %>% rename(drugs=1,status=2),
            HERMES_INCLUDED %>% 
              filter(exclude_duplicate_study=="no") %>% group_by(Tested.drug.s.,status) %>% count() %>% rename(drugs=1,status=2)) %>% 
    data.frame() %>% 
    group_by(drugs,status) %>% 
    summarise(n=sum(n)) %>% 
    mutate(across(everything(),~ gsub("NA",NA, .))) %>% 
    unique() %>% 
    drop_na()%>% 
    mutate(status=ifelse(status=="Marketed","Approved DMTs","Failed DMTs")) %>% 
    mutate(n=as.numeric(n)
    ) %>% 
    mutate(drugs=case_when(
      drugs=="Interferon Beta 1"~"Interferon beta-1",
      TRUE~drugs
    )) %>%
    ggplot(aes(x = reorder(drugs, -n), y = n,fill=status,color=status)) +
    geom_bar(stat = "identity") +
    labs(x = "Tested Drugs", y ="Number of studies") +
    common_theme+
    scale_fill_manual(values=c("Approved DMTs"="#6896ca","Failed DMTs"="#ff7b74"))+
    scale_color_manual(values=c("Approved DMTs"="#567ca7","Failed DMTs"="#d0645e"))+
    theme(legend.title=element_blank())
  
  
    output$Plot <- renderPlot({
        
      grid.arrange(plot_row1,plot_row2,
                   p3+ggtitle(bquote(bold("E.")~"Tested drugs"))+
                     theme(legend.position = "bottom",
                           legend.text = element_text(size=13,color="gray20")),
                   ncol=1,heights=c(1,1,1.5))
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
