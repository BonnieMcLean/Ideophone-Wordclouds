library(shiny)
library(leaflet)
library(wordcloud2)
library(dplyr)
library(tidyr)
library(rlang)

data<-read.csv('responses-wordcloud.csv',encoding = 'UTF-8')
locations<-read.csv('locations.csv')
participants<-read.csv('participantdat.csv')
data<-merge(data,participants,by='participant')

locations$location<-as.factor(locations$location)
ids<-subset(data,response.type=='ideophone')
ids %>%
    group_by(stimulus) %>%
    count(response_ipa)->wordclouddat

test<-data.frame(ids$stimulus,ids$response_ipa,ids$exactlocation)
names(test)<-c('stimulus','response_ipa','exactlocation')

test %>%
    group_by(stimulus,response_ipa,exactlocation)%>%
    summarise(Count=n())->teste
teste$Count<-as.factor(teste$Count)
unite(teste,'infor',exactlocation:Count,sep=': ')->d1

d1%>%
    group_by(stimulus,response_ipa)%>%
    mutate(info=paste(infor,collapse = ', '))->d2

d2$infor<-NULL
idinfo<-unique(d2)
ids$stimulus.type<-as.factor(ids$stimulus.type)
ids$stimulus<-as.factor(ids$stimulus)
ids$stimulus.type <- factor(ids$stimulus.type, levels = c("sound", "movement", "shape","colour","tactile","smell","taste","inner perc."))

sound<-subset(ids,stimulus.type=='sound')
movement<-subset(ids,stimulus.type=='movement')
shape<-subset(ids,stimulus.type=='shape')
colour<-subset(ids,stimulus.type=='colour')
tactile<-subset(ids,stimulus.type=='tactile')
smell<-subset(ids,stimulus.type=='smell')
taste<-subset(ids,stimulus.type=='taste')
inner<-subset(ids,stimulus.type=='inner perc.')

ui <- fluidPage(
    titlePanel("Ideophones across Japan"),
    tabsetPanel(
        tabPanel(title = 'About',
                 
                 mainPanel(
                     h2('About'),
                     h4('In 2018/2019 I collected descriptions of various sensory stimuli from people around Japan, to examine their use of ideophones (sound symbolic words) to depict these stimuli. This application summarises the results of the research and displays the data in an interactive way.'),
                     h4('To all the people who participated in this research, thank you so much for your time and I hope you enjoy looking at this data!'),
                     HTML("<p style='font-size:18px'>If you would like to know more about this research, you can read my <a href='https://doi.org/10.1515/lingty-2020-2063'>article in Linguistic Typology</a>.</p>"),
                     h4(''),
                     h4('Sincerely,'),
                     h4(''),
                     h4('Bonnie McLean') 
                     
                 )),
        tabPanel(
            title = 'Ideophones',
            sidebarPanel(
                selectInput('domain','Choose a type of stimulus',choices=levels(ids$stimulus.type),multiple = FALSE),
                conditionalPanel(
                    condition = "input.domain == 'sound'",
                    selectInput('soundstimulus','Choose a stimulus',choices=levels(droplevels(sound$stimulus)),multiple = FALSE,selected='03-water_dripping')
                ),
                conditionalPanel(
                    condition = "input.domain == 'movement'",
                    selectInput('movementstimulus','Choose a stimulus',choices=levels(droplevels(movement$stimulus)),multiple = FALSE)
                ),
                conditionalPanel(
                    condition = "input.domain == 'shape'",
                    selectInput('shapestimulus','Choose a stimulus',choices=levels(droplevels(shape$stimulus)),multiple = FALSE)
                ),
                conditionalPanel(
                    condition = "input.domain == 'colour'",
                    selectInput('colourstimulus','Choose a stimulus',choices=levels(droplevels(colour$stimulus)),multiple = FALSE)
                ),
                conditionalPanel(
                    condition = "input.domain == 'tactile'",
                    selectInput('tactilestimulus','Choose a stimulus',choices=levels(droplevels(tactile$stimulus)),multiple = FALSE)
                ),
                conditionalPanel(
                    condition = "input.domain == 'smell'",
                    selectInput('smellstimulus','Choose a stimulus',choices=levels(droplevels(smell$stimulus)),multiple = FALSE)
                ),
                conditionalPanel(
                    condition = "input.domain == 'taste'",
                    selectInput('tastestimulus','Choose a stimulus',choices=levels(droplevels(taste$stimulus)),multiple = FALSE)
                ),
                conditionalPanel(
                    condition = "input.domain == 'inner perc.'",
                    selectInput('innerperc.stimulus','Choose a stimulus',choices=levels(droplevels(inner$stimulus)),multiple = FALSE)
                ),
            ),
            mainPanel(
                wordcloud2Output('wordcloud'),
                tags$script(HTML(
                    "$(document).on('click', '#canvas', function() {",
                    'word = document.getElementById("wcSpan").innerHTML;',
                    "Shiny.onInputChange('selected_word', word);",
                    "});"
                )),
                h4('Click on the word to see the locations where it was used and by how many people.'), 
                verbatimTextOutput('test',placeholder = TRUE)

            )
                
        ),
        tabPanel(title = 'View Stimuli',
                 sidebarPanel(
                     selectInput('stimulidomain','Choose a group of stimuli',choices=levels(ids$stimulus.type),multiple = FALSE),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'sound'",
                         selectInput('viewstimulus','Choose a stimulus',choices=levels(droplevels(sound$stimulus)),multiple = FALSE)
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'movement'",
                         selectInput('viewmovestimulus','Choose a stimulus',choices=levels(droplevels(movement$stimulus)),multiple = FALSE)
                     )
                     ),
                 mainPanel(
                     conditionalPanel(
                         condition = "input.stimulidomain != 'sound' & input.stimulidomain != 'movement'",
                         imageOutput('stimuli',width = '100%', height='400px')
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'sound' & input.viewstimulus == '01-small_waterfall'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/JRF5h5RMmn4", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                         
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'sound' & input.viewstimulus == '02-large_waterfall'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/2p6zIe8sm_Q", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                         
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'sound' & input.viewstimulus == '03-water_dripping'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/Cj8XbFOYylc?start=268", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                         
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'sound' & input.viewstimulus == '04-bell'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/tms3tXHUv_0", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                         
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'sound' & input.viewstimulus == '08-tsukutsuku_cicada'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/fIXb61S9r4E?start=18", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'sound' & input.viewstimulus == '05-baby_laugh'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/JK7NpU00XA8?start=18", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'sound' & input.viewstimulus == '06-woman_laugh'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/aU8atlF0GWk", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'sound' & input.viewstimulus == '07-man_laugh'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/3INdJFowGCQ?start=1", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'sound' & input.viewstimulus == '09-miimii_cicada'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/1VNKDyrVJEM?start=89", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'sound' & input.viewstimulus == '10-cricket'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/sQPABz8VGGA", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'movement' & input.viewmovestimulus == '01-Marching'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/huPIswuPMA0?start=8", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'movement' & input.viewmovestimulus == '02-Sillywalk'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/F3UGk9QhoIw?start=101", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'movement' & input.viewmovestimulus == '03-Firststeps'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/zruLvBihBVA?start=4", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'movement' & input.viewmovestimulus == '04-Cheesecakejiggle'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/IBAhAk0-sPo?start=320", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'movement' & input.viewmovestimulus == '05-ballerinasinglespin'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/uTQkqzlJ0XA", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'movement' & input.viewmovestimulus == '06-ballerinamultiplespin'",
                         tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/JJGrDLgIlLU?start=42", frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA)
                     ),
                     conditionalPanel(
                         condition = "input.stimulidomain == 'inner perc.'",
                         "Cartoons were taken from ",
                         tags$a(href="https://tinyurl.com/y2blf74k", 
                                "Gomi (1989)")," or drawn in the same style."
                     )
                 )
                 ),
        tabPanel(title = 'View Locations',
                 sidebarPanel(
                     selectInput('location','Display select locations',choices = levels(locations$location),multiple = TRUE),
                 ),
                 mainPanel(
                     leafletOutput("map")
                 ))
        
    )
)




# Define server logic required to draw a histogram
server <- function(input, output) {
    mywordcloud <- reactive({
        domain<-sub(' ','',input$domain)
        i<-paste('input$',domain,'stimulus',sep='')
        data<-subset(wordclouddat,stimulus==eval(parse(text = i)))

        data
    })
    
    filtered_data <- reactive({
        data<-locations
        if (length(input$location) != 0) {
            data <- subset(locations,location %in% input$location)
        }
        data
    })
    
    output$wordcloud<-renderWordcloud2({
        dat<-mywordcloud()
        dat$stimulus<-NULL
        colnames(dat)<-c('word','freq')
        if (input$domain!='tactile'){wordcloud2(dat,size=.7)}
        else if (input$domain=='taste'){wordcloud2(dat,size=2)}
        else {
            wordcloud2(dat,size=.6)}
    })
    
    output$test<-renderText({
        dat<-mywordcloud()
        word<-gsub(":.*","",input$selected_word)
        stim<-as.character(unique(dat$stimulus))
        d<-subset(idinfo,response_ipa==word&stimulus==stim)
        variety<-as.character(unique(d$info))
       paste(variety)
    })
    
output$stimuli<-renderImage({
    if(input$stimulidomain=='inner perc.'){list(src='images/inner.jpg',contentType='image/jpg',alt='Alt text',width='85%',height='85%')}
    else{
    list(src=paste('images/',input$stimulidomain,'.jpg',sep=''),contentType='image/jpg',alt='Alt text',width='85%',height='85%')}
},deleteFile = FALSE)

output$map<-renderLeaflet({
    leaflet(filtered_data()) %>% addTiles() %>% addMarkers(~long,~lat,label = ~location, labelOptions = labelOptions(noHide = T))
})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
