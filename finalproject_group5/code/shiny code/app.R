# import library
library(shiny)
library(tidyverse)
# Radar Plot
library(fmsb)
# Correlation Plot
library(corrplot)
# Scatter Histogram Plot
library(ggpubr)
# grid.arrange function
library(gridExtra)

ui <- shinyUI(pageWithSidebar(
    
    headerPanel('Pokemon Battle Analysis'),
    
    sidebarPanel(width=3,
                 selectInput(inputId='Type1',
                             label='Type:',
                             choices=c('All','Dragon','Steel','Flying','Psychic','Rock',
                                       'Fire','Electric','Dark','Ghost','Ground','Ice',
                                       'Water','Grass','Fighting','Fairy','Poison','Normal',
                                       'Bug')),
                 selectInput(inputId='Generation',
                             label='Generation:',
                             choices=c('All','First','Second','Third','Fourth','Fifth','Sixth')),
                 selectInput(inputId='Ability',
                             label='Ability:',
                             choices=c('HP','Attack','Defense','Sp.Atk','Sp.Def','Speed','Total')),
                 selectInput(inputId='Classification',
                             label='Classification',
                             choices=c('Type.1','Generation','Legendary','Mega_Evolution')),
                 selectInput(inputId='Ability_X',
                             label='Ability Scatter plot X variable:',
                             choices=c('HP','Attack','Defense','Sp.Atk','Sp.Def','Speed')),
                 selectInput(inputId='Ability_Y',
                             label='Ability Scatter plot Y variable:',
                             choices=c('HP','Attack','Defense','Sp.Atk','Sp.Def','Speed')),
                 
                 img(src='Squirtle.png',height='30%',width='30%'),
                 img(src='Charmander.png',height='30%',width='30%'),
                 img(src='Bulbasaur.png',height='30%',width='30%'),
                 img(src='Totodile.png',height='30%',width='30%'),
                 img(src='Cyndaquil.png',height='30%',width='30%'),
                 img(src='Chikorita.png',height='30%',width='30%'),
                 img(src='Mudkip.png',height='30%',width='30%'),
                 img(src='Torchic.png',height='30%',width='30%'),
                 img(src='Treecko.png',height='30%',width='30%'),
                 img(src='Piplup.png',height='30%',width='30%'),
                 img(src='Chimchar.png',height='30%',width='30%'),
                 img(src='Turtwig.png',height='30%',width='30%')
    ),
    
    mainPanel(
        tabsetPanel(type='tabs',
                    tabPanel('Data',
                             h3('Raw Pokemon Data(n=800)'),
                             br(),
                             tableOutput('pokemon_table'),
                             br(),
                             h3('Training Battle Data(n=50000)'),
                             br(),
                             tableOutput('train_battle_table')),
                    tabPanel('Top 10 Ability of Pokemon Leaderboard',
                             h3('Top 10 Maximum Ability of Pokemon Leaderboard'),
                             br(),
                             tableOutput('max_pokemon_table'),
                             br(),
                             h3('Top 10 Minimum Ability of Pokemon Leaderboard'),
                             br(),
                             tableOutput('min_pokemon_table')),
                    tabPanel('Top 10 Ability of Pokemon',
                             h3('Top 10 Maximum Ability of Pokemon'),
                             img(src='Mega Mewtwo X.png',height='15%',width='15%'),
                             img(src='Mega Mewtwo Y.png',height='15%',width='15%'),
                             img(src='Mega Rayquaza.png',height='15%',width='15%'),
                             img(src='Primal Kyogre.png',height='15%',width='15%'),
                             img(src='Primal Groudon.png',height='15%',width='15%'),
                             img(src='Arceus.png',height='15%',width='15%'),
                             img(src='Mega Tyranitar.png',height='15%',width='15%'),
                             img(src='Mega Salamence.png',height='15%',width='15%'),
                             img(src='Mega Metagross.png',height='15%',width='15%'),
                             img(src='Mega Latios.png',height='15%',width='15%'),
                             h3('Top 10 Minimum Ability of Pokemon'),
                             img(src='Sunkern.png',height='15%',width='15%'),
                             img(src='Azurill.png',height='15%',width='15%'),
                             img(src='Kricketot.png',height='15%',width='15%'),
                             img(src='Caterpie.png',height='15%',width='15%'),
                             img(src='Weedle.png',height='15%',width='15%'),
                             img(src='Wurmple.png',height='15%',width='15%'),
                             img(src='Ralts.png',height='15%',width='15%'),
                             img(src='Magikarp.png',height='15%',width='15%'),
                             img(src='Feebas.png',height='15%',width='15%'),
                             img(src='Scatterbug.png',height='15%',width='15%')),
                    tabPanel('Top 10 Win Rate of Pokemon Leaderboard',
                             h3('Top 10 Maximum Win Rate of Pokemon Leaderboard'),
                             br(),
                             tableOutput('max_win_rate_df'),
                             br(),
                             h3('Top 10 Minimum Win Rate of Pokemon Leaderboard'),
                             br(),
                             tableOutput('min_win_rate_df')),
                    tabPanel('Top 10 Win Rate of Pokemon',
                             h3('Top 10 Maximum Win Rate of Pokemon'),
                             img(src='Mega Aerodactyl.png',height='15%',width='15%'),
                             img(src='Weavile.png',height='15%',width='15%'),
                             img(src='Tornadus Therian Forme.png',height='15%',width='15%'),
                             img(src='Mega Beedrill.png',height='15%',width='15%'),
                             img(src='Aerodactyl.png',height='15%',width='15%'),
                             img(src='Mega Lopunny.png',height='15%',width='15%'),
                             img(src='Greninja.png',height='15%',width='15%'),
                             img(src='Meloetta Pirouette Forme.png',height='15%',width='15%'),
                             img(src='Mega Mewtwo Y.png',height='15%',width='15%'),
                             img(src='Mega Sharpedo.png',height='15%',width='15%'),
                             h3('Top 10 Minimum Win Rate of Pokemon'),
                             img(src='Shuckle.png',height='15%',width='15%'),
                             img(src='Silcoon.png',height='15%',width='15%'),
                             img(src='Togepi.png',height='15%',width='15%'),
                             img(src='Solosis.png',height='15%',width='15%'),
                             img(src='Slugma.png',height='15%',width='15%'),
                             img(src='Munna.png',height='15%',width='15%'),
                             img(src='Igglybuff.png',height='15%',width='15%'),
                             img(src='Wynaut.png',height='15%',width='15%'),
                             img(src='Wooper.png',height='15%',width='15%'),
                             img(src='Cascoon.png',height='15%',width='15%')),
                    tabPanel('Type Plot',
                             h3('Pokemon\'s Type Histogram Plot'),
                             plotOutput('type_hist_plot',width='100%'),
                             br(),
                             h3('Pokemon Type\'s Radar Plot & Correlation Heatmap Plot'),
                             plotOutput('type_radar_heat_plot',width='90%')),
                    tabPanel('Generation Plot',
                             h3('Pokemon\'s Generation Histogram Plot'),
                             plotOutput('gener_hist_plot',width='100%'),
                             br(),
                             h3('Pokemon Generation\'s Radar Plot & Correlation Heatmap Plot'),
                             plotOutput('gener_radar_heat_plot',width='100%')),
                    tabPanel('Distribution Plot',
                             h3('Pokemon\'s Generation Bar Plot'),
                             plotOutput('gener_bar_plot',width='100%'),
                             br(),
                             h3('Pokemon\'s Ability Scatter Plot'),
                             plotOutput('abl_scatter_plot',width='100%')),
                    tabPanel('Win Rate Plot',
                             h3('Pokemon\'s Win Rate Plot'),
                             plotOutput('win_rate_ability_plot',width='100%'),
                             br(),
                             h3('Pokemon\'s Winner and Loser Ability Difference Histogram Plot'),
                             plotOutput('win_diff_hist_plot',width='100%')),
                    tabPanel('Summary',
                             h3('Pokemon Data Features\'s Statistics Summary'),
                             br(),
                             tableOutput('summary'),
                             br(),
                             h3('Pokemon\'s Type Xiangke Ability Table'),
                             br(),
                             tableOutput('type_xiangke_table')),
                    tabPanel('Cross Validation Result',
                             h3('Model Performance Result'),
                             br(),
                             h4('Logistic Regression Model'),
                             tableOutput('logistic_table'),
                             h4('Random Forest Model'),
                             tableOutput('randomforest_table'),
                             h4('XGBoost Model'),
                             tableOutput('xgboost_table')),
                    tabPanel('Model Result Table',
                             h3('Confusion Matrix & Score'),
                             br(),
                             h4('Logistic Regression Model'),
                             img(src='matrix_log.png',height='50%',width='50%'),
                             h4('Random Forest Model'),
                             img(src='matrix_rf.png',height='50%',width='50%'),
                             h4('XGBoost Model'),
                             img(src='matrix_xg.png',height='50%',width='50%')),
                    tabPanel('Model Result Plot',
                             h3('Random Forest Feature Importance Plot'),
                             img(src='varimportance.png',height='50%',width='50%'),
                             br(),
                             h3('Logistic Regression ROC & AUC'),
                             img(src='roc_curve.png',height='50%',width='50%')),
                    tabPanel('Reference',
                             h2('Reference Link'),
                             br(),
                             h3('Data Source'),
                             br(),
                             h4('Pokemon- Weedle\'s Cave'),
                             a(href='https://www.kaggle.com/terminus7/pokemon-challenge',
                               'https://www.kaggle.com/terminus7/pokemon-challenge',
                               style='font-size:20px;'),
                             br(),
                             h3('Domain Konwledge'),
                             br(),
                             h4('The Official Pokemon Website'),
                             a(href='https://www.pokemon.com/uk/',
                               'https://www.pokemon.com/uk/',
                               style='font-size:20px;'),
                             h4('Pokemon Wikipedia'),
                             a(href='https://wiki.52poke.com/',
                               'https://wiki.52poke.com/',
                               style='font-size:20px;'),
                             br(),
                             h3('Exploratory Data Analysis'),
                             br(),
                             h4('Pokemon Battles | Kaggle'),
                             a(href='https://www.kaggle.com/jonathanbouchet/pokemon-battles',
                               'https://www.kaggle.com/jonathanbouchet/pokemon-battles',
                               style='font-size:20px;'),
                             h4('Data ScienceTutorial for Beginners | Kaggle'),
                             a(href='https://www.kaggle.com/kanncaa1/data-sciencetutorial-for-beginners',
                               'https://www.kaggle.com/kanncaa1/data-sciencetutorial-for-beginners',
                               style='font-size:20px;'),
                             h4('Pokemon\'s Types and Features Exploratory Data Analysis | Zhihu'),
                             a(href='https://zhuanlan.zhihu.com/p/26787563',
                               'https://zhuanlan.zhihu.com/p/26787563',
                               style='font-size:20px;'))
        )
    )
))



server <- shinyServer(function(input,output,session){
    
    # chnge Generation column from numeric to string
    generation_func<-function(x){
        if(x=='1'){
            return('First')
        }else if(x=='2'){
            return('Second')
        }else if(x=='3'){
            return('Third')
        }else if(x=='4'){
            return('Fourth')
        }else if(x=='5'){
            return('Fifth')
        }else{
            return('Sixth')
        }
    }
    
    
    # read raw data
    pokemon<-read.csv('Data/pokemon.csv',header=T,stringsAsFactors=FALSE)
    train_battle<-read.csv('Data/combats.csv',header=T)
    logistic_df<-read.csv('Data/logistic.csv',header=T)
    randomforest_df<-read.csv('Data/randomforest.csv',header=T)
    xgboost_df<-read.csv('Data/xgboost.csv',header=T)
    
    
    # pokemon data preprocessing
    pokemon<-pokemon%>%
        rename(Id='X.',Sp.Atk='Sp..Atk',Sp.Def='Sp..Def')%>%
        mutate(Total=HP+Attack+Defense+Sp.Atk+Sp.Def+Speed,
               Generation=sapply(Generation,generation_func),
               Mega_Evolution=ifelse(str_detect(pokemon$Name,'Mega'),'True','False'))
    
    # solve data misplace problem
    pokemon[38,]<-list(Id=38,Name='Nidoran',Type.1='Poison',
                       Type.2='',HP=46,Attack=57,Defense=40,Sp.Atk=40,
                       Sp.Def=40,Speed=50,Generation='First',Legendary='False',
                       Total=46+57+40+40+40+50,Mega_Evolution='False')
    
    # solve data wrong or missing Name
    pokemon[c(35,63,738),'Name']=c('Nidoran(m)','Primeape','Flabebe')
    
    
    # pokemon Name df
    names_df<-pokemon%>%
        select(Id,Name)
    
    # train_battle preprocessing
    train_battle<-train_battle%>%
        mutate(Loser=ifelse(Winner==First_pokemon,Second_pokemon,First_pokemon),
               First_pokemon_name=sapply(First_pokemon,function(x){names_df[names_df$Id==x,'Name']}),
               Second_pokemon_name=sapply(Second_pokemon,function(x){names_df[names_df$Id==x,'Name']}),
               Winner_name=sapply(Winner,function(x){names_df[names_df$Id==x,'Name']}),
               Loser_name=sapply(Loser,function(x){names_df[names_df$Id==x,'Name']}))
    
    # battle win rate
    battle_winner<-train_battle%>%
        group_by(Winner)%>%
        count()%>%
        ungroup()%>%
        rename(Id='Winner',Win_Times='n')%>%
        mutate(Id=as.integer(Id))%>%
        arrange(Id)
    
    battle_loser<-train_battle%>%
        group_by(Loser)%>%
        count()%>%
        ungroup()%>%
        rename(Id='Loser',Lose_Times='n')%>%
        mutate(Id=as.integer(Id))%>%
        arrange(Id)
    
    Id_win_rate_df<-full_join(battle_winner,battle_loser,by='Id')%>%
        arrange(Id)%>%
        mutate(Win_Times=sapply(Win_Times,function(x){ifelse(is.na(x),0,x)}),
               Lose_Times=sapply(Lose_Times,function(x){ifelse(is.na(x),0,x)}),
               Total_Times=Win_Times+Lose_Times,
               Win_Rate=Win_Times/Total_Times)
    
    
    # pokemon win rate df
    pokemon_win_rate_df<-inner_join(pokemon,Id_win_rate_df,by='Id')%>%
        arrange(Id)
    
    # pokemon data for shiny display 
    pokemon_show_df<-pokemon%>%
        mutate(Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})),
               Mega_Evolution=as.character(sapply(Mega_Evolution,function(x){ifelse(x=='False',FALSE,TRUE)})))%>%
        select(Id,Name,Type.1,Type.2,HP,Attack,Defense,Sp.Atk,Sp.Def,Speed,Total,Generation,Legendary,Mega_Evolution)
    
    
    # pokemon statistics summary
    pokemon_summary<-do.call(cbind,lapply(pokemon_win_rate_df[,c(5,6,7,8,9,10,13,18)],summary))
    
    
    # Radar Plot df group by Type
    type_color<-c('#6F35FC','#B7B7CE','#A98FF3','#F95587','#B6A136','#EE8130',
                  '#F7D02C','#705746','#735797','#E2BF65','#96D9D6','#6390F0',
                  '#7AC74C','#C22E28','#D685AD','#A33EA1','#A8A77A','#A6B91A')
    
    Type.1<-c('Normal','Fire','Water','Electric','Grass','Ice','Fighting',
              'Poison','Ground','Flying','Psychic','Bug','Rock','Ghost',
              'Dragon','Dark','Steel','Fairy')
    
    type_col_df<-data.frame(Type.1,type_color)
    
    type_chars<-pokemon%>%
        select(Type.1,HP,Attack,Defense,Sp.Atk,Sp.Def,Speed)%>%
        group_by(Type.1)%>%
        summarise_all(list(mean=mean))%>%
        rename(HP=HP_mean,Attack=Attack_mean,Defense=Defense_mean,Sp.Atk=Sp.Atk_mean,Sp.Def=Sp.Def_mean,Speed=Speed_mean)%>%
        mutate(Sum=HP+Attack+Defense+Sp.Atk+Sp.Def+Speed,Color=type_color)
    
    # All column
    type_chars[19,]<-list(Type.1='All',HP=mean(pokemon$HP),Attack=mean(pokemon$Attack),Defense=mean(pokemon$Defense),
                          Sp.Atk=mean(pokemon$Sp.Atk),Sp.Def=mean(pokemon$Sp.Def),Speed=mean(pokemon$Speed),
                          Sum=mean(pokemon$HP+pokemon$Attack+pokemon$Defense+pokemon$Sp.Atk+pokemon$Sp.Def+pokemon$Speed),
                          Color='#006d2c')
    
    max<-ceiling(apply(type_chars[,2:7],2,function(x){max(x,na.rm=TRUE)})%>%sapply(as.double))%>%as.vector
    min<-rep.int(0,6)
    
    type_curCol<-reactive({
        as.vector((col2rgb(as.character(type_chars[type_chars$Type.1==input$Type1,'Color']))%>%as.integer())/255)
    })
    
    
    # Radar Plot df group by Generation
    gener_color<-c('#6F35FC','#B7B7CE','#A98FF3','#F95587','#B6A136','#EE8130')
    gener<-c('First','Second','Third','Fourth','Fifth','Sixth')
    
    gener_col_df<-data.frame(gener,gener_color)
    
    gener_chars<-pokemon%>%
        select(Generation,HP,Attack,Defense,Sp.Atk,Sp.Def,Speed)%>%
        group_by(Generation)%>%
        summarise_all(list(mean=mean))%>%
        rename(HP=HP_mean,Attack=Attack_mean,Defense=Defense_mean,Sp.Atk=Sp.Atk_mean,Sp.Def=Sp.Def_mean,Speed=Speed_mean)%>%
        mutate(Sum=HP+Attack+Defense+Sp.Atk+Sp.Def+Speed,Color=gener_color)
    
    # All column
    gener_chars[7,]<-list(Type.1='All',HP=mean(pokemon$HP),Attack=mean(pokemon$Attack),Defense=mean(pokemon$Defense),
                          Sp.Atk=mean(pokemon$Sp.Atk),Sp.Def=mean(pokemon$Sp.Def),Speed=mean(pokemon$Speed),
                          Sum=mean(pokemon$HP+pokemon$Attack+pokemon$Defense+pokemon$Sp.Atk+pokemon$Sp.Def+pokemon$Speed),
                          Color='#F7D02C')
    
    gener_curCol<-reactive({
        as.vector((col2rgb(as.character(gener_chars[gener_chars$Generation==input$Generation,'Color']))%>%as.integer())/255)
    })
    
    
    # Type Xiangke table
    normal<-c(1,1,1,1,1,1,2,1,1,1,1,1,1,0,1,1,1,1)
    fire<-c(1,0.5,2,1,0.5,0.5,1,1,2,1,1,0.5,2,1,1,1,0.5,0.5)
    water<-c(1,0.5,0.5,2,2,0.5,1,1,1,1,1,1,1,1,1,1,0.5,1)
    elec<-c(1,1,1,0.5,1,1,1,1,2,0.5,1,1,1,1,1,1,0.5,1)
    grass<-c(1,2,0.5,0.5,0.5,2,1,2,0.5,2,1,2,1,1,1,1,1,1)
    ice<-c(1,2,1,1,1,0.5,2,1,1,1,1,1,2,1,1,1,2,1)
    fighting<-c(1,1,1,1,1,1,1,1,1,2,2,0.5,0.5,1,1,0.5,1,2)
    poison<-c(1,1,1,1,0.5,1,0.5,0.5,2,1,2,0.5,1,1,1,1,1,0.5)
    ground<-c(1,1,2,0,2,2,1,0.5,1,1,1,1,0.5,1,1,1,1,1)
    flying<-c(1,1,1,2,0.5,2,0.5,1,0,1,1,0.5,2,1,1,1,1,1)
    psychic<-c(1,1,1,1,1,1,0.5,1,1,1,0.5,2,1,2,1,2,1,1)
    bug<-c(1,2,1,1,0.5,1,0.5,1,0.5,2,1,1,2,1,1,1,1,1)
    rock<-c(0.5,0.5,2,1,2,1,2,0.5,2,0.5,1,1,1,1,1,1,2,1)
    ghost<-c(0,1,1,1,1,1,0,0.5,1,1,1,0.5,1,2,1,2,1,1)
    dragon<-c(1,0.5,0.5,0.5,0.5,2,1,1,1,1,1,1,1,1,2,1,1,2)
    dark<-c(1,1,1,1,1,1,2,1,1,1,0,2,1,0.5,1,0.5,1,2)
    steel<-c(0.5,2,1,1,0.5,0.5,2,0,2,0.5,0.5,0.5,0.5,1,0.5,1,0.5,0.5)
    fairy<-c(1,1,1,1,1,1,0.5,2,1,1,1,0.5,1,1,0,0.5,2,1)
    
    xtable<-data.frame(Attacking=Type.1,Normal=normal,Fire=fire,Water=water,
                        Electric=elec,Grass=grass,Ice=ice,Fighting=fighting,
                        Poison=poison,Ground=ground,Flying=flying,Psychic=psychic,
                        Bug=bug,Rock=rock,Ghost=ghost,Dragon=dragon,Dark=dark,
                        Steel=steel,Fairy=fairy)
    
    
    # Correlation plot df group by Type
    type_corr<-reactive({
        if(input$Type1=='All'){
            return(pokemon%>%
                       select(HP,Attack,Defense,Sp.Atk,Sp.Def,Speed)%>%
                       cor())
        }else{
            return(pokemon%>%
                       filter(Type.1==input$Type1)%>%
                       select(HP,Attack,Defense,Sp.Atk,Sp.Def,Speed)%>%
                       cor())
        }})
    
    # Correlation Plot df group by Generation
    gener_corr<-reactive({
        if(input$Generation=='All'){
            return(pokemon%>%
                       select(HP,Attack,Defense,Sp.Atk,Sp.Def,Speed)%>%
                       cor())
        }else{
            return(pokemon%>%
                       filter(Generation==input$Generation)%>%
                       select(HP,Attack,Defense,Sp.Atk,Sp.Def,Speed)%>%
                       cor())
        }})
    
    
    # Scatter Plot df group by Type
    type_scatter<-reactive({
        if(input$Type1=='All'){
            return(pokemon)
        }else{
            return(pokemon%>%
                       filter(Type.1==input$Type1))
        }
    })
    
    # Scatter Plot df group by Generation
    gener_scatter<-reactive({
        if(input$Generation=='All'){
            return(pokemon)
        }else{
            return(pokemon%>%
                       filter(Generation==input$Generation))
        }
    })
    
    
    # Bar Plot df group by Generation
    total_chars<-reactive({
        if(input$Generation=='All'){
            return(pokemon%>%
                       group_by(Type.1)%>%
                       summarize(tot=n()))
        }else{
            return(pokemon%>%
                       filter(Generation==input$Generation)%>%
                       group_by(Type.1)%>%
                       summarize(tot=n()))
        }
    })
    
    # Legendary
    total_legend_chars<-reactive({
        if(input$Generation=='All'){
            return(pokemon%>%
                       group_by(Type.1,Legendary)%>%
                       summarize(count=n()))
        }else{
            return(pokemon%>%
                       filter(Generation==input$Generation)%>%
                       group_by(Type.1,Legendary)%>%
                       summarize(count=n()))
        }
    })
    
    # Mega Evolution
    total_mega_chars<-reactive({
        if(input$Generation=='All'){
            return(pokemon%>%
                       group_by(Type.1,Mega_Evolution)%>%
                       summarize(count=n()))
        }else{
            return(pokemon%>%
                       filter(Generation==input$Generation)%>%
                       group_by(Type.1,Mega_Evolution)%>%
                       summarize(count=n()))
        }
    })
    
    
    # Histogram Plot df
    plist_1<-list()
    plist_2<-list()
    plist_3<-list()
    value_index=c(5,6,7,8,9,10,13)
    
    # Histogram Plot df group by Type
    type_hist<-reactive({
        if(input$Type1=='All'){
            return(pokemon)
        }else{
            return(pokemon%>%
                       filter(Type.1==input$Type1))
        }
    })
    
    # Histogram Plot df group by Generation
    gener_hist<-reactive({
        if(input$Generation=='All'){
            return(pokemon)
        }else{
            return(pokemon%>%
                       filter(Generation==input$Generation))
        }
    })
    
    
    # max/min ability df
    max_pokemon_df<-reactive({
        if(input$Generation=='All'&input$Type1=='All'){
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,input$Ability)%>%
                       mutate(Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})))%>%
                       arrange(desc(!!sym(input$Ability))))}
        else if(input$Generation!='All'&input$Type1=='All'){
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,input$Ability)%>%
                       filter(Generation==input$Generation)%>%
                       mutate(Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})))%>%
                       arrange(desc(!!sym(input$Ability))))}
        else if(input$Generation=='All'&input$Type1!='All'){
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,input$Ability)%>%
                       mutate(Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})))%>%
                       filter(Type.1==input$Type1)%>%
                       arrange(desc(!!sym(input$Ability))))}
        else{
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,input$Ability)%>%
                       filter(Generation==input$Generation,
                              Type.1==input$Type1)%>%
                       mutate(Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})))%>%
                       arrange(desc(!!sym(input$Ability))))}
    })
    
    min_pokemon_df<-reactive({
        if(input$Generation=='All'&input$Type1=='All'){
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,input$Ability)%>%
                       mutate(Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})))%>%
                       arrange(!!sym(input$Ability)))}
        else if(input$Generation!='All'&input$Type1=='All'){
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,input$Ability)%>%
                       filter(Generation==input$Generation)%>%
                       mutate(Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})))%>%
                       arrange(!!sym(input$Ability)))}
        else if(input$Generation=='All'&input$Type1!='All'){
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,input$Ability)%>%
                       mutate(Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})))%>%
                       filter(Type.1==input$Type1)%>%
                       arrange(!!sym(input$Ability)))}
        else{
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,input$Ability)%>%
                       filter(Generation==input$Generation,
                              Type.1==input$Type1)%>%
                       mutate(Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})))%>%
                       arrange(!!sym(input$Ability)))}
    })
    
    
    # max/min win rate df
    max_pokemon_win_rate_df<-reactive({
        if(input$Generation=='All'&input$Type1=='All'){
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,HP,Attack,Defense,Sp.Atk,Sp.Def,Speed,Total,Win_Times,Total_Times,Win_Rate)%>%
                       arrange(desc(Win_Rate))%>%
                       mutate(Id=sprintf('%.0f',Id),
                              Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})),
                              HP=sprintf('%.0f',HP),
                              Attack=sprintf('%.0f',Attack),
                              Defense=sprintf('%.0f',Defense),
                              Sp.Atk=sprintf('%.0f',Sp.Atk),
                              Sp.Def=sprintf('%.0f',Sp.Def),
                              Speed=sprintf('%.0f',Speed),
                              Total=sprintf('%.0f',Total),
                              Win_Times=sprintf('%.0f',Win_Times),
                              Total_Times=sprintf('%.0f',Total_Times),
                              Win_Rate=sprintf('%.6f',Win_Rate)))}
        else if(input$Generation!='All'&input$Type1=='All'){
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,HP,Attack,Defense,Sp.Atk,Sp.Def,Speed,Total,Win_Times,Total_Times,Win_Rate)%>%
                       filter(Generation==input$Generation)%>%
                       arrange(desc(Win_Rate))%>%
                       mutate(Id=sprintf('%.0f',Id),
                              Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})),
                              HP=sprintf('%.0f',HP),
                              Attack=sprintf('%.0f',Attack),
                              Defense=sprintf('%.0f',Defense),
                              Sp.Atk=sprintf('%.0f',Sp.Atk),
                              Sp.Def=sprintf('%.0f',Sp.Def),
                              Speed=sprintf('%.0f',Speed),
                              Total=sprintf('%.0f',Total),
                              Win_Times=sprintf('%.0f',Win_Times),
                              Total_Times=sprintf('%.0f',Total_Times),
                              Win_Rate=sprintf('%.6f',Win_Rate)))}
        else if(input$Generation=='All'&input$Type1!='All'){
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,HP,Attack,Defense,Sp.Atk,Sp.Def,Speed,Total,Win_Times,Total_Times,Win_Rate)%>%
                       filter(Type.1==input$Type1)%>%
                       arrange(desc(Win_Rate))%>%
                       mutate(Id=sprintf('%.0f',Id),
                              Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})),
                              HP=sprintf('%.0f',HP),
                              Attack=sprintf('%.0f',Attack),
                              Defense=sprintf('%.0f',Defense),
                              Sp.Atk=sprintf('%.0f',Sp.Atk),
                              Sp.Def=sprintf('%.0f',Sp.Def),
                              Speed=sprintf('%.0f',Speed),
                              Total=sprintf('%.0f',Total),
                              Win_Times=sprintf('%.0f',Win_Times),
                              Total_Times=sprintf('%.0f',Total_Times),
                              Win_Rate=sprintf('%.6f',Win_Rate)))}
        else{
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,HP,Attack,Defense,Sp.Atk,Sp.Def,Speed,Total,Win_Times,Total_Times,Win_Rate)%>%
                       filter(Type.1==input$Type1,
                              Generation==input$Generation)%>%
                       arrange(desc(Win_Rate))%>%
                       mutate(Id=sprintf('%.0f',Id),
                              Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})),
                              HP=sprintf('%.0f',HP),
                              Attack=sprintf('%.0f',Attack),
                              Defense=sprintf('%.0f',Defense),
                              Sp.Atk=sprintf('%.0f',Sp.Atk),
                              Sp.Def=sprintf('%.0f',Sp.Def),
                              Speed=sprintf('%.0f',Speed),
                              Total=sprintf('%.0f',Total),
                              Win_Times=sprintf('%.0f',Win_Times),
                              Total_Times=sprintf('%.0f',Total_Times),
                              Win_Rate=sprintf('%.6f',Win_Rate)))}
    })
    
    
    min_pokemon_win_rate_df<-reactive({
        if(input$Generation=='All'&input$Type1=='All'){
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,HP,Attack,Defense,Sp.Atk,Sp.Def,Speed,Total,Win_Times,Total_Times,Win_Rate)%>%
                       arrange(Win_Rate)%>%
                       mutate(Id=sprintf('%.0f',Id),
                              Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})),
                              HP=sprintf('%.0f',HP),
                              Attack=sprintf('%.0f',Attack),
                              Defense=sprintf('%.0f',Defense),
                              Sp.Atk=sprintf('%.0f',Sp.Atk),
                              Sp.Def=sprintf('%.0f',Sp.Def),
                              Speed=sprintf('%.0f',Speed),
                              Total=sprintf('%.0f',Total),
                              Win_Times=sprintf('%.0f',Win_Times),
                              Total_Times=sprintf('%.0f',Total_Times),
                              Win_Rate=sprintf('%.6f',Win_Rate)))}
        else if(input$Generation!='All'&input$Type1=='All'){
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,HP,Attack,Defense,Sp.Atk,Sp.Def,Speed,Total,Win_Times,Total_Times,Win_Rate)%>%
                       filter(Generation==input$Generation)%>%
                       arrange(Win_Rate)%>%
                       mutate(Id=sprintf('%.0f',Id),
                              Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})),
                              HP=sprintf('%.0f',HP),
                              Attack=sprintf('%.0f',Attack),
                              Defense=sprintf('%.0f',Defense),
                              Sp.Atk=sprintf('%.0f',Sp.Atk),
                              Sp.Def=sprintf('%.0f',Sp.Def),
                              Speed=sprintf('%.0f',Speed),
                              Total=sprintf('%.0f',Total),
                              Win_Times=sprintf('%.0f',Win_Times),
                              Total_Times=sprintf('%.0f',Total_Times),
                              Win_Rate=sprintf('%.6f',Win_Rate)))}
        else if(input$Generation=='All'&input$Type1!='All'){
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,HP,Attack,Defense,Sp.Atk,Sp.Def,Speed,Total,Win_Times,Total_Times,Win_Rate)%>%
                       filter(Type.1==input$Type1)%>%
                       arrange(Win_Rate)%>%
                       mutate(Id=sprintf('%.0f',Id),
                              Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})),
                              HP=sprintf('%.0f',HP),
                              Attack=sprintf('%.0f',Attack),
                              Defense=sprintf('%.0f',Defense),
                              Sp.Atk=sprintf('%.0f',Sp.Atk),
                              Sp.Def=sprintf('%.0f',Sp.Def),
                              Speed=sprintf('%.0f',Speed),
                              Total=sprintf('%.0f',Total),
                              Win_Times=sprintf('%.0f',Win_Times),
                              Total_Times=sprintf('%.0f',Total_Times),
                              Win_Rate=sprintf('%.6f',Win_Rate)))}
        else{
            return(pokemon_win_rate_df%>%
                       select(Id,Name,Type.1,Generation,Legendary,HP,Attack,Defense,Sp.Atk,Sp.Def,Speed,Total,Win_Times,Total_Times,Win_Rate)%>%
                       filter(Type.1==input$Type1,
                              Generation==input$Generation)%>%
                       arrange(Win_Rate)%>%
                       mutate(Id=sprintf('%.0f',Id),
                              Legendary=as.character(sapply(Legendary,function(x){ifelse(x=='False',FALSE,TRUE)})),
                              HP=sprintf('%.0f',HP),
                              Attack=sprintf('%.0f',Attack),
                              Defense=sprintf('%.0f',Defense),
                              Sp.Atk=sprintf('%.0f',Sp.Atk),
                              Sp.Def=sprintf('%.0f',Sp.Def),
                              Speed=sprintf('%.0f',Speed),
                              Total=sprintf('%.0f',Total),
                              Win_Times=sprintf('%.0f',Win_Times),
                              Total_Times=sprintf('%.0f',Total_Times),
                              Win_Rate=sprintf('%.6f',Win_Rate)))}
    })
    
    
    # condition win rate df
    condition_win_rate_df<-reactive({
        pokemon_win_rate_df%>%
            group_by(!!sym(input$Classification))%>%
            summarise(Win_Times=sum(Win_Times),
                      Lose_Times=sum(Lose_Times),
                      Total_Times=sum(Total_Times),
                      Win_Rate=Win_Times/Total_Times)%>%
            arrange(desc(Win_Rate))
    })

    
    # Radar Plot & Correlation Plot group by Type
    output$type_radar_heat_plot<-renderPlot({
        par(mfrow=c(1,2))
        
        # Type.1 comprehensive ability radarchart
        radarchart(rbind(max,min,type_chars[type_chars$Type.1==input$Type1,2:7]),
                   axistype=2 , 
                   pcol=rgb(type_curCol()[1],type_curCol()[2],type_curCol()[3],alpha=1) ,
                   pfcol=rgb(type_curCol()[1],type_curCol()[2],type_curCol()[3],0.5) ,
                   plwd=2 ,cglcol='grey',cglty=1, 
                   axislabcol='black',caxislabels=seq(0,2000,5),cglwd=0.8,vlcex=0.8)
        mtext(paste(input$Type1,'Type\'s Mean Comprehensive Ability',sep=' '),at=0,line=0.5,cex=1.5)
        
        # comprehensive ability correlation heatmap
        corrplot(type_corr(),method='color',order='original',mar=c(1,1,1,1),addCoef.col='black',tl.col='black',tl.srt=90,
                 col=colorRampPalette(c('blue','white','red'))(200))
    })
    
    
    # Radar Plot & Correlation Plot group by Generation
    output$gener_radar_heat_plot<-renderPlot({
        par(mfrow=c(1,2))
        
        # Type.1 comprehensive ability radarchart
        radarchart(rbind(max,min,gener_chars[gener_chars$Generation==input$Generation,2:7]),
                   axistype=2 , 
                   pcol=rgb(gener_curCol()[1],gener_curCol()[2],gener_curCol()[3],alpha=1) ,
                   pfcol=rgb(gener_curCol()[1],gener_curCol()[2],gener_curCol()[3],0.5) ,
                   plwd=2 ,cglcol='grey',cglty=1, 
                   axislabcol='black',caxislabels=seq(0,2000,5),cglwd=0.8,vlcex=0.8)
        mtext(paste(input$Generation,'Generation\'s Mean Comprehensive Ability',sep=' '),at=0,line=0.5,cex=1.5)
        
        # comprehensive ability correlation heatmap
        corrplot(gener_corr(),method='color',order='original',mar=c(1,1,1,1),addCoef.col='black',tl.col='black',tl.srt=90,
                 col=colorRampPalette(c('blue','white','red'))(200))
    })
    
    
    # Scatter Plot
    output$abl_scatter_plot<-renderPlot({
        # comprehensive ability scatter plot
        type_scatter_plot<-ggscatterhist(data=type_scatter(),x=input$Ability_X,y=input$Ability_Y,
                                         color='#66B3FF',margin.plot='histogram',ggtheme=theme_grey(),
                                         margin.params=list(fill='pink',color='black'),
                                         title=paste(input$Type1,'Type\'s Ability Scatter Plot',sep=' '))
        
        gener_scatter_plot<-ggscatterhist(data=gener_scatter(),x=input$Ability_X,y=input$Ability_Y,
                                          color='#66B3FF',margin.plot='histogram',ggtheme=theme_grey(),
                                          margin.params=list(fill='pink',color='black'),
                                          title=paste(input$Generation,'Generation\'s Ability Scatter Plot',sep=' '))
        
        grid.arrange(type_scatter_plot,gener_scatter_plot,nrow=1)
    })
    
    
    # Bar Plot group by Generation
    output$gener_bar_plot<-renderPlot({
        legendary_bar<-merge(merge(total_chars(),total_legend_chars(),by='Type.1'),type_col_df,by='Type.1')%>%
            ggplot(aes(x=reorder(Type.1,tot),y=count))+ 
            geom_bar(aes(fill=type_color,alpha=Legendary),color='white',size=0.25,stat='identity') + 
            scale_fill_identity()+
            coord_flip()+
            labs(x='Type',y='Number')+
            scale_alpha_discrete(range=c(0.9,0.6))+
            labs(title=paste(input$Generation,'Generation\'s Type & Legendary Distribution',sep=' '))+
            theme(plot.title=element_text(hjust=0.5,size=15))
        
        mega_bar<-merge(merge(total_chars(),total_mega_chars(),by='Type.1'),type_col_df,by='Type.1')%>%
            ggplot(aes(x=reorder(Type.1,tot),y=count))+ 
            geom_bar(aes(fill=type_color,alpha=Mega_Evolution),color='white',size=0.25,stat='identity') + 
            scale_fill_identity()+
            coord_flip()+
            labs(x='Type',y='Number')+
            scale_alpha_discrete(range=c(0.9,0.6))+
            labs(title=paste(input$Generation,'Generation\'s Type & Mega Evolution Distribution',sep=' '))+
            theme(plot.title=element_text(hjust=0.5,size=15))
        
        grid.arrange(legendary_bar,mega_bar,nrow=1)
    })
    
    
    # Histogram Plot group by Type
    output$type_hist_plot<-renderPlot({
        for(i in 1:length(value_index)){
            plist_2[[i]]<-type_hist()%>%
                ggplot(aes_string(x=names(pokemon)[value_index[i]]))+
                geom_histogram(aes(y=..density..),binwidth=5,colour='black',fill='white')+
                geom_density(alpha=.2,colour='black')+
                labs(title=paste(paste(input$Type1,'Type\'s',sep=' '),paste(names(pokemon)[value_index[i]],'Distribution',sep=' '),sep=' '),y='')+
                theme(plot.title=element_text(hjust=0.5))
        }
        
        do.call(grid.arrange,c(plist_2,nrow=2))
    })
    
    # Histogram Plot group by Generation
    output$gener_hist_plot<-renderPlot({
        
        for(i in 1:length(value_index)){
            plist_1[[i]]<-gener_hist()%>%
                ggplot(aes_string(x=names(pokemon)[value_index[i]]))+
                geom_histogram(aes(y=..density..),binwidth=5,colour='black',fill='white')+
                geom_density(alpha=.2,colour='black')+
                labs(title=paste(paste(input$Generation,'Generation\'s',sep=' '),paste(names(pokemon)[value_index[i]],'Distribution',sep=' '),sep=' '),y='')+
                theme(plot.title=element_text(hjust=0.5))
        }
        
        do.call(grid.arrange,c(plist_1,nrow=2))
    })
    
    # Histogram Plot win diff
    output$win_diff_hist_plot<-renderPlot({
        for(i in 1:length(value_index)){
            plist_3[[i]]<-train_battle%>%
                mutate(Win_value=sapply(Winner,function(x){pokemon_win_rate_df[pokemon_win_rate_df$Id==x,value_index[i]]}),
                       Lose_value=sapply(Loser,function(x){pokemon_win_rate_df[pokemon_win_rate_df$Id==x,value_index[i]]}),
                       Win_Lose_diff=Win_value-Lose_value)%>%
                ggplot(aes(x=Win_Lose_diff))+
                geom_histogram(aes(y=..density..),binwidth=15,colour='black',fill='white')+
                geom_density(alpha=.2,colour='black')+
                labs(title=paste('Win and Lose',names(pokemon)[value_index[i]],'Diff Distribution',sep=' '),
                     x=paste(names(pokemon)[value_index[i]],'Diff',sep=' '),
                     y='')+
                theme(plot.title=element_text(hjust=0.5))
        }
        
        do.call(grid.arrange,c(plist_3,nrow=2))
        
    })
    
    
    # win rate ability Plot
    output$win_rate_ability_plot<-renderPlot({
        win_rate_scatter<-pokemon_win_rate_df%>%
            ggplot(aes_string(x=input$Ability,y='Win_Rate'))+
            geom_point()+
            labs(title=paste(input$Ability,'vs Win Rate Scatter Plot',sep=' '))+
            theme(plot.title=element_text(hjust=0.5))+
            geom_smooth()
        
        con_win_rate_bar<-condition_win_rate_df()%>%
            ggplot(aes(x=reorder(!!sym(input$Classification),desc(Win_Rate)),y=Win_Rate,fill=as.factor(Win_Rate)))+ 
            geom_bar(color='white',stat='identity',show.legend=FALSE)+
            geom_text(size=3,aes(label=sprintf('%.3f',Win_Rate)),position=position_dodge(width=0.8),vjust=-0.25)+
            labs(title=paste(input$Classification,'vs Win Rate Bar Plot',sep=' '),x=input$Classification)+
            theme(plot.title=element_text(hjust=0.5),axis.text.x=element_text(angle=20))+
            scale_fill_hue()
        
        grid.arrange(win_rate_scatter,con_win_rate_bar,nrow=1)
    })
    
    
    # output data table
    output$pokemon_table<-renderTable({
        pokemon_show_df[1:8,]},
        rownames=TRUE,digits=0)
    output$train_battle_table<-renderTable({
        train_battle[1:8,]},
        rownames=TRUE)
    output$type_xiangke_table<-renderTable({
        xtable},
        rownames=TRUE,digits=1)
    output$max_pokemon_table<-renderTable({
        head(max_pokemon_df(),n=10)},
        rownames=TRUE,digits=0)
    output$min_pokemon_table<-renderTable({
        head(min_pokemon_df(),n=10)},
        rownames=TRUE,digits=0)
    output$max_win_rate_df<-renderTable({
        head(max_pokemon_win_rate_df(),n=10)},
        rownames=TRUE)
    output$min_win_rate_df<-renderTable({
        head(min_pokemon_win_rate_df(),n=10)},
        rownames=TRUE)
    output$summary<-renderTable({
        pokemon_summary},
        rownames=TRUE,digits=6)
    output$logistic_table<-renderTable({
        logistic_df},
        rownames=TRUE)
    output$randomforest_table<-renderTable({
        randomforest_df},
        rownames=TRUE)
    output$xgboost_table<-renderTable({
        xgboost_df},
        rownames=TRUE)
})

# Run the application 
shinyApp(ui=ui,server=server)
