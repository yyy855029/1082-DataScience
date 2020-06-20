# import library
library(shiny)
library(ggbiplot)
library(FactoMineR)
library(factoextra)

shinyUI(pageWithSidebar(
    
    headerPanel('PCA & CA for Iris Data'),
    
    sidebarPanel(
        
        sliderInput(inputId='obs',
                    label='Number of observations to view:',
                    min=1,
                    max=nrow(iris),
                    value=15,
                    step=1),
        selectInput(inputId='PCA_X',
                    label='PCA plot X variable:',
                    choices=c('PC1','PC2','PC3','PC4')),
        selectInput(inputId='PCA_Y',
                    label='PCA plot Y variable:',
                    choices=c('PC1','PC2','PC3','PC4')),
        selectInput(inputId='CA_X',
                    label='CA plot X variable:',
                    choices=c('Dim1','Dim2','Dim3')),
        selectInput(inputId='CA_Y',
                    label='CA plot Y variable:',
                    choices=c('Dim1','Dim2','Dim3')),
        selectInput(inputId='CA_label',
                    label='CA plot label:',
                    choices=c('all','row','col'))
    ),
    
    mainPanel(
        tabsetPanel(type='tabs',
                    tabPanel('Plot',
                             h3('Log Iris Data PCA Plot'),
                             plotOutput('pca_plot',width='35%'),
                             br(),
                             h3('Iris Data CA Biplot'),
                             plotOutput('ca_plot',width='40%')),
                    tabPanel('Summary',
                             h3('Iris Data Feature Statistics Summary'),
                             br(),
                             tableOutput('iris_summary_table'),
                             br(),
                             h3('Log Iris Data PCA Result'),
                             br(),
                             tableOutput('pve_table'),
                             br(),
                             h3('Iris Data CA Result'),
                             tableOutput('eig_table')),
                    tabPanel('Data',
                             h3('Raw Iris Data'),
                             br(),
                             tableOutput('iris_table'))
        )
    )
))

