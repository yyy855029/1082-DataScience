# import library
library(shiny)
library(ggbiplot)
library(FactoMineR)
library(factoextra)


shinyServer(function(input,output,session){
    # process iris data 
    data(iris)
    ir.summary<-do.call(cbind,lapply(iris[,1:4],summary))
    log.ir<-log(iris[,1:4])
    ir.species<-iris[,5]
    
    # process pca data
    ir.pca<-prcomp(log.ir,center=TRUE,scale.=TRUE)
    pca.std<-ir.pca$sdev
    pca.var<-pca.std^2
    pca.pve<-pca.var/sum(pca.var)
    pca.cpve<-cumsum(pca.pve)
    # PCA PVE df
    pve<-data.frame(Std=pca.std,
                    Var=pca.var,
                    PVE=pca.pve,
                    CPVE=pca.cpve,
                    row.names=c('PC1','PC2','PC3','PC4'))
    
    # process CA data
    ir.ca<-CA(iris[,1:4],graph=FALSE)
    # CA PVE df
    eig<-as.data.frame(ir.ca$eig)
    colnames(eig)<-c('Eigenvalue','PVE','CPVE')
    
    # reactive df
    ir.summary.df<-reactive({ir.summary})
    ir.df<-reactive({iris[1:input$obs,]})
    pve.df<-reactive({pve})
    eig.df<-reactive({eig})
    
    # output plot PCA 
    output$pca_plot<-renderPlot({
        ggbiplot(ir.pca,obs.scale=1,var.scale=1,groups=ir.species,choices=c(as.numeric(substr(input$PCA_X,3,3)),as.numeric(substr(input$PCA_Y,3,3))))+
            scale_color_discrete(name='')+
            theme(legend.direction='horizontal',legend.position='top')+
            xlim(-3.5,3.5)+
            ylim(-3,3.5)
    })
    
    # outplot plot CA
    output$ca_plot<-renderPlot({
        fviz_ca_biplot(ir.ca,axes=c(as.numeric(substr(input$CA_X,4,4)),as.numeric(substr(input$CA_Y,4,4))),geom='text',label=input$CA_label)+
            labs(title='')
    })
    
    # output data table
    output$iris_summary_table<-renderTable({
        ir.summary.df()},
        rownames=TRUE)
    
    output$iris_table<-renderTable({
        head(ir.df(),n=input$obs)}
        ,rownames=TRUE)
    
    output$pve_table<-renderTable({
        pve.df()},
        rownames=TRUE)
    
    output$eig_table<-renderTable({
        eig.df()},
        rownames=TRUE)
})

