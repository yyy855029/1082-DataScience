# import library
if (!require('optparse')) { install.packages('optparse') }

library(optparse)


# read Rscript elements
option_list<-list(
  make_option(c('-i','--input'),type='character',default=FALSE,
              action='store',help='input file'
  ),
  make_option(c('-o','--output'),type='character',default=FALSE,
              action='store',help='output file'
  )
)

opt=parse_args(OptionParser(option_list=option_list))

input_name<-opt$input
output_name<-opt$output
set_name<-strsplit(tail(strsplit(input_name,'/')[[1]],n=1),'.csv')[[1]][1]
# print(input_name)
# print(output_name)
# print(set_name)


# read data
input<-read.table(input_name,sep=',', header=T)
weight<-round(max(input$weight),2)
height<-round(max(input$height),2)


# save output
output<-data.frame(set=set_name,weight=weight,height=height)
write.csv(output,output_name,row.names=FALSE,quote=FALSE)


