## plotly requires ggplot
library(ggplot2)
library(plotly)
library(stats)
data(iris)

##----Using_TSNE-----------------------------------------------------------------

X <- subset(iris, select = -c(Species)) 

axis = list(showline=FALSE, 
            zeroline=FALSE, 
            gridcolor='#ffff', 
            ticklen=4)

fig <- iris %>%  
  plot_ly()  %>%  
  add_trace(  
    type = 'splom',  
    dimensions = list( 
      list(label = 'sepal_width',values=~Sepal.Width),  
      list(label = 'sepal_length',values=~Sepal.Length),  
      list(label ='petal_width',values=~Petal.Width),  
      list(label = 'petal_length',values=~Petal.Length)),  
    color = ~Species, colors = c('#636EFA','#EF553B','#00CC96'))

fig <- fig %>% 
  layout( 
    legend=list(title=list(text='species')), 
    hovermode='closest', 
    dragmode= 'select', 
    plot_bgcolor='rgba(240,240,240,0.95)', 
    xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4), 
    yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4), 
    xaxis2=axis, 
    xaxis3=axis, 
    xaxis4=axis, 
    yaxis2=axis, 
    yaxis3=axis, 
    yaxis4=axis 
  ) 
fig

library(tsne)

features <- subset(iris, select = -c(Species))
    
set.seed(0)
tsne <- tsne(features, initial_dims = 2)
tsne <- data.frame(tsne)
pdb <- cbind(tsne, iris$Species)
options(warn = 1)

fig <- plot_ly(data = pdb, x = ~X1, y = ~X2, type = 'pointcloud', split = ~iris$Spcies)

fig <- fig %>%
  layout(plot_bgcolor  ='#e5ecf6')

fig
## did not colour by species (I think it should have)

##----Using_UMAP-----------------------------------------------------------------

library(umap)

iris.data = iris[, grep("Sepal|Petal", colnames(iris))]
iris.labels = iris[, "Species"] 
iris.umap = umap(iris.data, n_components = 2, random_state = 15) 
layout <- iris.umap[["layout"]] 
layout <- data.frame(layout) 
final <- cbind(layout, iris$Species) 

fig <- plot_ly(final, x = ~X1, y = ~X2, color = ~iris$Species, colors = c('#636EFA','#EF553B','#00CC96'), type = 'scatter', mode = 'markers')%>%  
  layout(
    plot_bgcolor = "#e5ecf6",
    legend=list(title=list(text='species')), 
    xaxis = list( 
      title = "0"),  
    yaxis = list( 
      title = "1")) 

fig


iris.umap = umap(iris.data, n_components = 3, random_state = 15) 
layout <- iris.umap[["layout"]] 
layout <- data.frame(layout) 
final <- cbind(layout, iris$Species) 

fig2 <- plot_ly(final, x = ~X1, y = ~X2, z = ~X3, color = ~iris$Species, colors = c('hotpink','pink','lavender')) 
fig2 <- fig2 %>% add_markers() 
fig2 <- fig2 %>% layout(scene = list(xaxis = list(title = '0'), 
                                     yaxis = list(title = '1'), 
                                     zaxis = list(title = '2'))) 

fig2 
