#graficos
library(plotly)
library(reshape2)


dta$Clase <- factor(dta$Clase, levels = c( "AVES","AMPHIBIA", "MAMMALIA", "PECES", "REPTILIA"))
df <- dcast(dta, year~Clase)

dfT <- dcast(dta, Tematica~Clase)

dfTot <- colSums(dfT[,-1])
  
fig2 <- df
fig2 <- fig2 %>% plot_ly(
  type = 'bar', 
  x = ~x, 
  y = ~y2, 
  color =  ~x, 
  legendgroup = ~x, 
  showlegend = F
)
fig2 <- fig2 %>% layout(
  xaxis = list(
    showgrid = F
  ),
  yaxis = list(
    showgrid = F
  )
)






fig1 <- plot_ly(df, x = ~year, y = ~AVES, type = 'bar', name = colnames(df[2]))
fig1 <- fig1 %>% add_trace(y = ~AMPHIBIA, name = colnames(df[3]))
fig1 <- fig1 %>% add_trace(y = ~MAMMALIA, name = colnames(df[4]))
fig1 <- fig1 %>% add_trace(y = ~PECES, name = colnames(df[5]))
fig1 <- fig1 %>% add_trace(y = ~REPTILIA, name = colnames(df[6]))

fig1 <- fig1 %>% layout(yaxis = list(title = 'Número'), 
                      xaxis = list(title = "Año de Publicación"))

fig2 <- plot_ly(dfT, x = ~Tematica, y = ~AVES, type = 'bar', 
                name = colnames(dfT[2]))
fig2 <- fig2 %>% add_trace(y = ~AMPHIBIA, name = colnames(dfT[3]))
fig2 <- fig2 %>% add_trace(y = ~MAMMALIA, name = colnames(dfT[4]))
fig2 <- fig2 %>% add_trace(y = ~PECES, name = colnames(dfT[5]))
fig2 <- fig2 %>% add_trace(y = ~REPTILIA, name = colnames(dfT[6]))
fig2 <- fig2 %>% layout(yaxis = list(title = 'Número'), 
                      xaxis = list(title = ""))

fig <- subplot(fig1, fig2, nrows = 1, shareX = F)
fig
