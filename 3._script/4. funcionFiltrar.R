


filtrar <-  function(x =dta,  pais = "TOTAL", ecos = "TOTAL"){ 
  
  require(dplyr)
  require(tidyr)
  
  #Generamos los filtros
  
  if(ecos=="TOTAL" & pais!="TOTAL"){
    dfTot <- dta %>% 
      subset(Pais==pais) %>% 
      group_by(Clase) %>% 
      summarise(n =n() )
    
  }
  
  if(ecos!="TOTAL" & pais=="TOTAL"){
    dfTot <- dta %>% 
      subset(Bioma==ecos) %>% 
      group_by(Clase) %>% 
      summarise(n =n() )
  }
  
  if(ecos!="TOTAL" & pais!="TOTAL"){
    dfTot <- dta %>% 
      subset(Bioma==ecos & Pais==pais) %>% 
      group_by(Clase) %>% 
      summarise(n =n() )
  }
  
  if(ecos=="TOTAL" & pais=="TOTAL"){
    dfTot <- dta  %>%
      distinct(Codigo, Clase)%>% 
      group_by(Clase) %>% 
      summarise(n =n())
  }
  
  #Procesamos los datos y los incorporamos a las cajas
  
  dfTot1 <- as.vector(t(dfTot[,-1]))
  names(dfTot1) <- (dfTot[["Clase"]])
  
  return(dfTot1)
}


filt2 <- function(x =dta,  pais = "TOTAL", ecos = "TOTAL", type = "TOTAL"){
  
  
  if(ecos=="TOTAL" & pais!="TOTAL" & type=="TOTAL"){
    dta1.1 <- dta %>% subset(Pais==pais)
  }
  
  if(ecos=="TOTAL" & pais=="TOTAL"& type!="TOTAL"){
    dta1.1 <- dta%>% subset(Clase==type)
  }
  
  if(ecos!="TOTAL" & pais=="TOTAL" & type=="TOTAL"){
    dta1.1 <- dta %>% subset(Bioma==ecos)
  }
  
  if(ecos!="TOTAL" & pais!="TOTAL"& type=="TOTAL"){
    dta1.1 <- dta%>% subset(Bioma==ecos & Pais==pais)
  }
  
  if(ecos!="TOTAL" & pais=="TOTAL" & type!="TOTAL"){
    dta1.1 <- dta %>% subset(Bioma==ecos &Clase==type)
  }
  
  if(ecos=="TOTAL" & pais!="TOTAL" & type!="TOTAL"){
    dta1.1 <- dta %>% subset(Clase==type & Pais==pais)
  }
  
  if(ecos!="TOTAL" & pais!="TOTAL" & type!="TOTAL"){
    dta1.1 <- dta %>% subset(Bioma==ecos &Clase==type & Pais==pais)
  }
  
  if(ecos=="TOTAL" & pais=="TOTAL" & type=="TOTAL"){
    dta1.1 <- dta
  }
  
  
  return(dta1.1)
}
