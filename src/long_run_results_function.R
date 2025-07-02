long_run_results_function <- function(model,data,sample){
  print(summary(model))
  
  ecm_term <- residuals(model)
  
  
  fig <- plot_ly(y=ecm_term%>%as.numeric(),x=index(data), type = 'scatter', mode = 'lines')%>%
    layout(title=paste("Long-run disequilibrium",sample ))
  print(fig)
  
  
  print(adf.test(ecm_term))
  print(pp.test(ecm_term))
  print(kpss.test(ecm_term))
  
  return(ecm_term)
  
}