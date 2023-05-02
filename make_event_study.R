make_event_study = function(model, xlab, ylab, title){
  
  packs <- c('ggplot2', 'ggthemes')
  lapply(packs, require, character.only = T)
  
  mod_twfe <- model
  
  xlab = xlab
  ylab = ylab
  title = title 
  
  coef_table <- cbind.data.frame(
    est = coef_store <- mod_twfe$coeftable[,1],
    U = mod_twfe$coeftable[,1] + mod_twfe$coeftable[,2]*1.96,
    L = mod_twfe$coeftable[,1] - mod_twfe$coeftable[,2]*1.96,
    pval = mod_twfe$coeftable[,4]
  )
  
  m <- summary(mod_twfe, agg = 'ATT')
  
  coef_table$time_to_treat <- as.numeric(cbind(c(gsub('year::', "", row.names(coef_table)))))
  coef_table <- rbind.data.frame(coef_table, "-1" = c(0,0,0, .99, -1))
  coef_table$name2 <- round(coef_table$est, 3)
  coef_table$name2 <- ifelse(coef_table$name2 == 0, 'Reference', coef_table$name2)
  
  event_study_plot <- ggplot(coef_table, aes(time_to_treat, est)) + 
    geom_point() + 
    geom_line() + 
    geom_ribbon(aes(ymin=L, ymax=U), alpha=0.2, linetype = 'dashed') + 
    geom_vline(xintercept = -1, lty = 2, col = 'red') + 
    geom_hline(yintercept = 0, col = 'black', lty = 1) + 
    xlab(xlab) + 
    ylab(ylab) + 
    ggtitle(title) + 
    theme(text = element_text(size=12)) + 
    geom_label(data = coef_table, aes(x = time_to_treat, y = est, label = name2), family = 'Times') + 
   geom_label(data = coef_table, aes(x = max(time_to_treat)-1, y = max(U)-2, 
                                      label = paste(paste("ATT:",  round(m$coeftable[1], 2), sep = " "), 
                                                    paste("\nSE:", round(m$coeftable[2],2)))), family = 'Times') 
  
  return(event_study_plot)
  
}
