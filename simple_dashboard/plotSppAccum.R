plotSppAccum <- function(data_array, 
                         sample_array, 
                         columnValue = NULL, 
                         predSamp = NULL,
                         ...){
  # if no columnValue selected then plot the first dimname
  if(is.null(columnValue)){
    columnValue <- dimnames(data_array)[[3]][1]
  }
  
  # calculate median and quantiles for each sample
  spp_vals <- data_array[,,columnValue]
  samp_vals <- sample_array[,,columnValue]
  
  spp_summary <- apply(spp_vals, 2, quantile, probs = c(0.5,0.025,0.975))
  samp_summary <- apply(samp_vals, 2, mean, na.rm = TRUE)
  
  # create plot 
  par(bty = "l")
  plot(spp_vals ~ jitter(samp_vals,amount = 1), 
       pch = 20, 
       col = "gray88",
       las = 1,
       main = columnValue,
       xlab = "Number of sampling events",
       ylab = "Species")
  
  # Prepare a good inital state
  theta.0 <- max(spp_vals) * 1.1
  model.0 <- lm(log(- spp + theta.0) ~ samples, 
                data = data.frame(spp = apply(spp_vals,2,median),
                                  samples = apply(samp_vals,2,mean)))
  alpha.0 <- -exp(coef(model.0)[1])
  beta.0 <- coef(model.0)[2]
  
  start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
  
  # Fit the model
#  for(i in 1:nrow(spp_vals)){
#    model <- nls(spp ~ alpha * exp(beta * samples) + theta , start = start,
#                 data = data.frame(spp = spp_vals[i,],
#                                   samples = apply(samp_vals,2,mean)),
#                 control = list(maxiter = 500))
#    
#    pred_line <- predict(model, 
#                         list(samples = seq(0,max(samp_vals),1)))
#    
#    points(pred_line ~ seq(0,max(samp_vals),1), type = "l", lwd = 1,
#           col = rgb(100,100,100,110,max = 255))
#  }
  
  model <- nls(spp ~ alpha * exp(beta * samples) + theta , start = start,
               data = data.frame(spp = apply(spp_vals,2,median),
                                 samples = apply(samp_vals,2,mean)),
               control = list(maxiter = 500))
  
  pred_line <- predict(model, 
                       list(samples = seq(0,max(samp_vals),1)))
  
  points(pred_line ~ seq(0,max(samp_vals),1), type = "l", lwd = 2)
  
  if(!is.null(predSamp)){
    if(!is.numeric(predSamp)){stop("predSamp is not numeric")}
    model <- nls(spp ~ alpha * exp(beta * samples) + theta , start = start,
                 data = data.frame(spp = apply(spp_vals,2,median),
                                   samples = apply(samp_vals,2,mean)),
                 control = list(maxiter = 500))
    
    pred_lineNew <- predict(model, 
                         list(samples = seq(0,predSamp,1)))
    
    plot(spp_vals ~ jitter(samp_vals,amount = 1), 
         pch = 20,
         xlim = c(1,predSamp),
         ylim = c(0,max(pred_lineNew, na.rm = TRUE)),
         col = "gray88",
         las = 1,
         main = paste0(columnValue," Predictions"),
         xlab = "Number of sampling events",
         ylab = "Species")
    
    points(pred_line ~ seq(0,max(samp_vals),1), type = "l", lwd = 2)
    points(pred_lineNew ~ seq(0,predSamp,1), type = "l", lwd = 2, lty = 2)
    
  }
  
}