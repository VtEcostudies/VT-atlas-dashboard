# THIS SCRIPT CALCULATES THE NUMBER OF OBSERVERED AND UNOBSERVED SPECIES 
# USING A SPECIES ACCUMULATION CURVE 



predictNumSpp <- function(data_array, 
                          sample_array, 
                          columnValue = NULL, 
                          predSamp = 100000,
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

# Prepare a good inital state
theta.0 <- max(spp_vals) * 1.1
model.0 <- lm(log(- spp + theta.0) ~ samples, 
              data = data.frame(spp = apply(spp_vals,2,median),
                                samples = apply(samp_vals,2,mean)))
alpha.0 <- -exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)


model <- nls(spp ~ alpha * exp(beta * samples) + theta , start = start,
             data = data.frame(spp = apply(spp_vals,2,median),
                               samples = apply(samp_vals,2,mean)),
             control = list(maxiter = 500))

pred_line <- predict(model, 
                     list(samples = seq(0,max(samp_vals),1)))

obsSpp <- max(pred_line, na.rm = TRUE)

model <- nls(spp ~ alpha * exp(beta * samples) + theta , start = start,
               data = data.frame(spp = apply(spp_vals,2,median),
                                 samples = apply(samp_vals,2,mean)),
               control = list(maxiter = 500))
  
pred_lineNew <- predict(model, 
                          list(samples = seq(0,predSamp,1)))
  
predSpp <- max(pred_lineNew, na.rm = TRUE)

SppDiff <- predSpp - obsSpp 

return(data.frame(obs = obsSpp,
                  predicted = predSpp,
                  difference = SppDiff,
                  perObs = obsSpp/predSpp,
                  perPred = SppDiff/predSpp))
  
}