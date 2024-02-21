## Creating IPM functions
## original code from Emily Schultz DRM: 
## updated and reformatted by Cecilia Martinez
## April 20 2023
## cecimartinez333@gmail.com

### Create vital rate and IPM functions 

# Survival ---------------------------------------
#s.x <- function(size.x, Tann, PPTann, interval = 1) {
s.x <- function(model, size.x, data, interval = 1, elast = F, perturb = 0, t.clamp = F, plot=14546600020004) {
  sdata <- data.frame(PREVDIA = size.x, 
                      BALIVE = data$BALIVE)  
  # rescaled data
  scaled.sdata = data.frame(scale(sdata, 
                                  scale = surv.scaling$scale[match(names(sdata), names(surv.scaling$scale))], # match assures that variables are matched up
                                  center = surv.scaling$center[match(names(sdata), names(surv.scaling$center))])) # regardless of the order they are entered
  
  # add the census interval offset to the scaled data
  scaled.sdata = cbind(scaled.sdata,
                       CENSUS_INTERVAL = interval) 
  #scaled.sdata$CENSUS_INTERVAL <- interval # would this be equivalent? consistent with fec below
  scaled.sdata$PLT_CN_factor = as.factor(plot)
  
  # apply the fitted model to the scaled input data
  spred <- 1-predict(model, newdata = scaled.sdata, type = "response", re.form = NA, exclude = "s(PLT_CN_factor)") # no plot rnd effects used by predict here
  
  if(elast==F){spred<-spred+perturb}else{spred<-spred+perturb*spred}
  
  return(spred)
}


# Growth ------------------------------

# g.yx is used for building the IPM
# it returns the probability density of size at time t+1 as a function size at time t and covariate data
g.yx <- function(model, growSD, size.x, size.y, h, data, interval = 1, elast = F, perturb = 0, t.clamp = F, ba.clamp = T, plot=14546600020004) { 
  # raw (unscaled) data
  gdata <- data.frame(PREVDIA = size.x,
                      #BALIVE = ifelse(ba.clamp == T & data$BALIVE > 190, 190, data$BALIVE)
                      BALIVE = data$BALIVE
  ) 
  scaled.gdata = data.frame(scale(gdata, 
                                  scale = gr.scaling$scale[match(names(gdata), names(gr.scaling$scale))], 
                                  center = gr.scaling$center[match(names(gdata), names(gr.scaling$center))]))  
  scaled.gdata$PLT_CN_factor = as.factor(plot)
  
  # apply the fitted model to the scaled input data
  gpred <- (predict(model, newdata = scaled.gdata, re.form = NA, exclude = "s(PLT_CN_factor)") * interval) # plot rnd effects not included in prediction
  if(elast==F){gpred<-gpred+perturb}else{gpred<-gpred+perturb*gpred}
  
  return(pnorm((size.y+h) - size.x, gpred, growSD)-pnorm(size.y - size.x, gpred, growSD)) # returns probability density for quantiles ranging from size.y[1]-size.x to size.y[n]-size.x
  # with mean = gpred and sd = growSD
  # growSD is the SD of the residuals from the growth model
  # i.e., probability of transitioning to a new size at time t+1
  
}

# g.mean is used for making a map (raster) of predicted growth
g.mean <- function(model, size.x, data, interval = 1, elast = F, perturb = 0, t.clamp = F, ba.clamp = F, plot=14546600020004) {
  # raw (unscaled) data
  gdata <- data.frame(PREVDIA = size.x,
                      #BALIVE = ifelse(ba.clamp == T & data$BALIVE > 190, 190, data$BALIVE).
                      BALIVE = data$BALIVE
  )
  # rescaled data
  scaled.gdata = data.frame(scale(gdata, 
                                  scale = gr.scaling$scale[match(names(gdata), names(gr.scaling$scale))], 
                                  center = gr.scaling$center[match(names(gdata), names(gr.scaling$center))]))  
  scaled.gdata$PLT_CN_factor = as.factor(plot)
  
  # apply the fitted model to the scaled input data
  gpred <- predict(model, newdata = scaled.gdata, re.form = NA, exclude = "s(PLT_CN_factor)") * interval
  if(elast==F){gpred<-gpred+perturb}else{gpred<-gpred+perturb*gpred}
  
  return(gpred)
}

# Fecundity -----------------------------------------------
# function fec is used for building the IPM
fec <- function(model, size.y, size.x, data, interval = 1, elast = F, perturb = 0, ba.clamp = F, ba.clamp2 = F) {
  rdata <- data.frame(BALIVE = #ifelse(ba.clamp2 == T & data$BALIVE > 204, 204, 
                        #ifelse(ba.clamp == T & data$BALIVE < 93, 93 , 
                        data$BALIVE)

  
  scaled.rdata = data.frame(scale(rdata, 
                                  scale = r.scaling$scale[match(names(rdata), names(r.scaling$scale))], 
                                  center = r.scaling$center[match(names(rdata), names(r.scaling$center))]))  
  scaled.rdata$CENSUS_INTERVAL <- interval
  scaled.rdata$WBPadults1 <- 1 # remove the size threshold
  scaled.rdata$off = log(scaled.rdata$CENSUS_INTERVAL) + log(scaled.rdata$WBPadults1)

  rpred <- (as.vector(predict(model, newdata = scaled.rdata, type = "response")))
  if(elast==F){rpred<-rpred+perturb}else{rpred<-rpred+perturb*rpred}
  
  return(dnorm(log(size.y), r.sizemean, r.sizesd) * rpred) # returns the probability density for quantiles ranging from size.y[1] to size.y[n]
}

# function f.mean is used for making a map (raster) of predicted fecundity (recruitment)
f.mean <- function(model, data, interval = 1, elast = F, perturb = 0, ba.clamp = F, ba.clamp2 = F) { 
  # raw (unscaled) data
  rdata <- data.frame(BALIVE = data$BALIVE)
  #BALIVE = data$BALIVE,
  #T_yr_norm = data$T_yr,
  #PPT_yr_norm = data$PPT_yr)
  #T_wd_norm = data$T_wd_norm,
  #T_c_norm = data$T_c_norm, T_m_norm = data$T_m_norm)
  # rescaled data
  scaled.rdata = data.frame(scale(rdata, 
                                  scale = r.scaling$scale[match(names(rdata), names(r.scaling$scale))], 
                                  center = r.scaling$center[match(names(rdata), names(r.scaling$center))]))  
  # add census interval offset to the scaled data
  scaled.rdata$CENSUS_INTERVAL <- interval
  
  # size threshold above which trees are treated as reproductively mature
  # threshold defined here is 1 inch DRC, which is not correct biologically
  # but <is> consistent with how the ZIP model of recruitment was built
  #scaled.rdata$PIEDadults1 <- ifelse(size.x >= 1, 1, 0)
  scaled.rdata$WBPadults1 <- 1 # removed the size threshold
  scaled.rdata$off = log(scaled.rdata$CENSUS_INTERVAL) + log(scaled.rdata$WBPadults1)
  
  rpred <- predict(model, newdata = scaled.rdata, type = "response")
  if(elast==F){rpred<-rpred+perturb}else{rpred<-rpred+perturb*rpred}
  
  return(rpred)
}


ipm_fun<-function(min, max, n=500, gmodel, smodel, rmodel, gSD,
                  data, elast=F, gperturb=0, sperturb=0, rperturb=0,
                  s.t.clamp=F, g.t.clamp=F, g.ba.clamp=F,r.ba.clamp=F){
  b <- min+c(0:n)*(max-min)/n # these are the n+1 "edges" of the size bins
  y <- 0.5*(b[1:n]+b[2:(n+1)]) # these are the mid-points of the n size classes
  # y <- b[1:n]+0.5*((max.size-min.size)/n)
  h <- y[2]-y[1] # bin width 
  
  # Growth and survival
  G <- outer(y, b[1:500], g.yx, model = gmodel, growSD = gSD, h = h, data = data, elast = elast, perturb = gperturb, t.clamp = g.t.clamp, ba.clamp = g.ba.clamp)
  # G is an n*n matrix, currently 500*500 = 250,000
  # each column is one of n sizes at time t (the mid-points), each row is the transition probability to a new size at time t+dt
  #plot(y, G[,10], type = "l", ylab = "density", xlim = c(0,55)) # try with larger and larger values for the G column, the PD marches down/across
  # S <- s.x(y, PPTann = ppt_yr_val, Tann = t_yr_val, interval = 1)
  #image(y,y,t(G),main='growth kernel')  = from cory merow  
  S <- s.x(model = smodel, y, data = data, interval = 1, elast = elast, perturb = sperturb, t.clamp = s.t.clamp)
  
  P <- G
  for (k in 1:n) P[,k] <- G[,k]*S #survival*growth subkernel
  
  # Recruitment
  R <- h*outer(y, y, fec, model = rmodel, data = data, elast = elast, perturb = rperturb, ba.clamp = r.ba.clamp)
  # Entire kernel
  K <- P + R
  return(K)
  
  #image(y,y,t(K),main='full kernel')          # plot it
  #image(y,y,t(K)^.3,main='full kernel^0.3')           # plot it
  
}

ipm_ltre<-function(min, max, n=500, gmodel, smodel, rmodel, gSD,
                   data_g, data_s, data_r, elast=F, gperturb=0, sperturb=0, rperturb=0,
                   s.t.clamp=F, g.t.clamp=F, g.ba.clamp=F,r.ba.clamp=F){
  b <- min+c(0:n)*(max-min)/n # these are the n+1 "edges" of the size bins
  y <- 0.5*(b[1:n]+b[2:(n+1)]) # these are the mid-points of the n size classes
  # y <- b[1:n]+0.5*((max.size-min.size)/n)
  h <- y[2]-y[1] # bin width 
  
  # Growth and survival
  G <- outer(y, b[1:500], g.yx, model = gmodel, growSD = gSD, h = h, data = data_g, elast = elast, perturb = gperturb, t.clamp = g.t.clamp, ba.clamp = g.ba.clamp)
  # G is an n*n matrix, currently 500*500 = 250,000
  # each column is one of n sizes at time t (the mid-points), each row is the transition probability to a new size at time t+dt
  #plot(y, G[,10], type = "l", ylab = "density", xlim = c(0,55)) # try with larger and larger values for the G column, the PD marches down/across
  # S <- s.x(y, PPTann = ppt_yr_val, Tann = t_yr_val, interval = 1)
  S <- s.x(model = smodel, y, data = data_s, interval = 1, elast = elast, perturb = sperturb, t.clamp = s.t.clamp)
  
  P <- G
  for (k in 1:n) P[,k] <- G[,k]*S #survival*growth subkernel
  
  # Recruitment
  R <- h*outer(y, y, fec, model = rmodel, data = data_r, elast = elast, perturb = rperturb, ba.clamp = r.ba.clamp)
  # Entire kernel
  K <- P + R
  return(K)
}




