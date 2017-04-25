VariableSummary<-function(data,
                          name="Data",
                          decimal=2,
                          graph=TRUE,
                          pdf = TRUE,
                          path = Graphics_Path){ # Create a VariableSummary object which is
  
  library(e1071) # Need to install package most likely. 
  
  # Coefficient of Dispersion
  CoD <- function(X) {return(100* mean(abs(X - median(X)),na.rm = T)/median(X,na.rm = T))}
  
  # Coefficient of Variability
  CoV <- function(X) {return(100 * (sd(X,na.rm = T)/mean(X,na.rm = T)))}
  
  # The function of user inputs data and decimal.
  # Data is a numeric vector that we want to 
  # perform a series of statistical test on, and 
  # decimal is the number of decimal place
  # desired in our summary output.
  
  sum.val<-sum(data,na.rm = T)        # Save sum of the data
  mean.val<-mean(data,na.rm = T)      # Save the mean of the data
  median.val<-median(data, na.rm = T)  # Save the median of the data
  var.val<-var(data, na.rm = T)        # Save the variance of the data
  sd.val<-sd(data,na.rm = T)          # Save the standard deviation of the data
  mad.val<-mad(data,na.rm = T)        # Save the mean absolute deviaion of the data
  n<-as.integer(nrow(data))          # Save the length of the data
  invalidNA<-length(which(is.na(data)))
  se<-sd.val/sqrt(n)        # Calculate and save the standard error of the data
  low.val<-range(data,na.rm = T)[1]   # Save the low value of the range of data
  hi.val<-range(data,na.rm = T)[2]    # Save the high value of the range of data
  low.quant05<-quantile(data,probs = .05, na.rm = T) # Calculate the 5th percentile and save
  hi.quant95<-quantile(data,probs = .95,na.rm = T)  # Calculate the 95th percentile and save
  low.quant25<-quantile(data,probs = .25, na.rm = T) # Calculate the 25th percentile and save
  hi.quant75<-quantile(data,probs = .75,na.rm = T)  # Calculate the 75th percentile and save
  skewness.val<-skewness(data,na.rm = T) # Calculate the variable's skewess 
  # *Uses e1071 library
  kurtosis.val<-kurtosis(data,na.rm = T) # Calculate the variables's Kurtosis 
  # *Uses e1071 library
  CoD.val<-CoD(data) # Calculate the variable's Coefficient of Dispersion
  CoV.val<-CoV(data) # Calculate the variable's Coefficient of Variation
  # Create an R object called VarSummary consisting of the rounded values of the saved
  # variables. Arrange and re-name variables to display using the combine c() function. 
  VarSummary<-round(c("Length"=n, 
                      "NA"=invalidNA,
                      "Sum"=sum.val,
                      "Median"=median.val,
                      "Variance"=var.val,
                      "Std.Dev"=sd.val,
                      "Mean Abs Dev"=mad.val,
                      "Std.Err"=se,
                      "Range Low"=low.val,
                      low.quant05,
                      low.quant25,
                      "Mean"=mean.val,
                      hi.quant75,
                      hi.quant95,
                      "Range High"=hi.val,
                      "Skew"=skewness.val,
                      "Kurtosis"=kurtosis.val,
                      "CoD" = CoD.val,
                      "CoV" = CoV.val),
                    decimal)
  
  
  #############################################################################  
  # Graphical Depiction
  #############################################################################  
  if(graph==TRUE){
    
    
    par(mfrow=c(2,2)) # Set graph parameters to 2 x 2
  
    if(pdf == TRUE) {pdf(file=paste0(Graphics_Path, "Variable Summary ", name, " Graph.pdf"),
        width = 13.5, 
        height = 8.5) # Open the pdf
      par(pin = c(10.193889/1.22,10.193889/(1.22*1.6180)))
    }
    # 1. Histogram with Normal and Density c.numeric_version
    
    hist(data,
         breaks = length(unique(data)),
         col = "white",
         border = "navyblue",
         density = 50,
         probability = T,
         include.lowest = T,
         main = paste0("Histogram of ",name),
         xlab = name,
         ylab = "Density")
    
    curve(dnorm(x,mean=mean(data),sd = sd(data)), 
          col = "black", lwd=2, add=TRUE ,yaxt="n") 
    
    lines(density(data),col = "navyblue", lwd=2)
    
    # 2. Box and Whisker Plot 
    
    boxplot(data,
            varwidth = F,
            notch = T,
            border = 'black',
            col = 'navyblue',
            density = 75,
            horizontal = T,
            main = paste0("Boxplot of ",name),
            xlab = paste0("Values in ",name))
    
    # 3. Scatterplot
    
    plot(data,
         main = paste0("Scatterplot of ",name),
         ylab = paste0("Values in ",name),xlab = paste0("Obsv. Number ",name)
    )
    
    # A. Add Mean line
    abline(h = mean(data),lwd = 2,lty=4,col = 'navyblue')
    text(length(data)/1.2,y = mean(data),labels = "Mean",cex = 1,
         col = "navyblue")
    
    # Add Median line
    abline(h = median(data),lwd = 2,lty=5,col = 'black')
    text(length(data)/1.5,y = median(data),labels = "Median",cex = 1,
         col = "black")
    
    # 4. Quantile-Quantile Plot   
    
    qqnorm(data,main = paste0("Normal Q-Q Plot ",name),col = 'navyblue',pch = 1, cex = 1)
    
    if(pdf == TRUE) {dev.off()}
   
  }# End if function
  
  par(mfrow=c(1,1)) 
  
  return(VarSummary) # Return the VarSummary Object
}# End Function
