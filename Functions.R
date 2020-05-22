################################################################################
################################################################################
ipak <- function(pkg){
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg)) 
    
    install.packages(new.pkg, dependencies = TRUE)
  
  sapply(pkg, require, character.only = TRUE)
  
}

################################################################################
################################################################################
# LoadData function imports a single txt file from the Data folder by its
# index in the list.files() function

LoadData <- function(file_ID = 4){
  
  # Check datalog files
  Av_Data <- list.files(paste0(getwd(),"/Data"))
  
  # Import file
  file <- paste0(getwd(),"/Data/",Av_Data[file_ID])
  
  # Create dataframe
  Data <<- read.table(file, header = FALSE, sep = ",", dec = ".")
  names(Data) <<- c("ID","LAT","LON","ALT","TIME","DATE","SAT","SPEED","Temperature","ALT_B","HUM","PPM")
  
  # Delete LAT LON outliners 
  Data <<- Data[!Data$LON %in% boxplot(Data$LON,plot=FALSE)$out,]
  Data <<- Data[!Data$LAT %in% boxplot(Data$LAT,plot=FALSE)$out,]
  
  # Delete less than 3 SAT data even if its blocked from the arduino
  Data <<- Data[Data$SAT >= 3,]
  
  # Reset ID
  Data$ID <<- seq(1:length(Data$ID))
  
  # Transform Date to %d%m%y 
  Data <<- Data %>% mutate(DATE = as.Date(as.character(DATE), "%d%m%y"))
  
  # Transform Time to %h%m%s 
  Data$TIME <<- as.character(Data$TIME)
  Data$TIME <<- unlist(lapply(Data$TIME, FUN = function(x){
    x <- substr(x,1,6)
    stri_sub(x, 3, 2)
    stri_sub(x, 3, 2) <- ":"
    stri_sub(x, 6, 5)
    stri_sub(x, 6, 5) <- ":"
    return(x)
    
  }))
  
  Data <<- Data %>% mutate(TIME = as_hms(TIME))
  
}
