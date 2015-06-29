# Install relevant packages
# -------------------------
if (!require("dplyr")) {
  install.packages("dplyr")
}
require("dplyr")


# Programatically read and save data files
# -------------------------------------------------------------------------------------
# takes a full directory path as an arguement
# reads every data.file in that directory (recursively) and returns them in a list
# name of data frames: name of files with redundant information (prefix, directory name) removed
# -------------------------------------------------------------------------------------
read <- function(directory) {
  
  # generate list of names and file paths for the sudirectory given
  file.paths = list.files(directory, full.names=TRUE, pattern=".txt")
  file.names = list.files(directory, pattern=".txt")
  file.names = sub("-", "", substr(file.names, 11, nchar(file.names) - 4 ))
  remove names of the leaf directory from name of data.frame
  file.names = matrix( unlist( strsplit( file.names, unlist(strsplit(directory, "/"))[10]  )), ncol=2, byrow=TRUE)[,2]
  
  # loop through file paths and read them into a list of data frames
  temp = list()
  for (i in seq_along(file.paths)) {
    temp[[i]] = read.table(file.paths[i], col.names=(c("time", "x", "y")))
  }
    
  # returns a list of data frames (names have been streamlined)
  names(temp) <- file.names
  

  
  return(temp)  
}
# This function calculates additional variables
# ------------------------------------------------------------------------------------------------
# Takes a list of data.frames of the raw format as arguement
# Calculates:
#     dt, dx, dy -- differences in variables from previous measurement 
#     dz -- distance travelled in previous measurement (in millimetres)
#     velocity -- dz/dt (millimetres per second)
#     direction -- angle between the direction of travel and the top face of the nest (in radians)
#     distance -- cumulative distance travelled since recording began
# ------------------------------------------------------------------------------------------------
instantaneous.calculations <- function(list) {
  return.list = list()
  for(i in seq_along(list)){
    data = list[[i]]
    
    # calculates changes in t, x, y from previous measurement
    data.dt = data[2:nrow(data),] - data[1:(nrow(data)-1),]
    data.dt = rbind(c(0, 0, 0), data.dt) # 0s instead of NAs chosen because ants spend large amounts of time stationary so its not going to affect the results
    names(data.dt) <- c("dt", "dx", "dy")
    data = cbind(data, data.dt)
    
    # calculates instantaneous distance and velocity direction measurements and total distance. 
    data = mutate(data, dz = sqrt(dx^2 + dy^2), velocity = abs(dz/dt), direction = atan( dx/dy ), distance = cumsum(dz))
    return.list[[i]] = data
  }
  names(return.list) = names(list)
  return(return.list)
}