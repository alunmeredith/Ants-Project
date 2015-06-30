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
  
  # This cleans up the file names but makes the function ungeneralised 
  file.names = sub("-", "", substr(file.names, 11, nchar(file.names) - 4 ))
  
  # remove names of the leaf directory from name of data.frame
  directory.name = tail(unlist(strsplit(directory, "/")), 1)
  file.names <- sub(directory.name, "", file.names)
  
  # loop through file paths and read them into a list of data frames
  temp = list()
  for (i in seq_along(file.paths)) {
    temp[[i]] = read.table(file.paths[i], col.names=(c("time", "x", "y")))
  }
    
  # returns a list of data frames (names have been streamlined)
  names(temp) <- file.names
  return(temp)  
}

# This function reads data files recursively and extracts them to a nested list
# -----------------------------------------------------------------------------
# The only argument is a root directory

read.list <- function(directory.list, index = NULL)
{
  if (is.null(index))
  {
    index <- list()
  }
  
  # for each readable file add an item to the list
  files.list = list.files(directory.list, pattern=".txt")
  if (length(files.list) > 0 ) {
    temp <- read(directory.list)
    index <- temp
    #index <- append(index, temp)
  }
  
  # for each directory add a list to the list and recurse 
  root.directories = list.dirs(directory.list, recursive=FALSE)
  if (length(root.directories) > 0) 
  {
    for ( j in seq_along (root.directories) ) {
      # recursively read the subirectory into a list
      sublist = read.list(root.directories[j], index[ length(files.list) + j])
      index[ length(files.list) + j] = list(sublist)
    }
    
    # Extract direcectory names, clean them and add to index
    names = unlist(strsplit(root.directories, "/"))
    names = tail(matrix(names, ncol=length(root.directories)),1)
    names = append(names(index), names, after=length(files.list))[1:length(index)]
    names = tolower(sub( "-", "",names))
    names(index) <- names
    
  }
  return(index)
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
#     moving -- logical, is distance > 0
#     eventboundary -- 1 if first measurement moving after stationary, -1 if first measurement of stationary after moving, else 9
#     acceleration -- current acceleration dv/dt (value set to 0 for first two measurements which cannot be calculated)
#     event -- numbers sequentially moving and stationary events
# ------------------------------------------------------------------------------------------------

instantaneous.calculations <- function(list) 
{
  
  # load (and install) dplyr 
  if (!require("dplyr")) {
    install.packages("dplyr")
  }
  require("dplyr")
  
  return.list = list()
  for(i in seq_along(list)){
    data = as.tbl(list[[i]])
    
    # calculates changes in t, x, y from previous measurement
    data.dt = data[2:nrow(data),] - data[1:(nrow(data)-1),]
    data.dt = rbind(c(NA, NA, NA), data.dt)
    names(data.dt) <- c("dt", "dx", "dy")
    data = cbind(data, data.dt)
    
    # calculates instantaneous distance and velocity direction measurements and total distance. 
    data = mutate(data, dz = sqrt(dx^2 + dy^2), velocity = dz/dt, direction = atan( dx/dy ), distance = cumsum(ifelse(is.na(dz), 0, dz)) + dz*0, moving = velocity > 0)
    
    # calculates event boundaries (1 if start moving, -1 if stopping) and change in velocity
    accel.event = data[2:nrow(data), c("velocity", "moving")] - data[1:( nrow(data) - 1 ), c("velocity", "moving")]
    accel.event = rbind(c(NA,NA), accel.event)
    names(accel.event) = c("dv" , "eventboundary")
    data <- bind_cols(data, accel.event)
    
    # calculates acceleration and event id (removes change in velocity)
    data <- 
      mutate(data, acceleration=dv/dt,
             ant = gsub('[a-zA-Z]', "", names(list[i])), 
             event=cumsum(ifelse(is.na(abs(eventboundary)), 0, abs(eventboundary))) + abs(eventboundary*0)) %>% 
      select(-dv)
    # Coerce the ant number to be a numeric variable (cannot coerce to factor at this point)
    data$ant <- as.numeric(data$ant)
    # return the altered data frame to the list
    return.list[[i]] = data
  }
  # name and return the list
  names(return.list) = names(list)
  return(return.list)
}

# ------------------------------------------------------------------------------------ 
# This function takes a list of data frames as an arguement and combines them together
#

stitch <- function(x)
{
  # load (and install) dplyr 
  if (!require("dplyr")) {
    install.packages("dplyr")
  }
  require("dplyr")
  
  bound <- x[[1]]
  for (i in seq(2, length(x)))
  {
    bound <- bind_rows(bound, x[[i]])
  }
  return(bound)
}

