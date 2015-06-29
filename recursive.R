list <- list()

read.list <- function(directory.list, index)
{
  # for each readable file add an item to the list
  files.list = list.files(directory.list, pattern=".txt")
  if (length(files.list) > 0 ) {
    for ( i in seq_along(files.list)) {
      temp <- list(files.list[i])
      index[i] <- as.list(temp)
    }
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
  }
  if (!length(root.directories) > 0) {
    return(index)
  }
  return(index)
}


test.directory <-  paste(getwd(), "/Ant Data Package/", sep="")
directory.list = test.directory
result <- read.list(test.directory, list)
result2 <- read.list(root.directories[1], result[4 + 1])
i = 1
j = 1
index <- list()
