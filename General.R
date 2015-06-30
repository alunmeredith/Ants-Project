# Defines functions
source(file="Functions.R")

# -------------------------------
# Extract all the data from files
# -------------------------------

raw <- read.list(paste(getwd(), "/../Ant Data Package", sep=""))

# -------------------------------------------------
# Calculate instantaneous variables (and ant index)
# -------------------------------------------------

instantaneous <- raw
for (j in seq_along(raw)) 
{
  for (i in seq_along(raw[[j]])) {
    instantaneous[[j]][[i]] <- instantaneous.calculations(raw[[j]][[i]])
  }
}

# -----------------------------------
# stitch ants together for same group
# -----------------------------------

# go through each leaf sublist and combine the different ant data.frames
stitched <- instantaneous
for (j in seq_along(stitched)){
  for (i in seq_along(stitched[[j]])) {
    # stitches the ant data frames together and groups them by ant/event
    stitched[[j]][[i]] <- group_by(  stitch(stitched[[j]][[i]])  , ant, event)  
  }
}

# --------------------------------------
# Analysing moving and stationary events 
# --------------------------------------
test <- stitched[[1]][[1]]
events <- group_by(test, ant, event, moving) %>%
  summarise(pathlength = sum(dz), directdistance = max(distance) - min(distance), av.speed = mean(velocity), duration =  sum(dt))
