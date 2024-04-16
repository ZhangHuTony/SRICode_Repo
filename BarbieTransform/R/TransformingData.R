

#' Position Normalization
#'
#' Creates a new column in a data frame which goes from 0 to 1000 in x and y directions to -500 to 500.
#' Also adds new columns of useful information such as distance from nearest edge and distance from the center.
#' @param Df The dataframe which has the positions as Position.X and Position.Y
#' @return Returns the dataframe with the normalized positions as Centered_Position.X and Centered_Position.Y as well as Distance_From_Edge and Distance_From_Center
#' @export
#Call Example:
rescalePosition <- function(Df){
  Df$Centered_Position.X = Df$Position.X - 500
  Df$Centered_Position.Y = Df$Position.Y - 500
  Df$Distance_From_Edge = pmin(500 - abs(Df$Centered_Position.X), 500 - abs(Df$Centered_Position.Y))
  Df$Distance_From_Center = sqrt(Df$Centered_Position.X^2 + Df$Centered_Position.Y^2)
}

#Method which can be used after the rescale position method
addDistanceIntervals <- function(Df, interval_width){
  Df$distance_interval <- cut(Df$Distance_From_Edge, breaks = seq(0, max(Df$Distance_From_Edge) + interval_width, by = interval_width), include.lowest = TRUE, right = FALSE)
  return (Df)
}

reverseDistanceIntervals <- function(Df){
  Df$distance_interval <- factor(Df$distance_interval, levels = rev(levels(Df$distance_interval)))
  return(Df)
}

addDurations <- function(Df){
  # Initialize duration column
  Df$Duration.Min <- NA
  Df$Duration.Hrs <- NA

  # Create duration column based on occurrence of Tracking.ID
  for (id in unique(Df$Tracking.ID)) {
    currDuration = 15
    for(index in which(Df$Tracking.ID == id)){
      Df$Duration.Min[index] = currDuration
      Df$Duration.Hrs[index] = currDuration/60
      currDuration = currDuration+15
    }
  }


  return(Df)
}

addBinaryStoppedTracking <- function(Df){
  # Initialize left column
  Df$Stopped_Tracking <- 0

  # Find last row appearance of each unique Tracking.ID and mark as 1
  unique_ids <- unique(Df$Tracking.ID)
  for (id in unique_ids) {
    last_row_index <- max(which(Df$Tracking.ID == id))
    Df$Stopped_Tracking[last_row_index] <- 1
  }

  return(Df)
}

#Function which aggregates data using the Tracking ID as the shared key.
#Call Example: aggregatedData = aggregate_TrackingID(nonAggregatedData)
aggregate_TrackingID <- function(nonAggDf){
  agg_df <- nonAggDf %>%
    group_by(Tracking.ID) %>%
    summarise(
      #Time between frame is 15 minutes
      Duration.minutes = (max(Frame) - min(Frame))*15,
      Duration.hours = Duration.minutes/60,
      Position.FinalX = last(Position.X),
      Position.FinalY = last(Position.Y)
    )
}


