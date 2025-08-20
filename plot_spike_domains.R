# Function to plot spike protein domains

# Define the spike protein domain data
types <- c("chain", "domain", "domain", "domain", "domain", "domain", "domain", "domain")
description <- c(
  "Spike",  # Full chain
  "NTD",    # N-terminal domain
  "RBD",    # Receptor Binding Domain
  "RBM",    # Receptor Binding Motif
  "FP",     # Fusion Peptide
  "HR1",    # Heptad Repeat 1
  "HR2",    # Heptad Repeat 2
  "TM"      # Transmembrane Domain
)
full_names <- c(
  "Spike Protein (Full Chain)", 
  "N-terminal Domain (NTD)", 
  "Receptor Binding Domain (RBD)", 
  "Receptor Binding Motif (RBM)", 
  "Fusion Peptide (FP)", 
  "Heptad Repeat 1 (HR1)", 
  "Heptad Repeat 2 (HR2)", 
  "Transmembrane Domain (TM)"
)
begin <- c(1, 15, 319, 437, 788, 912, 1163, 1214)
end <- c(1273, 305, 541, 508, 830, 984, 1213, 1237)
col <- c("#808080", "#96ceb4", "#ff6f69", "#ff6f69", "#ffcc5c", "#FFC0CB", "#FFC0CB", "#d1ecf1")


# Assemble into a data frame
features <- data.frame(types, description, full_names, begin, end, col)


plot_spike_domains <- function(features, num_columns = 2) {
  # Determine screen dimensions
  screen.width <- max(features$end)
  screen.height <- 10  # Arbitrary height
  
  # Open plot canvas
  plot(c(-10, screen.width), 
       c(-10, screen.height),  # Increased height to make space for the legend
       type = "n", 
       xlab = "", 
       ylab = "", 
       yaxt = 'n', 
       xaxt = 'n')  # Suppress the axes
  
  # Draw rectangles for each domain
  for (i in 1:nrow(features)) {
    rect(
      xleft   = features$begin[i],
      ytop    = screen.height / 2 + 2.5,
      ybottom = screen.height / 2 - 2.5,
      xright  = features$end[i],
      col = features$col[i]
    )
    
    # Adjust the position of "RBD" to avoid overlap
    if (features$description[i] == "RBD") {
      text(
        x = (features$begin[i] + features$end[i]) / 2 - 20, # Move slightly to the left
        y = screen.height / 2, 
        labels = features$description[i], 
        cex = 0.8, 
        col = "black"
      )
    } else {
      # Add the abbreviation inside the box
      text(
        x = (features$begin[i] + features$end[i]) / 2, 
        y = screen.height / 2, 
        labels = features$description[i], 
        cex = 0.8, 
        col = "black"
      )
    }
    
    # Add the start amino acid number above the block
    text(
      x = features$begin[i], 
      y = screen.height / 2 + 4, 
      labels = features$begin[i], 
      cex = 0.8, 
      col = "black"
    )
  }
}
