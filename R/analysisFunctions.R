library(xtable)
library(ggplot2)
library(scales)


# not exported for users, imports data and grabs the factory factors we need.
subset.simulation.data <- function(simulation.data, name) {
  # error checking
  stopifnot(is.data.frame(simulation.data))
  stopifnot(is.character(name))
  
  input.data <- as.data.frame(
    cbind(
      simulation.data,
      total_inv = simulation.data$daily_inv + simulation.data$in_transit
    )
  )
  
  column.names <- colnames(input.data)
  
  stopifnot(name %in% as.character(input.data$factory)) # makes sure no errors are present
  output.data <- input.data[as.character(input.data$factory) %in% name, 
                            c("date", "factory", "region", "daily_inv", "in_transit", "total_inv")]
  
  return(output.data)
}

rm.graphs <- function(fac.data, on.hand, in.transit, total.inv, name, range) {
  library(ggplot2)
  stopifnot(is.data.frame(fac.data) | is.ggplot(fac.data));
  stopifnot(is.numeric(on.hand)); stopifnot(length(on.hand) == 1);
  stopifnot(is.numeric(in.transit)); stopifnot(length(in.transit) == 1);
  stopifnot(is.numeric(total.inv)); stopifnot(length(total.inv) == 1);
  stopifnot(is.character(name));
  stopifnot(is.numeric(range)); stopifnot(length(range) > 1);
  
  if (is.data.frame(fac.data)) {
    plt.data <- ggplot(fac.data)
  } else if (is.ggplot(fac.data)) {r
                                   plt.data <- fac.data
  }
  
  graphs <- vector("list", length = 3)
  
  comparison <- as.data.frame(cbind(on.hand, in.transit, total.inv))
  
  graphs[[1]] <- plt.data + geom_point(aes(x = date, y = daily_inv)) +
    stat_smooth(aes(x = date, y = daily_inv), col = "orange") + 
    geom_hline(aes(yintercept = on.hand), data = comparison, col = "blue", linetype = "dashed") + 
    labs(list(title = paste("Daily On-Hand Inventory -", name),
              x = "Simulation Days", y = "Daily Inventory (kg)")) +
    scale_y_continuous(labels = comma, 
                       limits = c(0, max(c(fac.data$daily_inv, on.hand)))) + 
    scale_x_continuous(labels = comma, limits = c(min(range), max(range)))
  
  graphs[[2]] <- plt.data + geom_point(aes(x = date, y = in_transit)) +
    stat_smooth(aes(x = date, y = in_transit), col = "orange") + 
    geom_hline(aes(yintercept = in.transit), data = comparison, col = "blue", linetype = "dashed") +     
    labs(list(title = paste("Daily In-Transit Inventory -", name),
              x = "Simulation Days", y = "Daily Inventory (kg)")) +
    scale_y_continuous(labels = comma, 
                       limits = c(0, max(c(fac.data$in_transit, in.transit)))) + 
    scale_x_continuous(labels = comma, limits = c(min(range), max(range)))
  
  graphs[[3]] <- plt.data + geom_point(aes(x = date, y = total_inv)) +
    stat_smooth(aes(x = date, y = total_inv), col = "orange") + 
    geom_hline(aes(yintercept = total.inv), data = comparison, col = "blue", linetype = "dashed") +     
    labs(list(title = paste("Daily Total FOB Inventory -", name),
              x = "Simulation Days", y = "Daily Inventory (kg)")) +
    scale_y_continuous(labels = comma, 
                       limits = c(0, max(c(fac.data$total_inv, total.inv)))) + 
    scale_x_continuous(labels = comma, limits = c(min(range), max(range)))
  
  return(graphs)
}


#' Creates graphs of the on hand, in transit and total inventory as compared to input information
#' 
#' This function takes the output data, comparison data and whether or not we want a range
#' of values to be compared and summarizes the data at a factory level.
#' 
#' @param simulation.data the data frame of simulation output
#' @param comparison.data the data frame of comparison data
#' @param name the name of the factory to be summarized
#' @param range a vector of at least length two which gives us the input data ranges
#' from the simulation to be compared.
#' 
#' @export
inventory.data <- function(simulation.data, comparison.data, name, range = NULL) {
  stopifnot(is.data.frame(simulation.data));
  stopifnot(is.data.frame(comparison.data));
  stopifnot(is.character(name));
  stopifnot(is.numeric(range) | is.null(range)); stopifnot(length(range) > 1);
  
  if (is.null(range)) {
    range <- as.numeric(output.data$date)
  }
  
  comp.data <- comparison.data[as.character(comparison.data$DESTINATION) == name, c("DESTINATION", "REGION", "Facility.Inventory", "In.transit.Inventory")]
  
  simulation.data <- subset.simulation.data(simulation.data, name)
  
  on.hand <- as.numeric(comp.data$Facility.Inventory)
  in.transit <- as.numeric(comp.data$In.transit.Inventory)
  total.fob <- as.numeric(on.hand + in.transit)
  
  graphs <- rm.graphs(simulation.data, on.hand, in.transit, total.fob, name, range)
  
  for (i in 1:length(graphs)) {
    print(graphs[[i]])
  }
  
  print(xtable(
    desc.stat(simulation.data, on.hand, in.transit, total.fob, name, range = 500:1500), 
    caption = name, digits = 0), 
        type = "html", caption.placement = "top"
  )
  
}

#' Creates graphs of the on hand, in transit and total inventory as compared to input information
#' 
#' This function takes the output data, comparison data and whether or not we want a range
#' of values to be compared and summarizes the data at a global level.
#' 
#' @param simulation.data the data frame of simulation output
#' @param comparison.data the data frame of comparison data
#' @param range a vector of at least length two which gives us the input data ranges
#' from the simulation to be compared.
#' 
#' @export
global.data <- function(simulation.data, comparison.data, range = NULL) {
  # error checking
  stopifnot(is.data.frame(simulation.data));
  stopifnot(is.data.frame(comparison.data));
  stopifnot(is.numeric(range) | is.null(range)); stopifnot(length(range) > 1);
  
  if (is.null(range)) {
    range <- as.numeric(output.data$date)
  }
  
  fac.list <- unique(simulation.data$factory)
  
  for (i in 1:length(fac.list)) {
    if (i == 1) {
      fac.data <- subset.simulation.data(simulation.data, as.character(fac.list[i]))
    } else {
      temp <- subset.simulation.data(simulation.data, as.character(fac.list[i]))
      for (j in c("daily_inv", "in_transit", "total_inv")) {
        fac.data[[j]] <- fac.data[[j]] + temp[[j]]
      }
    }
    fac.data$factory <- as.factor(as.character(rep("Global", length = nrow(fac.data))))
  }
  
  fac.data <- as.data.frame(
    cbind(
      fac.data,
      total_inv = fac.data$daily_inv + fac.data$in_transit
    )
  )
  
  name <- "Global"
  
  on.hand <- sum(as.numeric(comparison.data$Facility.Inventory))
  in.transit <- sum(as.numeric(comparison.data$In.transit.Inventory))
  total.fob <- sum(as.numeric(comparison.data$Facility.Inventory)) + sum(as.numeric(comparison.data$In.transit.Inventory))
  
  graphs <- rm.graphs(fac.data, on.hand, in.transit, total.fob, name, range)
  
  for (i in 1:length(graphs)) {
    print(graphs[[i]])
  }
  
  print(xtable(
    desc.stat(fac.data, on.hand, in.transit, total.fob, name, range = range), 
    caption = name, digits = 0), 
        type = "html", caption.placement = "top"
  )
}


#' Creates graphs of the on hand, in transit and total inventory as compared to input information
#' 
#' This function takes the output data, comparison data and whether or not we want a range
#' of values to be compared and summarizes the data at a regional level.
#' 
#' @param simulation.data the data frame of simulation output
#' @param comparison.data the data frame of comparison data
#' @param name the name of the region to be summarized
#' @param range a vector of at least length two which gives us the input data ranges
#' from the simulation to be compared.
#' 
#' @export
region.data <- function(simulation.data, comparison.data, name, range = NULL) {
  # error checking
  # error checking
  stopifnot(is.data.frame(simulation.data));
  stopifnot(is.data.frame(comparison.data));
  stopifnot(is.numeric(range) | is.null(range)); stopifnot(length(range) > 1);
  stopifnot(is.character(name))
  
  if (is.null(range)) {
    range <- as.numeric(output.data$date)
  }
  
  sim.subset <- simulation.data[as.character(simulation.data$region) == name, ]
  fac.list <- unique(sim.subset$factory)
  
  simulation.data <- as.data.frame(
    cbind(
      simulation.data,
      total_inv = simulation.data$daily_inv + simulation.data$in_transit
    )
  )
  
  for (i in 1:length(fac.list)) {
    if (i == 1) {
      fac.data <- subset.simulation.data(simulation.data, as.character(fac.list[i]))
    } else {
      temp <- subset.simulation.data(simulation.data, as.character(fac.list[i]))
      for (j in c("daily_inv", "in_transit", "total_inv")) {
        fac.data[[j]] <- fac.data[[j]] + temp[[j]]
      }
    }
    fac.data$factory <- as.factor(as.character(rep(as.character(name), length = nrow(fac.data))))
  }
  
  on.hand <- sum(as.numeric(comparison.data$Facility.Inventory))
  in.transit <- sum(as.numeric(comparison.data$In.transit.Inventory))
  total.fob <- sum(as.numeric(comparison.data$Facility.Inventory)) + sum(as.numeric(comparison.data$In.transit.Inventory))
  
  graphs <- rm.graphs(fac.data, on.hand, in.transit, total.fob, name, range)
  
  for (i in 1:length(graphs)) {
    print(graphs[[i]])
  }
  
  print(
    xtable(
      desc.stat(fac.data, on.hand, in.transit, total.fob, name, range = range), 
      caption = name, digits = 0), 
    type = "html", caption.placement = "top"
  )
}

#' Creates a data frame of the on hand, in transit and total inventory descriptive statistics
#' 
#' @param data the file path to the folder that contains the CSV output
#' @param name the character value of the name to be plotted
#' @param range a vector indicating a time range or a numeric vector length one 
#' to indicate the starting point and to output graphs to the end of the input data.
#' 
#' @export
desc.stat <- function(data, on.hand, in.transit, total.fob, name, range) {
  # error checking
  stopifnot(is.data.frame(data));
  stopifnot(is.numeric(on.hand)); stopifnot(is.numeric(in.transit)); stopifnot(is.numeric(total.fob)); 
  stopifnot(is.character(name));
  stopifnot(is.numeric(range)); stopifnot(length(range) > 1);
  
  # creates the date subset by factory
  output.data <- subset.simulation.data(data, name)
  # creates the data subset by range
  output.data <- output.data[range, ]
  
  desc.stat <- as.data.frame( 
    rbind(
      cbind(
        sum(output.data$daily_inv == 0),
        sum(output.data$in_transit == 0),
        sum(output.data$total_inv == 0)
      ),
      cbind(
        min(output.data$daily_inv),
        min(output.data$in_transit),
        min(output.data$total_inv)
      ),
      cbind(
        quantile(output.data$daily_inv, probs = 0.25),
        quantile(output.data$in_transit, probs = 0.25),
        quantile(output.data$total_inv, probs = 0.25)
      ),
      cbind(
        median(output.data$daily_inv),
        median(output.data$in_transit),
        median(output.data$total_inv)
      ),
      cbind(
        mean(output.data$daily_inv),
        mean(output.data$in_transit),
        mean(output.data$total_inv)
      ),
      cbind(
        quantile(output.data$daily_inv, probs = 0.75),
        quantile(output.data$in_transit, probs = 0.75),
        quantile(output.data$total_inv, probs = 0.75)
      ),
      cbind(
        max(output.data$daily_inv),
        max(output.data$in_transit),
        max(output.data$total_inv)
      ),
      cbind(
        on.hand,
        in.transit,
        total.fob
      )
    )
  )
  
  names(desc.stat) <- c("On Hand", "In Transit", "Total FOB")
  row.names(desc.stat) <- c("Zero Inventory", "Minimum", "25th Percentile", "Median", "Mean", "75th Percentile", "Maximum", "Snapshot")
  return(desc.stat)
}








