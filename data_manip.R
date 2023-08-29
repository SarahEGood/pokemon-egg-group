library('dplyr')
library('toolbox')
library('rlist')

# Functions to create lookup tables for paths

# Get list of unique egg group possibilities
get_egg_group_list <- function (df) {
    # Get list of all egg group combinations
    egg_groups <- df %>% select('Pokemon', 'EggGroupI', 'EggGroupII')
    # Remove NA values
    egg_groups <- subset(egg_groups, egg_groups$EggGroupI != "-" & !is.na(egg_groups$EggGroupI) & egg_groups$EggGroupI != "")
    # Listify egg groups
    egg_groups$EggGroupList <- combineCols(list("x"=egg_groups$EggGroupI, "y"=egg_groups$EggGroupII))
    #egg_groups$EggGroupList <- apply(cbind(egg_groups$EggGroupI, egg_groups$EggGroupII), 1, function(x) paste(sort(x), collapse=" "))
    egg_groups$EggGroupList <- lapply(egg_groups$EggGroupList, sort, decreasing=FALSE)
    egg_groups2 <- unique(egg_groups$EggGroupList)
    return (list(egg_groups, egg_groups2))
}

# Get shortest path between each egg group
get_shortest_path <- function(df, start_pkmn, finish_pkmn) {
  # Note for later: Filter generation in previous step
  # Load data
  obj <- get_egg_group_list(df)
  df <- obj[[1]]
  all_groups <- obj[[2]]
  single_groups <- as.list(read.csv('egg_groups.csv'))
  # Get Pkmn egg group
  start <- df[df$Pokemon == start_pkmn,'EggGroupList'][[1]]
  finish <- df[df$Pokemon == finish_pkmn,'EggGroupList'][[1]]
  # Remove nulls (if Pokemon belongs to single egg group)
  start <- start[nzchar(start)]
  finish <- finish[nzchar(finish)]
  # Set up while loop
  steps <- 1
  paths <- c(list(start))
  overlap_flag <- FALSE
  # Check if groups overlap; skip entire loop if true
  if (length(unique(unlist(c(start, finish)))) < length(unlist(c(start, finish)))) {
    overlap_flag <- TRUE
  }
  # If groups don't immediately overlap, search for path
  while (!overlap_flag) {
    # Init new paths var (so as not to duplicate shorter paths)
    new_paths <- c()
    # Loop over existing paths
    for (s in paths) {
      # Get groups not in current set
      for (g in single_groups[[1]]) {
        possible_path <- unlist(c(s, g))
        path_length <- length(possible_path)
        print(possible_path)
        # Only Evaluate if egg group not in path already
        if (!(g %in% s)) {
          # Check if egg group overlaps has pokemon attached
          group1 <- possible_path[[path_length]]
          group2 <- possible_path[[path_length-1]]
          bridge_group1 <- unlist(sort(c(group1, group2)))
          pokemon_bridge_group <- df[(df$EggGroupI == bridge_group1[[1]] & df$EggGroupII == bridge_group1[[2]])
                                     | (df$EggGroupI == bridge_group1[[2]] & df$EggGroupII == bridge_group1[[1]]), 'Pokemon']
          if (length(pokemon_bridge_group) > 0) {
            new_paths <- c(new_paths, list(possible_path))
            print(new_paths)
            if (g %in% finish) {
              overlap_flag <- TRUE
            }
          }
          # Iterate to account for dual groups (if list has more than 2 entries)
          else if (length(possible_path) > 2) {
            group1 <- possible_path[[path_length]]
            group2 <- possible_path[[path_length-2]]
            bridge_group2 <- unlist(sort(c(group1, group2)))
            pokemon_bridge_group <- df[(df$EggGroupI == bridge_group2[[1]] & df$EggGroupII == bridge_group2[[2]])
                                       | (df$EggGroupI == bridge_group2[[2]] & df$EggGroupII == bridge_group2[[1]]), 'Pokemon']
            if (length(pokemon_bridge_group) > 0) {
              new_paths <- c(new_paths, list(possible_path))
              print(new_paths)
              if (g %in% finish) {
                overlap_flag <- TRUE
              }
            }
          }
          # At end of loop, check overlap flag if possible paths contain finish egg group
        }
      }
    }
    # Assign new paths to main variable
    paths <- new_paths
    # Remove paths that contain 
  }
  print("Loop has broken")
  # Remove paths except for ones that have a finishing egg group
  print(paste("length finish: ", length(finish)))
  if (length(finish) == 1) {
    paths <- paths[sapply(paths, function(x) finish[[1]] %in% x)]
  } else {
    paths <- paths[sapply(paths, function(x) (finish[[1]] %in% x | finish[[2]] %in% x))]
  }
  return (paths)

}

# Test Group

df <- read.csv('pokemon.csv')
obj <- get_egg_group_steps(df)[[1]]
result <- get_shortest_path(df, "Pikachu", "Tentacool")

