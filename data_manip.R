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
  paths <- c()
  overlap_flag <- FALSE
  # If groups don't immediately overlap, search for path
  while (!overlap_flag) {
    # Get groups not in current set
    for (g in single_groups[[1]]) {
      # Only Evaluate if egg group not in path already
      if (!(g %in% path)) {
        # Check if egg group overlaps has pokemon attached
        possible_path <- unlist(c(start, g))
        path_length <- length(possible_path)
        print(possible_path)
        group1 <- possible_path[[path_length]]
        group2 <- possible_path[[path_length-1]]
        bridge_group1 <- unlist(sort(c(group1, group2)))
        print(bridge_group1)
        pokemon_bridge_group <- df[(df$EggGroupI == bridge_group1[[1]] & df$EggGroupII == bridge_group1[[2]])
                                   | (df$EggGroupI == bridge_group1[[2]] & df$EggGroupII == bridge_group1[[1]]), 'Pokemon']
        if (length(pokemon_bridge_group) > 0) {
          paths <- append(paths, bridge_group1)
        }
        # Iterate to account for dual groups (if list has more than 2 entries)
      }
    }
    overlap_flag <- TRUE
  }

}

# Test Group

df <- read.csv('pokemon.csv')
obj <- get_egg_group_steps(df)[[1]]
get_shortest_path(df, "Pikachu", "Geodude")

# Calc shortest path between groups

# e.g. Fairy to Mineral
# Fairy -> intermediate groups -> mineral


