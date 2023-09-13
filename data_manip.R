library('dplyr')
library('toolbox')
library('rlist')

# Functions to create lookup tables for paths

# Filter table to only include selected gen/game data
filter_pkmn_data <- function(df, generation, hidden_ability="") {
  new_df <- df[df[generation]=='X' & df$EggGroupI != '-'
               & df['Gender'] != 'None',]
  # Filter to hidden ability if able
  print(paste('Gen is ', generation))
  print(!(generation %in% c('GenII', 'GenIII', 'GenIV')))
  if ((!is.null(hidden_ability) && (hidden_ability != "")
      && !(generation %in% c('GenII', 'GenIII', 'GenIV')))) {
    new_df <- new_df[new_df["HiddenAbility"] == hidden_ability,]
  }
  return (new_df)
}

# Export list of Pkmn by game/generation
get_pkmn_by_gen <- function() {
  # Import direct from csv
  df <- read.csv('pokemon.csv')
  generation_list <- c('GenII', 'GenIII', 'GenIV', 'GenV', 'GenVI',
                       'GenVI','SunMoon','ORAS','SwSh','BDSP','SV')
  for (item in generation_list) {
    new_df <- filter_pkmn_data(df, item)
    file_name <- paste0('gen_data/', item, '.csv')
    write.csv(new_df[c('Nat','Pokemon',"HiddenAbility", "Gender")], file_name)
  }
}

# Get list of unique egg group possibilities
get_egg_group_list <- function (df, hidden_ability=NULL) {
    # Get list of all egg group combinations
    egg_groups <- df %>% select('Pokemon', 'EggGroupI', 'EggGroupII', 'HiddenAbility')
    # Filter to only pkmn with hidden ability (if selected)
    if (!is.null(hidden_ability) && (hidden_ability != "")) {
      egg_groups <- egg_groups[egg_groups$HiddenAbility == hidden_ability,]
    }
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
get_shortest_path <- function(df, start_pkmn, finish_pkmn, hidden_ability=NULL) {
  # Note for later: Filter generation in previous step
  # Load data
  obj <- get_egg_group_list(df, hidden_ability = hidden_ability)
  df <- obj[[1]]
  df_test2 <<- df
  all_groups <- obj[[2]]
  single_groups <- as.list(read.csv('egg_groups.csv'))
  # Filter to hidden ability if exists
  if (!is.null(hidden_ability) && (hidden_ability != "")) {
    df <- df[df$HiddenAbility == hidden_ability,]
  }
  # Get Pkmn egg group
  df_test <<- df
  start <- df[df$Pokemon == start_pkmn,'EggGroupList'][[1]]
  finish <- df[df$Pokemon == finish_pkmn,'EggGroupList'][[1]]
  # Remove nulls (if Pokemon belongs to single egg group)
  start <- start[nzchar(start)]
  finish <- finish[nzchar(finish)]
  # Set up while loop
  steps <- 1
  if (length(start) > 1) {
    paths <- c(list(start[[1]]), list(start[[2]]))
  } else {
    paths <- c()
  }
  overlap_flag <- FALSE
  # Check if groups overlap; skip entire loop if true
  if (length(unique(unlist(c(start, finish)))) < length(unlist(c(start, finish)))) {
    overlap_flag <- TRUE
    paths <- c('direct')
  }
  # Counter for cases where a path doesn't exist (i.e. too specific of params)
  termination_counter = 1
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
            if (g %in% finish) {
              overlap_flag <- TRUE
            }
          }
          # Iterate to account for dual groups (if list has more than 2 entries)
          else if (length(possible_path) > 2) {
            # Reverse egg groups in possible path
            group3 <- possible_path[[path_length-2]]
            possible_path[[path_length-2]] <- group2
            possible_path[[path_length-1]] <- group3
            bridge_group2 <- unlist(sort(c(group1, group2)))
            pokemon_bridge_group <- df[(df$EggGroupI == bridge_group2[[1]] & df$EggGroupII == bridge_group2[[2]])
                                       | (df$EggGroupI == bridge_group2[[2]] & df$EggGroupII == bridge_group2[[1]]), 'Pokemon']
            if (length(pokemon_bridge_group) > 0) {
              new_paths <- c(new_paths, list(possible_path))
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
    # break loop if a path can't be found after 7 iterations
    if (termination_counter > 6 & overlap_flag == FALSE) {
      print(paste("counter: ", termination_counter))
      return (NULL)
    } else {
      termination_counter <- termination_counter + 1
    }
  }
  print("Loop has broken")
  # Remove paths except for ones that have a finishing egg group
  if ('direct' %in% paths) {
    return (paths)
  } else if (length(finish) == 1) {
    paths <- paths[sapply(paths, function(x) finish[[1]] %in% x)]
  } else {
    paths <- paths[sapply(paths, function(x) (finish[[1]] %in% x | finish[[2]] %in% x))]
  }
  return (paths)
}

# Determine if cols values are empty or not
return_egggroup_num <- function(df, pkmn) {
  egg_grps <- df[df$Pokemon == pkmn,'EggGroupList'][[1]]
  egg_grp_num <- 0
  for (grp in egg_grps) {
    if (grp > length(0)) {
      egg_grp_num <- egg_grp_num + 1
    }
  }
  return (egg_grp_num)
}

# Returns the actual lists of pokemon for each path
# df = pokemon dataframe
# paths = paths from get_shortest_path()
# start_pkmn and finish_pkmn = starting and finishing pkmn
return_breeding_steps <- function(df, paths, start_pkmn, finish_pkmn, hidden_ability=NULL) {
  obj <- get_egg_group_list(df, hidden_ability=hidden_ability)
  df <- obj[[1]]
  if (!is.null(hidden_ability) && (hidden_ability != "")) {
    df <- df[df$HiddenAbility == hidden_ability,]
  }
  # Get Egg Groups amount for each pkmn
  # Only need length to determine path(s)
  start_l <- return_egggroup_num(df, start_pkmn)
  finish_l <- return_egggroup_num(df, finish_pkmn)
  print(paste(start_l, finish_l))
  all_pkmn_path <- c()

  # Get each group of pkmn for each element of path
  for (path in paths) {
    # Init path
    pkmn_path <- c()
    for (i in 1:(length(path)-1)) {
      print(i)
      # Get Pokemon in group for each step in path
      group1 <- path[[i]]
      group2 <- path[[i+1]]
      pkmn_in_group <- df[(df$EggGroupI == group1 & df$EggGroupII ==  group2)
                          | (df$EggGroupI == group2 & df$EggGroupII ==  group1), 'Pokemon']
      # Add to path
      pkmn_path <- c(pkmn_path, list(pkmn_in_group))
    }
    # Add pkmn to path
    all_pkmn_path <- c(all_pkmn_path, list(pkmn_path))
  }
  return(all_pkmn_path)
}

# Returns breeding egg groups and steps as human readable text
return_steps_as_text <- function(df, start_pkmn, finish_pkmn, hidden_ability = NULL) {
  
  # Get egg group paths
  egg_group_lists <- get_shortest_path(df, start_pkmn, finish_pkmn, hidden_ability)
  if (is.null(egg_group_lists)) {
    final_UI_string <- paste0("<div class='container'>
                              <div class='row'>
                              <h2>No path between ", start_pkmn, " and ",
                              finish_pkmn, " exists.</h2>
                              </div>
                              <div class='row'>
                              <p>Your search criteria was probably too specific.</p>
                              </div>
                              </div>")
    return (final_UI_string)
  } else if ('direct' %in% egg_group_lists) {
    final_UI_string <- paste0("<div class='container'>
                              <div class='row'>
                              <h2>", start_pkmn, " and ",
                              finish_pkmn, " are directly compatible.</h2>
                              </div>
                              </div>")
    return (final_UI_string)
  } else {
    # Get pkmn by egg group paths
    pkmn_lists <- return_breeding_steps(df, egg_group_lists, start_pkmn, finish_pkmn)
    print(egg_group_lists)
    print(pkmn_lists)
    
    # Get minimum number of breeding steps
    actual_steps <- length(pkmn_lists[[1]])
    length_of_list <- length(egg_group_lists[[1]])
    print(actual_steps)
    print(length_of_list)
    
    # Init HTML string for UI
    final_UI_string <- "<div class='container'>"
    
    # Iterate to generate text
    for (i in 1:length(egg_group_lists)) {
      text_egg_group_steps <- paste(egg_group_lists[[i]][(length_of_list-actual_steps):length_of_list], collapse=' -> ')
      text_egg_group_steps <- paste0("Path ", i, ": ", text_egg_group_steps)
      final_UI_string <- paste0(final_UI_string, "<div class='row'><h2>", text_egg_group_steps, "</h2></div>")
      for (j in 1:length(pkmn_lists[[i]])) {
        pkmn_sublist <- paste(pkmn_lists[[i]][[j]], collapse=', ')
        pkmn_sublist <- paste0("<div class='row'><h3>Step ", j, ": Breed into..</h3></div><div class='row'><p>", pkmn_sublist, "</p></div>")
        print(pkmn_sublist)
        final_UI_string <- paste0(final_UI_string, pkmn_sublist)
      }
    }
    
    final_UI_string <- paste0(final_UI_string, "</div>")  
  }
  
  return (final_UI_string)
}

#df <- read.csv('pokemon.csv')

#result <- return_steps_as_text(df, 'Charizard', 'Pidgeotto', hidden_ability="")
#print(result)

#result <- return_steps_as_text(df, 'Gloom', 'Koffing', hidden_ability = 'Stench')
#print(result)