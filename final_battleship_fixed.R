# input

n_col <- as.numeric(readline("How many column in combat area? "))
n_row <- as.numeric(readline("How many row in combat area? "))

n5 <- as.numeric(readline("How many battleships? "))
n4 <- as.numeric(readline("How many cruisers? "))
n3 <- as.numeric(readline("How many destroyers? "))
n2 <- as.numeric(readline("How many submarines? "))



# plot:
# do i need : # keep user's par settings 


gameplot<-function(n_row = 10, n_col = 10) {
 
  # plot an empty combat area
  
  par(xaxs = "i", yaxs = "i") # These arguments prevent the adding of extra space at the axis intervals
  plot.new()
  plot.window(xlim=c(0,n_col), ylim=c(0,n_row)) # the coordinate system
  grid(nx = n_col, ny = n_row, col = "grey")
  box() # a box around it
  axis(3, seq(1,n_col),las=1, cex=0.8, font = 2, padj = 1, hadj = 2, tick = FALSE)
  axis(2, seq(1,n_row,1),las=2, labels = toupper(letters[n_row:1]), font=2, padj = 2.5, tick = FALSE)
  text(rep(1:n_col, n_row), rep(1:n_row, each = n_col), col = "lightgray") ### check whether i should keep this code line
  
}

gameplot(n_row, n_col)




# function to get all possible postions for each ship:

get_possible_positions <- function(board, direction, count){
  # creat a list to save all possible positions. Initially, this list is empty.
  
  list <- as.list(c())
  
  # if direction == 0, horizontal (CHECKED)
  
  if (direction == 0) {
    for (i in 1:n_row){
      for(j in 1:n_col){
        if (((j + length[count] - 1) <= n_col) && (all(board[i,j:(j + length[count] - 1)] == 0) == TRUE)) {
          list[[length(list)+1]] <- c(0, length[count], i, j)
        } else {
          j <- j + 1
        }
      }
    }
    return(list)
  }
  
  # if direction == 1, vertical: (CHECKED)
  if (direction == 1) {
    for (j in 1:n_col){
      for(i in 1:n_row){
        if (((i + length[count] - 1) <= n_row) && (all(board[i:(i+length[count]-1),j] == 0) == TRUE)) {
          list[[length(list)+1]] <- c(1, length[count], i, j)
        } else {
          i <- i + 1
        }
      }
    }
    return(list)
  }
}




# main function:

combat_area <- function(n_row = 10, n_col = n_row, n5 = 1, n4 = 2, n3 = 3, n2 = 4){
  # keep user's par settings 
  par_original <- par(no.readonly = TRUE)
  on.exit(par(par_original))
  
  # plot empty playing field
  gameplot(n_row,n_col)
  
  # initialization
  count <- 1

  length <- c(rep(5, n5), rep(4, n4), rep(3, n3), rep(2, n2))
  colorset<- c(rep("purple", n5), rep("red", n4), rep("orange", n3), rep("green", n2))
  
  board <- matrix(0, nrow = n_row, ncol= n_col)
 
  
  
  while (count <= sum(n5, n4, n3, n2)){
  
  direction <- sample(c(0, 1), 1) #0 horizontal, 1 vertical  (check whether you put it inside or outside while loop)
  
  # get all possible positions for each ship:  
  all_possible_positions <- get_possible_positions(board = board, direction = direction, count = count)
  
  # consider different scenerios for the result of all_possible_positions
  # and then choose ramdomly 1 posible postion 
  if(length(all_possible_positions) == 0){
    new_direction <- (1 - direction) # different direction
    all_possible_positions <- get_possible_positions(direction = new_direction, board = board, count = count)
    if(length(all_possible_positions) == 0) {
      stop ("Error in combat_area(): Ship of length 2 does not have enough space.")
    } else {
      pick_one <- unlist(all_possible_positions[[1]])
      return(pick_one)
    }
  } else {
    pick_one <- unlist(sample(all_possible_positions, 1))
    return(pick_one)
  }
  
  #note: pick_one[1]: direction, pick_one[2]: length, pick_one[3]: row, pick_one[4]: column
  
  # detail choosen column(s) and row(s)
  if (direction == 0){
    choosen_col_indices <- pick_one[4]:(pick_one[4] + pick_one[2] -1)
    choosen_row_indices <- pick_one[3]
  } else {
    choosen_col_indices <- pick_one[4]
    choosen_row_indices <- pick_one[3]:(pick_one[3] + pick_one[2] - 1)
  }
  
  # put the ship in the plot:
  if (direction == 0){
    points(choosen_col_indices, rep(choosen_row_indices, length[count]), cex=3, col=colorset[count]) 
  } else {
    points(rep(choosen_col_indices,length[count]), choosen_row_indices, cex=3, col=colorset[count]) 
  }
  
  
  # question: notice the difference between function points (x- column, y-row) and board[row, column]
  
  # update the board
  
  #1 the positions of the already placed ships:
  board[choosen_row_indices, choosen_col_indices] <- "NA"
  
  #2 remove places which touch the ships horizontally or vertically (but not touch diagonally)
  
  if ((choosen_row_indices + 1)<= n_row){
    board [choosen_row_indices + 1, choosen_col_indices] <- "NA"
  }
  if ((choosen_row_indices - 1) >= 1) {
    board [choosen_row_indices -1, choosen_col_indices] <- "NA"
  }
  
  if ((choosen_col_indices + 1)<= n_col){
    board [choosen_row_indices, choosen_col_indices+1] <- "NA"
  }
  if ((choosen_col_indices - 1) >= 1) {
    board [choosen_row_indices, choosen_col_indices -1] <- "NA"
  }
  
  # update count:
  count <- count + 1
    
  }
}



combat_area()





