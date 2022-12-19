#' @title Maze Game
#'
#' @description Maze game, you can set the size by yourself.
#'
#' @param size total maze size
#' @param cex_set each cell size
#'
#' @return
#' @export
#'
#' @examples
#' maze(25,2.5)
#'
maze <- function(size, cex_set = 2.5){
  # 1 is wall，0 is path

  # build the maze structure
  size2 = 2*size -3
  block_map = matrix(0, size, size) # mark the start and end point
  maze_map = matrix(1, size2, size2) #mark the maze inside structure
  for(i in 1:(size-2)){
    for(j in 1:(size-2)){
      maze_map[2*i, 2*j]=0
    }
  }
  #maze 4 enclosures
  block_map[1, ] = 1
  block_map[size, ] = 1
  block_map[, 1] = 1
  block_map[, size] = 1

  #Set moving operation: "up, down, left and right"
  move=list(c(-1, 0), c(1, 0), c(0, -1), c(0, 1))
  #Check for out-of-bounds
  bound <- function(x, y){
    return((1<=x) && (x<=size) && (1<=y) && (y<=size))
  }
  #connect each path
  connect_path <- function(x, y, xxx, yyy){
    maze_map[(x+xxx)-2, (y+yyy)-2] <<- 0
  }
  #count the neighbors
  neighbors <- function(x, y){
    temp = 0
    for(i in 1:4){
      if(bound(x + move[[i]][1], y + move[[i]][2])){
        if(block_map[x + move[[i]][1], y + move[[i]][2]] == 1){
          temp = temp + 1
        }
      }
    }
    return(temp)
  }
  # main function
  dfs <- function(x, y){
    print(c(x, y, neighbors(x, y)))
    if(neighbors(x, y) == 4){return}
    direction = c(FALSE, FALSE, FALSE, FALSE)
    while(neighbors(x, y) < 4){
      temp = -1
      while(temp == -1 || direction[temp] == TRUE ){
        temp = sample(1:4, 1)
      }

      xxx=x+move[[temp]][1]
      yyy=y+move[[temp]][2]

      if(bound(xxx, yyy) && block_map[xxx, yyy] == 0){
        block_map[xxx, yyy] <<- 1
        connect_path(x, y, xxx, yyy)
        dfs(xxx, yyy)

        direction[temp]=TRUE
        if(neighbors(x, y) == 4){
          return
        }
      }
    }
    return
  }
  # Set the start point and generate the maze map
  block_map[2, 2] = 1
  dfs(2, 2)
  # generate a map matrix
  windows()
  plot(0,0,xlim=c(0, size2),ylim=c(0, size2),type='n',xaxs="i", yaxs="i")
  for(i in 1:(size2 -1)){
    abline(h=i,col="gray60") # Horizontal line
    abline(v=i,col="gray60") # Vertical line
  }
  abline(h=size2)
  abline(v=size2)
  for(i in 1:size2){
    for(j in 1:size2){
      if(maze_map[i, j]==1){
        points(i-0.5, j-0.5, col = 8, pch = 15, cex = cex_set)
      }
      else{
        points(i-0.5, j-0.5, col = 7, pch = 15, cex = cex_set)
      }
    }
  }

  now.x = 2
  now.y = 2
  end.x = size2 - 1
  end.y = size2 - 1
  points(now.x-0.5, now.y-0.5, col = 2, pch = 15, cex = cex_set)
  points(end.x-0.5, end.y-0.5, col = 6, pch = 15, cex = cex_set)
  #set the keyboard moving
  keydown<-function(K){
    K = tolower(K)
    print(K)
    if(K == "down"){
      if(now.y > 2 && maze_map[now.x, now.y-1] == 0){
        points(now.x-0.5, now.y-0.5, col = 7, pch = 15, cex = cex_set)
        now.y <<- now.y - 1
        points(now.x-0.5, now.y-0.5, col = 2, pch = 15, cex = cex_set)
      }
    }
    if(K == "up"){
      if(now.y < size2 - 1 && maze_map[now.x, now.y+1] == 0){
        points(now.x-0.5, now.y-0.5, col = 7, pch = 15, cex = cex_set)
        now.y <<- now.y + 1
        points(now.x-0.5, now.y-0.5, col = 2, pch = 15, cex = cex_set)
      }
    }
    if(K == "left"){
      if(now.x > 2 && maze_map[now.x - 1, now.y] == 0){
        points(now.x-0.5, now.y-0.5, col = 7, pch = 15, cex = cex_set)
        now.x <<- now.x - 1
        points(now.x-0.5, now.y-0.5, col = 2, pch = 15, cex = cex_set)
      }
    }
    if(K == "right"){
      if(now.x < size2 - 1 && maze_map[now.x + 1, now.y] == 0){
        points(now.x-0.5, now.y-0.5, col = 7, pch = 15, cex = cex_set)
        now.x <<- now.x + 1
        points(now.x-0.5, now.y-0.5, col = 2, pch = 15, cex = cex_set)
      }
    }

    if(now.x == end.x && now.y == end.y){
      text(4, 4, label="You Win", cex = 2 )
      getGraphicsEvent(onKeybd = NULL)
    }
  }
  getGraphicsEvent(onKeybd = keydown)
}



