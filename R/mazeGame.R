#'@title Maze Game
#'
#'@description Maze game, you can set the size by yourself.
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
  block_map = matrix(0, size, size)
  maze_map = matrix(1, size2, size2)
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
  #
  in_map <- function(x, y){
    return((1<=x) && (x<=size) && (1<=y) && (y<=size))
  }
  #
  connect <- function(x, y, xx, yy){
    maze_map[(x+xx)-2, (y+yy)-2] <<- 0
  }
  #
  neighbor_count <- function(x, y){
    temp = 0
    for(i in 1:4){
      if(in_map(x + move[[i]][1], y + move[[i]][2])){
        if(block_map[x + move[[i]][1], y + move[[i]][2]] == 1){
          temp = temp + 1
        }
      }
    }
    return(temp)
  }

  dfs <- function(x, y){
    print(c(x, y, neighbor_count(x, y)))
    if(neighbor_count(x, y) == 4){return}
    direction = c(FALSE, FALSE, FALSE, FALSE)
    while(neighbor_count(x, y) < 4){
      temp = -1

      while(temp == -1 || direction[temp] == TRUE ){
        temp = sample(1:4, 1)
      }

      xx=x+move[[temp]][1]
      yy=y+move[[temp]][2]

      if(in_map(xx, yy) && block_map[xx, yy] == 0){
        block_map[xx, yy] <<- 1
        connect(x, y, xx, yy)
        dfs(xx, yy)

        direction[temp]=TRUE
        if(neighbor_count(x, y) == 4){
          return
        }
      }
    }
    return
  }
  # Set the starting point and start generating the map matrix
  block_map[2, 2] = 1
  dfs(2, 2)

  # generate a map matrix
  windows()
  plot(0,0,xlim=c(0, size2),ylim=c(0, size2),type='n',xaxs="i", yaxs="i")
  for(i in 1:(size2 -1)){
    abline(h=i,col="gray60") # Horizontal line
    abline(v=i,col="gray60")
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
  dest.x = size2 - 1 #start point
  dest.y = size2 - 1 #end point
  points(now.x-0.5, now.y-0.5, col = 2, pch = 15, cex = cex_set)
  points(dest.x-0.5, dest.y-0.5, col = 6, pch = 15, cex = cex_set)

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

    if(now.x == dest.x && now.y == dest.y){
      text(4, 4, label="You Win", cex = 2 , )
      getGraphicsEvent(onKeybd = NULL)
    }
  }

  getGraphicsEvent(onKeybd = keydown)

}



