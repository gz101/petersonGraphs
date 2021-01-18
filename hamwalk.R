makeVertexDF <- function() {
  DF <- data.frame(V = character(10), x = numeric (10), y = numeric(10), bg = character(10))
  DF$V <- c("RGP","RYB","YGP","RGB","YBP","YGB","BGP","RBP","RYP","RYG")
  DF$x[1:5] <- 2*sin((0:4)*2*pi/5)
  DF$y[1:5] <- 2*cos((0:4)*2*pi/5)
  DF$x[6:10] <- sin((0:4)*2*pi/5)
  DF$y[6:10] <- cos((0:4)*2*pi/5)
  DF$bg <- rep("white",10)
  return(DF)
}

makeEdgeDF <- function() {
  DF <- data.frame(V1 = character(15), V2 = character(15), color = character(15))
  DF$V1 <- c("RGP","RYB","YGP","RGB","YBP","RGP","RYB","YGP","RGB","YBP","RYP","RYG","YGB","BGP","RBP")
  DF$V2 <- c("RYB","YGP","RGB","YBP","RGP","YGB","BGP","RBP","RYP","RYG","YGB","BGP","RBP","RYP","RYG")
  DF$color <- c("red","orange","green","blue","purple","green","blue","purple","red","orange","orange","green","blue","purple","red")
  return(DF)
}

makeCycleEdgeDF <- function() {
  DF <- data.frame(V1 = character(20), V2 = character(20), color = character(20))
  DF$V1 <- c("RGP","RYB","YGP","RGB","YBP","RGP","RYB","YGP","RGB","YBP","RYP","RYG","YGB","BGP","RBP","RGP","YBP","RGB","YGP","RYB")
  DF$V2 <- c("RYB","YGP","RGB","YBP","RGP","YGB","BGP","RBP","RYP","RYG","YGB","BGP","RBP","RYP","RYG","RYG","RYP","RBP","BGP","YGB")
  DF$color <- c("red","orange","green","blue","purple","green","blue","purple","red","orange","orange","green","blue","purple","red","black","black","black","black","black")
  return(DF)
}

findClosestVert <- function(DF, x, y) {
  n <- nrow(DF)
  distsq <- numeric(n)
  for (i in 1:n) {
    distsq[i] <- (DF[i,3] - y) ^ 2 + (DF[i,2] - x) ^ 2
  }
  k <- which(distsq == min(distsq))
  stopifnot(length(k)==1)
  return(k)
}

# dest is index in PeteDF, prev is name string
checkRoad <- function(vertDF, edgeDF, dest, prev) {
  dest.name <- vertDF[dest,1]
  valid.orig1 <- which(edgeDF[,1]==prev)
  valid.orig2 <- which(edgeDF[,2]==prev)
  
  if (length(valid.orig1>0)) {
    for (i in 1:length(valid.orig1)) {
      if (dest.name==edgeDF[valid.orig1[i],2]) return(TRUE)
    }
  }
  if (length(valid.orig2>0)) {
    for (i in 1:length(valid.orig2)) {
      if (dest.name==edgeDF[valid.orig2[i],1]) return(TRUE)
    }
  }
  return(FALSE)
}

# dest is index in PeteDF, prev is index in PeteDF
roadLookUp <- function(vertDF, edgeDF, dest, prev) {
  dest.name <- vertDF[dest,1]
  prev.name <- vertDF[prev,1]
  valid.orig1 <- which(edgeDF[,1]==prev.name)
  valid.orig2 <- which(edgeDF[,2]==prev.name)
  
  if (length(valid.orig1>0)) {
    for (i in 1:length(valid.orig1)) {
      if (dest.name==edgeDF[valid.orig1[i],2]) return(valid.orig1[i])
    }
  }
  if (length(valid.orig2>0)) {
    for (i in 1:length(valid.orig2)) {
      if (dest.name==edgeDF[valid.orig2[i],1]) return(valid.orig2[i])
    }
  }
  return(-1) # FALSE
}

# Checks if all connected vertices to the last user clicked vertex have been visited
# prev is index in PeteDF
resolve <- function(vertDF, edgeDF, prev) {
  prev.name <- vertDF[prev,1] # String literal of last clicked point
  remaining <- which(vertDF$bg!="yellow")
  valid.edge1 <- which(edgeDF[,1]==prev.name) # Valid edges
  valid.edge2 <- which(edgeDF[,2]==prev.name) # Valid edges
  
  for (i in 1:length(valid.edge1)) {
    nextvert <- edgeDF[valid.edge1[i],2]
    vertidx <- which(vertDF[,1]==nextvert)
    if (vertDF[vertidx,4]!="yellow") return(TRUE) # If there are valid vertices
  }
  for (i in 1:length(valid.edge2)) {
    nextvert <- edgeDF[valid.edge2[i],1]
    vertidx <- which(vertDF[,1]==nextvert)
    if (vertDF[vertidx,4]!="yellow") return(TRUE) # If there are valid vertices
  }
  return(FALSE) # No valid vertices left
}

# first is an index in PeteDF
resolve.cycle <- function(vertDF, edgeDF, prev, first) {
  prev.name <- vertDF[prev,1]
  first.name <- vertDF[first,1]
  connected1 <- which(edgeDF[,1]==prev.name)
  connected2 <- which(edgeDF[,2]==prev.name)
  
  for (i in 1:length(connected1)) {
    nextvert <- edgeDF[connected1[i],2]
    vertidx <- which(vertDF[,1]==nextvert)
    if (vertDF[vertidx,1]==first.name) return(TRUE)
  }
  for (i in 1:length(connected2)) {
    nextvert <- edgeDF[connected2[i],1]
    vertidx <- which(vertDF[,1]==nextvert)
    if (vertDF[vertidx,1]==first.name) return(TRUE)
  }
  return(FALSE)
}

# Check that adjacent edges don't have the same color
checkColor <- function(vertDF, edgeDF, vertex, color) {
  vertex.name <- vertDF[vertex,1]
  vert.color <- vertDF[vertex,4]
  
  if (color=="grey" || color=="beige" || color=="pink") {
    connected1 <- which(edgeDF[,1]==vertex.name)
    connected2 <- which(edgeDF[,2]==vertex.name)
    
    for (i in 1:length(connected1)) {
      adjvert <- edgeDF[connected1[i],2]
      vertidx <- which(vertDF[,1]==adjvert)
      if (vertDF[vertidx,4]==color) return(FALSE)
    }
    for (i in 1:length(connected2)) {
      adjvert <- edgeDF[connected2[i],1]
      vertidx <- which(vertDF[,1]==adjvert)
      if (vertDF[vertidx,4]==color) return(FALSE)
    }
  }
  return(TRUE)
}

roadsLeft <- function(PeteDF, edgeDF, roadCycle, vertex) {
  valid.roads <- which(roadCycle==1)
  if (length(valid.roads)==1) return(FALSE)
  vertex.name <- PeteDF[vertex,1]
  connected1 <- which(edgeDF[,1]==vertex.name)
  connected2 <- which(edgeDF[,2]==vertex.name)
  
  for (i in 1:length(valid.roads)) {
    if (valid.roads[i] %in% connected1 || valid.roads[i] %in% connected2) return(TRUE)
  }
  return(FALSE)
}

# roadCycle <- c(1,1,1,2,2,2,2,2,1,1,1,1,2,1,1,2,2,2,2,2)
# PeteDF <- makeVertexDF()
# edgeDF <- makeEdgeDF()
# roadsLeft(PeteDF, edgeDF, roadCycle, 1)
# findClosestVert(PeteDF, 0, 2)
# checkRoad(PeteDF, edgeDF, 1, "RYB")
# checkRoad(PeteDF, edgeDF, 2, "RGP")
# checkRoad(PeteDF, edgeDF, 1, "YGB")
# checkRoad(PeteDF, edgeDF, 6, "RGP")
# checkRoad(PeteDF, edgeDF, 1, "YBP")
# checkRoad(PeteDF, edgeDF, 5, "RGP")
# roadLookUp(PeteDF, edgeDF, 1, 2)
# roadLookUp(PeteDF, edgeDF, 2, 1)
# roadLookUp(PeteDF, edgeDF, 1, 6)
# roadLookUp(PeteDF, edgeDF, 6, 1)
# roadLookUp(PeteDF, edgeDF, 4, 9)
# roadLookUp(PeteDF, edgeDF, 9, 4)
# resolve(PeteDF, edgeDF, 1)
# resolve(PeteDF, edgeDF, 3)
# resolve(PeteDF, edgeDF, 7)
# resolve.cycle(PeteDF, edgeDF, 1, 2)
# checkColor(PeteDF, edgeDF, 1)
