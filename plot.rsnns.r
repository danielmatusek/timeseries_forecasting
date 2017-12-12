plot.rsnns <- function(x)
{
  netInfo <<- extractNetInfo(x)
  unitDefinitions <- netInfo$unitDefinitions
  posX <- unitDefinitions$posX
  posY <- unitDefinitions$posY
  numNeurons <- length(posX)
  
  maxX <- max(unitDefinitions$posX) + 1
  maxY <- max(unitDefinitions$posY) + 1
  
  radius <- 1 / max(maxX, maxY) / 2
  
  grid::grid.newpage()
  
  for(i in 1:numNeurons)
  {
    grid::grid.circle(x = posX[i]/maxX, y = posY[i]/maxY, r = radius, gp = grid::gpar(fill = 'white', col = 'black'))
    
    for(j in 1:numNeurons)
    {
      if (netInfo$fullWeightMatrix[i, j] != 0)
      {
        x = c(posX[i] / maxX + radius/2, posX[j] / maxX - radius/2)
        y = c(posY[i], posY[j]) / maxY
        grid::grid.lines(x = x, y = y, 
          arrow = grid::arrow(length = grid::unit(0.15, "cm"), type = "closed"), gp = grid::gpar(fill = 'black', col = 'black'))
        grid::grid.text(label = netInfo$fullWeightMatrix[i, j], x = x[1]*0.7+x[2]*0.3, y = y[1]*0.7+y[2]*0.3+0.05)
      }
    }
  }
}
