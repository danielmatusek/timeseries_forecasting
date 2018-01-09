plot.rsnns <- function(x, labels = NULL)
{
  nn <<- x
  netInfo <<- extractNetInfo(x)
  unitDefinitions <- netInfo$unitDefinitions
  types <- unitDefinitions$type
  
  if (inherits(x, 'mlp'))
  {
    posX <- unitDefinitions$posY + 1
    posY <- unitDefinitions$posX
  }
  else
  {
    posX <- unitDefinitions$posX
    posY <- unitDefinitions$posY
  }
  numNeurons <- length(posX)
  
  if (!is.null(labels))
  {
    if (class(labels) != 'character' || length(labels) != x$nInputs)
    {
      labels <- NULL
    }
  }
  
  
  normalY <- c()
  contextY <- c()
  xToMaxY <- list(NULL)
  
  for (i in 1:numNeurons)
  {
    if (types[i] == 'UNIT_SPECIAL_H')
    {
      contextY <- c(contextY, posY[i])
    }
    else
    {
      normalY <- c(normalY, posY[i])
      
      key <- paste('x', posX[i])
      if (is.null(xToMaxY[[key]]))
      {
        xToMaxY[[key]] <- posY[i]
      }
      else if (posY[i] > xToMaxY[[key]])
      {
        xToMaxY[[key]] <- posY[i]
      }
    }
  }
  
  maxX <- max(posX) + 1
  
  normalYWidth <- max(normalY) - min(normalY)
  if (length(contextY) > 0)
  {
    minContextY <- min(contextY)
    contextYWidth <- max(contextY) - minContextY
    
    maxY <- normalYWidth + contextYWidth + 3
  }
  else
  {
    maxY <- normalYWidth + 1
  }
  
  radius <- 1 / max(maxX, maxY) / 2
  
  grid::grid.newpage()
  
  neurons <- lapply(1:numNeurons, function(i) {
    x <- posX[i]/maxX
    y <- posY[i]/maxY
    
    if (types[i] == 'UNIT_SPECIAL_H')
    {
      name <- gsub('con', 'hid', unitDefinitions$unitName[i])
      
      for (j in 1:numNeurons)
      {
        if (unitDefinitions$unitName[j] == name)
        {
          x <- posX[j] / maxX
          break
        }
      }
      
      y <- 1 + normalYWidth + (posY[i] - minContextY) * 1.5
    }
    else
    {
      y <- (posY[i] - 1 + 0.5) / xToMaxY[[paste('x', posX[i])]] * normalYWidth + 0.5
    }
    y <- y / maxY
    
    grid::grid.circle(x = x, y = y, r = radius, gp = grid::gpar(fill = 'white', col = 'black'))
    
    if (types[i] == 'UNIT_INPUT' && !is.null(labels))
    {
      grid::grid.text(label = labels[i], x = x, y = y, gp = grid::gpar(fontsize = 10))
    }
    
    c(x, y)
  })
  
  for(i in 1:numNeurons)
  {
    from <- neurons[[i]]
    fromIsContext <- types[i] == 'UNIT_SPECIAL_H'
    
    for(j in 1:numNeurons)
    {
      if (netInfo$fullWeightMatrix[i, j] != 0)
      {
        to <- neurons[[j]]
        toIsContext <- types[j] == 'UNIT_SPECIAL_H'
        label <- round(netInfo$fullWeightMatrix[i, j], digits = 4)
        
        if (i == j)
        {
          grid::grid.bezier(x = c(from[1] - radius, from[1] - radius - 0.1, to[1] + radius + 0.1, to[1] + radius),
            y = c(from[2], from[2]+0.1, to[2]+0.1, to[2]),
            gp = grid::gpar(fill = "blue", col = 'blue'), arrow = arrow(length = grid::unit(0.15, "cm"), type = "closed"))
          grid::grid.text(label = label, x = from[1], y = from[2] + 0.07, just = 'top', gp = grid::gpar(fontsize = 10, col = 'blue'))
        }
        else
        {
          if (fromIsContext)
          {
            deltaX <- posY[j] * 0.01
            grid::grid.lines(x = c(from[1] - radius, from[1] - radius - deltaX, to[1] - radius - deltaX, to[1] - radius),
              y = c(from[2], from[2], to[2] + 0.05, to[2]),
              arrow = grid::arrow(length = grid::unit(0.15, "cm"), type = "closed"), gp = grid::gpar(fill = 'blue', col = 'blue'))
            #grid::grid.text(label = label, x = from[1]*0.7+to[1]*0.3,
            #  y = from[2]*0.7+to[2]*0.25 + 0.2 / maxY, gp = grid::gpar(fontsize = 10))
          }
          else if (toIsContext)
          {
            deltaX <- posY[i] * 0.01
            grid::grid.lines(x = c(from[1] + radius, from[1] + radius + deltaX, to[1] + radius + deltaX, to[1] + radius),
              y = c(from[2], from[2] + 0.05, to[2], to[2]),
              arrow = grid::arrow(length = grid::unit(0.15, "cm"), type = "closed"), gp = grid::gpar(fill = 'blue', col = 'blue'))
            grid::grid.text(label = label, x = from[1] + radius + deltaX + 0.005,
             y = from[2] + 0.06, just = c('right', 'bottom'), rot = -90, gp = grid::gpar(fontsize = 10, col = 'blue'))
          }
          else
          {
            deltaY <- posY[j] - xToMaxY[[paste('x', posX[j])]] / 2 - 0.5
            deltaY <- 0.02 * deltaY
            grid::grid.lines(x = c(from[1] + radius, from[1] + radius + 0.01, from[1] + radius + 0.08, to[1] - radius),
              y = c(from[2], from[2] + deltaY, from[2] + deltaY, to[2]), arrow = grid::arrow(length = grid::unit(0.15, "cm"),
              type = "closed"), gp = grid::gpar(fill = 'black', col = 'black'))
            grid::grid.text(label = label, x = from[1] + radius + 0.08, y = from[2] + deltaY + 0.003, just = c('right', 'bottom'),
              gp = grid::gpar(fontsize = 10))
          }
        }
      }
    }
  }
}
