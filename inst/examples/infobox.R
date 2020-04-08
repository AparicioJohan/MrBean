v1 <- bsPopover(id="heritability", title = "Heritability", 
          content = " ", 
          trigger = "hover", 
          placement = "top",
          options = list(container="body"))

v2 <- bsPopover(id="maxline", title = "Residual", 
                content = "Residual standard deviation", 
                trigger = "hover", 
                placement = "top",
                options = list(container="body"))

v3 <- bsPopover(id="minline", title = "R-square", 
                content = "Proportion of variance explained", 
                trigger = "hover", 
                placement = "top",
                options = list(container="body"))

v4 <- bsPopover(id="cv", title = "CV", 
                content = "Coefficient of variation of BLUPs", 
                trigger = "hover", 
                placement = "top",
                options = list(container="body"))

v5 <- bsPopover(id="q1", title = "Required", 
                content = "Phenotype is necessary", 
                trigger = "hover", 
                placement = "right",
                options = list(container="body"))

v6 <- bsPopover(id="q2", title = "Required", 
                content = "Genotype is necessary", 
                trigger = "hover", 
                placement = "right",
                options = list(container="body"))

v7 <- bsPopover(id="q3", title = "Required", 
                content = "Column Coord", 
                trigger = "hover", 
                placement = "right",
                options = list(container="body"))

v8 <- bsPopover(id="q4", title = "Required", 
                content = "Row Coord", 
                trigger = "hover", 
                placement = "right",
                options = list(container="body"))