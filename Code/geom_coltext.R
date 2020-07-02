## Functions based on https://github.com/STAT-UP/statupinternal, GPL-2 licence

#' Get Brightness of a Color
#'
#' This function tries to return how "bright" (i.e. perceived luminance) a given
#' color is based on psychological evaluations at the dawn of the color TV era.
#'
#' This takes into account that the human eye is most sensitive to green,
#' followed by red with blue colors appearing darker.
#'
#' Everything is stolen from stackoverflow so don't sue me.
#'
#' @param .colors Vector or list with colors.This is passed to `col2rgb` and
#'                so accepts color names ("black"), hexadecimal rgb strings
#'                ("#rrggbb" or "#rrggbbaa") or positive integers (`i` meaning
#'                `palette()[i]`).
#'
#' @return A vector of brightness values between 0 and 1. 0 meaning black and
#' 1 white.
#'
#' @export
get_brightness <-
  function(.color) {
    colSums(grDevices::col2rgb(.color) / 255 * c(0.299, 0.587, 0.114))
  }

#' Negation of \%between\% Operator
#'
#' R (data.table) lacks a negation of the \%between\% operator. While you can negate the whole
#' expression, that solution results in less readable and clear code.
#'
#' @param .x Numeric vector. Input data
#' @param .lower Numeric value or vector with the lower bound(s)
#' @param .upper Numeric value or vector with the upper bound(s)
#' @param .incbounds `TRUE` means inclusive bounds, i.e. `lower <= .x <= .upper`. `FALSE`
#'                   means exclusive bounds. For \%outside\%
#'                   `.incbounds = TRUE` is used.
#'
#' @return Logical vector of same length as `.x`. `TRUE` if the input value is
#' outside the bounds, `FALSE` otherwise.
#'
#' @examples
#' rnorm(10) %outside% c(-0.5, 0.5)
#'
#' @export
outside <- function(.x, .lower, .upper, .incbounds=TRUE)
{
  !data.table::between(.x, .lower, .upper, .incbounds)
}

#' @rdname outside
#' @export
`%outside%` <- function(.x, .bounds)
{
  outside(.x, .bounds[[1L]], .bounds[[2L]], .incbounds = TRUE)
}

#' @rdname outside
#' @export
`%notbetween%` <- `%outside%`


`%||%` <- ggplot2:::`%||%`
geom_coltext <- function(mapping = NULL,
                         data = NULL,
                         position = "dodge",
                         ...,
                         stat = "identity",
                         width = NULL,
                         center_text = FALSE,
                         just_mirror = TRUE,
                         just_mirror_cutoff = c(0.1, 0.9),
                         fontcolor = c("darkgrey", "grey"),
                         parse = FALSE,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomColtext,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      width = width,
      center_text = center_text,
      just_mirror = just_mirror,
      just_mirror_cutoff = just_mirror_cutoff,
      fontcolor = fontcolor,
      parse = FALSE,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_coltext
#'
#' @export
geom_bartext <- geom_coltext

#' @rdname geom_coltext
#'
#' @import ggplot2
#' @export
GeomColtext <-
  ggplot2::ggproto(
    "GeomColtext",
    ggplot2::GeomRect,
    
    required_aes = c("x", "y"),
    
    default_aes = list(label = NULL,
                       colour = NA,
                       fill = "black",
                       size = 0.5,
                       fontsize = 3.88,
                       linetype = 1,
                       alpha = NA,
                       angle = 0,
                       injust = 1.3,
                       family = "",
                       fontface = 1,
                       lineheight = 1.2),
    
    setup_data = function(self, data, params) {
      data$width <- data$width %||%
        params$width %||% (resolution(data$x, FALSE) * 0.9)
      
      data <-
        transform(
          data,
          ymin = pmin(y, 0), ymax = pmax(y, 0),
          xmin = x - width / 2, xmax = x + width / 2,
          width = NULL
        )
      
      data
    },
    
    draw_panel = function(self, data, params, coord,
                          stat = "identity",
                          width = NULL,
                          na.rm = FALSE,
                          parse = FALSE,
                          center_text = FALSE,
                          just_mirror = TRUE,
                          just_mirror_cutoff = c(0.1, 0.9),
                          fontcolor = c("darkgrey", "grey"))
    {
      ##### > Bar Grob #####
      col_grob <- GeomCol$draw_panel(data, params, coord, width)
      
      ##### > Text Grob #####
      
      ##### >> Small stuff #####
      
      # Add missing labels
      if(is.null(data$label))
        data$label <- as.character(data$y)
      
      # switch font size
      data$size <- data$fontsize
      
      ##### >> Adjust the injust #####
      
      ##### >>> React to coord_flip #####
      if("CoordFlip" %in% class(coord))
      {
        just <- "hjust"
        range <- "x.range"
      } else {
        just <- "vjust"
        range <- "y.range"
      }
      
      ##### >>> Mirror large and small values #####
      if (isTRUE(just_mirror))
      {
        ind <- (
          data$injust < 0.5 &
            data$y %outside% {just_mirror_cutoff[2] * params[[range]]}
        ) | (
          data$injust > 0.5 &
            !data$y %outside% {(just_mirror_cutoff[1]) * params[[range]]}
        )
        
        ind <- which(ind)
        data$injust[ind] <- - data$injust[ind]
        
        # Flip value for negative y
        data$injust <- 0.5 + sign(data$y) * (data$injust - 0.5)
      }
      
      data[[just]] <- data$injust / 2 + 0.5
      
      ##### >> Center Text #####
      
      if(isTRUE(center_text))
      {
        data$y <- (data$ymax + data$ymin) / 2
      }
      
      ##### >> Set text color #####
      
      data$colour <- fontcolor[1]
      
      # This will fail if fill is a variable and not a color
      try({
        ind <- which(sign(data[[just]] - 0.5) == sign(data$y))
        
        if(length(ind) > 0)
        {
          fills <- unique(data$fill[ind])
          bright.fills <- get_brightness(fills)
          bright.colors <- get_brightness(fontcolor)
          
          optimal.colors <-
            sapply(bright.fills,
                   function(.fill)
                     fontcolor[which.max(abs(bright.colors - .fill))])
          
          names(optimal.colors) <- fills
          
          data$colour[ind] <- optimal.colors[data$fill[ind]]
        }
      })
      
      ##### >> Create Text grob #####
      text_grob <- GeomText$draw_panel(data, params, coord, parse, na.rm)
      
      #  browser()
      ##### > Output #####
      ggplot2:::ggname("geom_coltext",
                       grid::grobTree(col_grob,
                                      text_grob))
    }
  )
