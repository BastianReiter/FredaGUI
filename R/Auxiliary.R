
#===============================================================================
# AUXILIARY FUNCTIONS within FredaApp package
#===============================================================================



#===============================================================================
#' ColorToRGBCSS
#'
#' Turn hexadecimal color code into a string of CSS code of the form 'rgba(r, g, b, a)'.
#' Can be used with vectors.
#'
#' @param Color \code{character} - Vector of hexadecimal color code
#' @param Alpha \code{double} - Optional alpha value vector - Default: 1
#' @param RenderNATransparent \code{logical} - Indicating whether \code{NA} values for \code{Color} result in a totally transparent color
#'
#' @author Bastian Reiter
#-------------------------------------------------------------------------------
ColorToRGBCSS <- function(Color,
                          Alpha = 1,
                          RenderNATransparent = TRUE)
{
  Scalar <- function(color, alpha)
            {
                RGB <- col2rgb(color)
                if (RenderNATransparent == TRUE & is.na(color)) { Alpha <- 0 } else { Alpha <- alpha }      # If color is NA, set alpha value 0 (making resulting color effectively non-existent)
                paste0("rgba(", RGB[["red", 1]], ", ", RGB[["green", 1]], ", ", RGB[["blue", 1]], ", ", Alpha, ")")
            }

  Vectorize(Scalar)(Color, Alpha)
}
#===============================================================================


#===============================================================================
#' ConvertLogicalToIcon
#' @param DataFrame \code{data.frame} or \code{tibble}
#' @return \code{data.frame}
#' @export
#' @author Bastian Reiter
#-------------------------------------------------------------------------------
ConvertLogicalToIcon <- function(DataFrame)
{
  if (!is.null(DataFrame))
  {
      DataFrame %>%
          mutate(across(.cols = where(is.logical),
                        .fns = ~ case_match(.x,
                                            TRUE ~ as.character(shiny.semantic::icon(class = "small green check")),
                                            FALSE ~ as.character(shiny.semantic::icon(class = "small red times")))))
  }
  else { return(NULL) }
}
#===============================================================================


#===============================================================================
#' CreateWaiterScreen
#' @param ID \code{string}
#' @return Waiter object
#' @noRd
#-------------------------------------------------------------------------------
CreateWaiterScreen <- function(ID)
{
  waiter::Waiter$new(id = ID,
                     html = waiter::spin_3(),
                     color = waiter::transparent(.5))
}
#===============================================================================


#===============================================================================
#' DecimalToColor
#' @param Decimal A decimal number
#' @param Type One of 'Change' / 'Completeness' / 'Percentage'
#' @return Hexadecimal color code as string
#' @noRd
#-------------------------------------------------------------------------------
DecimalToColor <- function(Decimal,
                           Type = "Change")
{
  if (is.na(Decimal)) { return(NA) }

  Color <- "#000000"

  if (Type == "Change")
  {
      if (Decimal == 0) { return("#FFFFFF") }
      if (Decimal < 0) { ColorFunction <- grDevices::colorRampPalette(c("#EFD5DF", "#B03060")) }
      if (Decimal > 0) { ColorFunction <- grDevices::colorRampPalette(c("#CDDAE9", "#054996")) }

      Palette <- ColorFunction(101)      # Create a vector of 101 colors to assure valid indexing (s. below)
      Color <- Palette[round(100 * abs(Decimal), digits = 0) + 1]      # '+1' is necessary to avoid invalid indexing (the term in the []-brackets can create 101 different integer numbers)
  }

  if (Type == "Completeness")
  {
        # Convert decimal numbers between 0 and 1 into hexadecimal color codes ranging on a defined color palette
        ColorFunction <- grDevices::colorRampPalette(c("#B03060", "#FFD700", "#016936"))      # Use base-function grDevices::colorRampPalette to create function that allows mapping to hexadecimal codes on a palette of defined color points
        Palette <- ColorFunction(101)      # Create a vector of 101 colors to assure valid indexing (s. below)
        Color <- Palette[round(100 * abs(Decimal), digits = 0) + 1]      # '+1' is necessary to avoid invalid indexing (the term in the []-brackets can create 101 different integer numbers)
  }

  return(Color)
}
#===============================================================================


#===============================================================================
#' MessageToHtml
#'
#' Turn message into HTML
#'
#' @param message \code{character vector} of length 1 with optional name
#' @export
#-------------------------------------------------------------------------------
MessageToHtml <- function(message)
#-------------------------------------------------------------------------------
{
  assert_that(is.vector(message))

  MessageClass <- names(message)
  if (is.null(MessageClass)) { MessageClass <- "Info" }
  MessageClassTypeDetails <- FALSE

  MessageIndent <- ""
  MessageStyle <- c("font-size: 0.8em")

  HtmlMessage <- shiny::br(shiny::span(""))

  if (MessageClass == "Topic")
  {
      HtmlMessage <- div(class = "ui horizontal divider", as.character(message))
  }
  else
  {
      if (MessageClass == "Subtopic")
      {
          MessageStyle <- c(MessageStyle, "font-weight: bold", "text-decoration-line: underline")
      }

      if (str_starts(MessageClass, "Details."))
      {
          MessageIndent <- "   - "
          MessageStyle <- c(MessageStyle, "color: #595959")
          MessageClass <- str_remove(MessageClass, "Details.")
      }

      if (MessageClass == "Special")
      {
          MessageStyle <- c(MessageStyle, "font-weight: bold")
      }

      IconClass <- case_when(MessageClass == "Info" ~ "blue info circle",
                             MessageClass == "Success" ~ "green check circle",
                             MessageClass == "Warning" ~ "orange exclamation triangle",
                             MessageClass == "Failure" ~ "red times circle",
                             MessageClass == "Special" ~ "star",
                             .default = "none")

      HtmlMessage <- shiny::br(shiny::span(style = paste0(MessageStyle, collapse = "; "),
                                           shiny.semantic::icon(class = IconClass),
                                           paste0(MessageIndent, as.character(message))))
  }

  return(HtmlMessage)
}
#===============================================================================


#===============================================================================
#' Reactable.ConditionalCellStyle
#' @param Value The cell value
#' @param Type One of 'Change' / 'Completeness'
#' @return list of CSS properties
#' @noRd
#-------------------------------------------------------------------------------
Reactable.ConditionalCellStyle <- function(Value,
                                           Type = "Change")
{
  if (is.na(Value)) { return(NULL) }

  Color <- DecimalToColor(Decimal = Value, Type = Type)

  return(list(#background = ColorToRGBCSS(Color = Color, Alpha = 0.4),
              background = Color,
              color = "#000000"))
}

#===============================================================================


#===============================================================================
#' SafeDS
#' @noRd
#-------------------------------------------------------------------------------
SafeDS <- function(Expression)
{
  tryCatch(expr = Expression,
           error = function(e)
                   {
                      structure(list(error = TRUE,
                                     msg = conditionMessage(e)),
                                class = "dsFail")
                   })
}
#===============================================================================


#===============================================================================
#' ShowDSError
#' @noRd
#-------------------------------------------------------------------------------
ShowDSError <- function()
{
  showNotification(ui = "Server temporarily unavailable (Bad Gateway). Please repeat the action.",
                   duration = NULL,
                   type = "error")
}
#===============================================================================
