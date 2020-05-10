#' CAIM Color Palette
#' @export
caim_colors <- function(color=NA, alpha=1) {
  colormtx <- matrix(c(36, 0, 132), nrow=1,
                     dimnames=list("darkblue", c("r", "g", "b")))
  colormtx <- rbind(colormtx, "darkred"=c(160, 0, 46))
  colormtx <- rbind(colormtx, "ash"=c(156, 155, 155))
  colormtx <- rbind(colormtx, "ashblue"=c(73, 98, 108))
  colormtx <- rbind(colormtx, "sapphire"=c(42, 70, 108))
  colormtx <- rbind(colormtx, "darkteal"=c(2, 52, 90))
  colormtx <- rbind(colormtx, "darkblue2"=c(16, 9, 79))
  colormtx <- rbind(colormtx, "darkred2"=c(97, 0, 28))
  colormtx <- rbind(colormtx, "ash2"=c(94, 93, 93))
  colormtx <- rbind(colormtx, "ashblue2"=c(44, 59, 64))
  colormtx <- rbind(colormtx, "sapphire2"=c(20, 44, 107))
  colormtx <- rbind(colormtx, "darkteal2"=c(0, 32, 53))

  if (is.na(color[1])) { # default, return the palette so drawing functions can cycle through it
    # return(cbind(colormtx/255, alpha))
    return(rgb(colormtx/255, alpha=alpha, names=row.names(colormtx)))
  } else if (is.numeric(color)) {
    numcolors <- nrow(colormtx)
    cix <- (((color-1) %% numcolors) + 1)
    return(rgb(colormtx[cix, ,drop=F]/255, alpha=alpha, names=row.names(colormtx)[cix]))
  } else {
    colormtx <- rbind(colormtx, "white"=c(255, 255, 255))
    colormtx <- rbind(colormtx, "black"=c(0, 0, 0))
    colormtx <- rbind(colormtx, "grey"=c(99, 102, 106))
    colormtx <- rbind(colormtx, "gray"=c(99, 102, 106))
    colormtx <- rbind(colormtx, "orange"=c(237, 139, 0))
    colormtx <- rbind(colormtx, "green"=c(0, 162, 42))
    colormtx <- rbind(colormtx, "yellow"=c(250, 200, 45))
    colormtx <- rbind(colormtx, "red"=c(226, 0, 0))
    colormtx <- rbind(colormtx, "lightash"=c(228, 228, 228))

    colormtx <- rbind(colormtx, "light1"=c(255, 255, 255))
    colormtx <- rbind(colormtx, "dark1"=c(88, 88, 88))
    colormtx <- rbind(colormtx, "light2"=c(156, 155, 155))
    colormtx <- rbind(colormtx, "dark2"=c(2, 52, 90))

    return (rgb(colormtx[color, ,drop=F]/255, alpha=alpha)) #, names=color))
  }

}

#' Heatmap color
#' @export
heatmap_color <- function(
  x
  , range = c(-1, 1)
  , color_lo = caim::caim_colors("darkblue")
  , color_mid = caim::caim_colors("white")
  , color_hi = caim::caim_colors("darkred")
) {
  cr <- colorRamp(c(color_lo, color_mid, color_hi))
  return(cr((x - range[1]) / (range[2] - range[1])))
}

#' format number to text
#' @export
#' @param num number or vector of numbers to format
#' @param fmt format to apply. Default = c("pct", 2). Acceptable formats = "pct", "dec", "dk"
#'  (divided by 1000)
#' @examples
#' number_format(.0023, c("pct", 2))
#' number_format(c(.016784, .002), c("dec", 4))
#' number_format(1234567.89, c("dk", 0))
number_format <- function(num, fmt=c("pct", 2)) {
  if (fmt[1]=="pct")
    fnum <- sprintf(round(100*num,as.numeric(fmt[2])),fmt=paste("%1.",fmt[2],"f%%",sep=""))
  if (fmt[1]=="dec")
    fnum <- sprintf(num,fmt=paste("%1.",fmt[2],"f",sep=""))
  if (fmt[1]=="dk") # divided by 1000
    fnum <- sprintf(round(num/1000,as.numeric(fmt[2])),fmt=paste("%1.",fmt[2],"f",sep=""))
  return(fnum)
}

#' Main chart plot function
#' @export
chart_plot <- function(xdata=c(0,1),
                       ydata=c(0,1),
                       revx=F,
                       bgcolor=caim::caim_colors("lightash"),
                       textcolor=caim::caim_colors("dark1"),
                       showxvalues=T,
                       showyvalues=T,
                       showxaxis=F,
                       showyaxis=F,
                       showgrid=T,
                       xat=NULL,
                       yat=NULL,
                       xfmt=c("dec", 2), #e.g. c("dec",2) "dec" "pct" "dk" and "txt" supported
                       yfmt=c("dec", 2),
                       xaxistext=NULL,
                       yaxistext=NULL,
                       source=NULL,
                       xlab=NA,
                       ylab=NA,
                       # las=0,
                       ...) {

  mar_bottom <- 1
  if (showxvalues)
    mar_bottom <- mar_bottom + 1
  if (!is.na(xlab))
    mar_bottom <- mar_bottom + 1

  mar_left <- 1
  if (showyvalues)
    mar_left <- mar_left + 1
  if (!is.na(ylab))
    mar_left <- mar_left + 1

  mar_top <- 1
  if (hasArg(main))
    mar_top <- mar_top + 1

  mar_right <- 1

  omar_bottom <- 0
  if (!is.null(source))
    omar_bottom <- 1

  # if (hasArg(main)) mar_top <- 2 else mar_top <- 1
  # if (hasArg(xlab)) {
  #   if (showxvalues)
  #     mar_bottom <- 4.2
  #   else
  #     mar_bottom <- 2.2
  # } else {
  #   if (showxvalues)
  #     mar_bottom <- 3
  #   else
  #     mar_bottom <- 1
  # }
  # if (hasArg(ylab)) {
  #   if (showyvalues)
  #     mar_left <- 4.2
  #   else
  #     mar_left <- 2.5
  # } else {
  #   if (showyvalues)
  #     mar_left <- 2
  #   else
  #     mar_left <- 1
  # }
  # mar_right <- 1

  # save current background color and margins
  op<-par(no.readonly=TRUE)
  obg <- par("bg")
  omar <- par("mar")
  ooma <- par("oma")
  # enforce new background color and margins
  # par(bg=bgcolor)
  par(oma=c(omar_bottom,0,0,0) + .1)
  par(mar=c(mar_bottom, mar_left, mar_top, mar_right) + .1)

  xlim <- range(xdata)
  ylim <- range(ydata)
  if (revx) xlim <- rev(xlim)

  plot(xlim, ylim
       , xlim=xlim
       , ylim=ylim
       , type="n"
       , axes=F
       , cex.main = 1
       , col.lab = caim::caim_colors("dark1")
       , cex.lab = 0.9
       , xlab=xlab
       , ylab=ylab
       , ...
       )

  # for (l in 0:(mar_bottom - 1))
  #   mtext(paste0("Line", l), side = 1, line=l)
  # for (l in 0:(mar_left - 1))
  #   mtext(paste0("Line", l), side = 2, line=l)

  # draw plot background area
  rect(par("usr")[1], par("usr")[3], par("usr")[2],
       par("usr")[4],col = bgcolor, border=NA)


  if(showxvalues) {
    if(is.null(xat))
      xat <- axTicks(1)
    xlabels <- xat
    if (xfmt[1]=="pct")
      xlabels <- sprintf(round(100*xat,as.numeric(xfmt[2])),fmt=paste("%1.",xfmt[2],"f%%",sep=""))
    if (xfmt[1]=="dec")
      xlabels <- sprintf(xat,fmt=paste("%1.",xfmt[2],"f",sep=""))
    if (xfmt[1]=="dk") # divided by 1000
      xlabels <- sprintf(round(xat/1000,as.numeric(xfmt[2])),fmt=paste("%1.",xfmt[2],"f",sep=""))
    if (xfmt[1]=="txt")
      xlabels <- xaxistext
    axis(1,tick=FALSE,col.axis=textcolor,at=xat,labels=xlabels, pos=par("usr")[3])
  }

  if (!is.null(source))
    mtext(paste0("  Source: ", source, format(lubridate::now(), ", %B %d, %Y")
), side=1, adj=0, line=0, outer=T, col=textcolor, cex=0.8) # line=1, adj=0, outer=T,

  if (showyvalues) {
    if(is.null(yat))
      yat <- axTicks(2)
    if (!is.null(yfmt)) {
      ylabels <- yat
      if (yfmt[1]=="pct")
        ylabels <- sprintf(round(100*yat,as.numeric(yfmt[2])),fmt=paste("%1.",yfmt[2],"f%%",sep=""))
      if (yfmt[1]=="dec")
        ylabels <- sprintf(yat,fmt=paste("%1.",yfmt[2],"f",sep=""))
      if (yfmt[1]=="dk") # divided by 1000
        ylabels <- sprintf(round(yat/1000,as.numeric(yfmt[2])),fmt=paste("%1.",yfmt[2],"f",sep=""))
      if (yfmt[1]=="txt")
        ylabels <- yaxistext
    }
    axis(2,tick=FALSE,col.axis=textcolor,at=yat,labels=ylabels, pos=par("usr")[1])
  }

  if (showgrid) {
    gridcolor <- caim::caim_colors("white")
    if (!is.null(yat)) {
      for (l in yat)
        abline(h=l,col=gridcolor)
    } else {
      grid(NA,NULL,gridcolor,"solid")
    }
  }
  if (showyaxis) abline(v=0,col=textcolor)
  if (showxaxis) abline(h=0,col=textcolor)

  # par(bg=obg)
  # par(mar=omar)
  # par(oma=ooma)
  # par(op)
}

#' Resets graphics parameters to default
#' @export
reset_graphics <- function() {
  op <- list(
    # adj: justification - 0 = left, 0.5 = center, 1 = right
    adj=0.5
    # ann: if F, plot.default does not annotate w titles and axis titles
    , ann=TRUE
    # ask: if T, requires user interaction before another plot is dragn
    , ask=FALSE
    # bg: background color for entire plot
    , bg="white"
    # bty: box type around plot.
    #   n suppresses
    #   o, 1, 7, c, u, or ] draw boxes corresponding to uppercase letter
    , bty="o"
    # cex: size of plotting text and symbols. Will resize in multi-plots
    , cex=1
    # cex.n: multiplier of cex for specific objects
    , cex.axis=1
    , cex.lab=1
    , cex.main=1.2
    , cex.sub=1
    # col: default plotting color
    , col="black"
    , col.axis="black"
    , col.lab="black"
    , col.main="black"
    , col.sub="black"
    # crt: angle to rotate single characters. Only expect multiples of 90 to work. srt does strings.
    , crt=0
    # err: unimplemented, R is silent when points outside plot are not plotted
    , err=0
    # family: name of a font family for drawing text. "" is default.
    #   standard values are serif, sans and mono
    #   can try font names, but not guaranteed to work on all systems
    , family=""
    # fg: foreground color, for things like axes and boxes
    , fg="black"
    # fig: coordinates of the figure region.
    #   if you set this, you start a new plot, so to add to existing plot use new=T as well
    , fig=c(0, 1, 0, 1)
    # fin: figure dimension in inches, width, height. See fig re: setting this
    , fin=c(7.029070, 5.697674)
    # font: 1=plain, 2=bold, 3=italic, 4=bold italic, 5=symbol
    , font=1
    , font.axis=1
    , font.lab=1
    , font.main=2
    , font.sub=1
    # lab: vector (x y, len) x and y give approximate number of ticks on axis
    #   len is unimplemented in R
    #   this only affects how xaxp and yaxp are set up, and not consulted when axes are drawn
    , lab=c(5, 5, 7)
    # las: style of axis labels. 0=parallel[default], 1=horizontal, 2=perpendicular, 3=vertical
    , las=0
    # lend: line end style 0 and "round" = round[default], 1 and "butt" = butt, 2 and "square"=square
    , lend="round"
    # lheight: line height multiplier for text
    , lheight=1
    # ljoin: line join style. 0 and "round" = round[default], 1 and "mitre", 2 and "bevel"
    , ljoin="round"
    # lmitre: controls when mitred joins automatically convert to bevel. Must be > 1, default=10
    , lmitre=10
    # lty: line type. 0="blank", 1="solid"[default], 2="dashed", 3="dotted", 4="dotdash",
    #   5="longdash", 6="twodash"
    #   Alternatively, a string of up to 8 characters (from c(1:9, "A":"F")) may be given
    #   , giving the length of line segments which are alternatively drawn and skipped
    , lty="solid"
    # lwd: line width
    , lwd=1
    # mai: inner margin in inches, c(bottom, left, top, right)
    , mai=c(1.02, 0.82, 0.82, 0.42)
    # mar: margin in number of text lines, c(bottom, left, top, right)
    , mar=c(5.1, 4.1, 4.1, 2.1)
    # mex: character size expansion factor. starts as 1 when device opens and reset when layout changed
    , mex=1
    # mfcol: vector of c(numrows, numcols) for multi plots
    , mfcol=c(1, 1)
    # mfg: vector of form c(i, j) or c(i, j, numrows, numcols) where i and j indicate which
    #   figure in an array of figures is to be drawn next
    , mfg=c(1, 1, 1, 1)
    # mfrow: vector of c(numrows, numcols) for multi plots
    , mfrow=c(1, 1)
    # mgp: margin line (in mex units) for axis title, axis labels and axis line.
    # , mgp=c(3, 1, 0)
    , mgp=c(1.5, 0.25, 0)
    # mkh: ignored in R
    , mkh=0.001
    # new: T will draw plot on existing frame as it were new
    , new=FALSE
    # oma: outer margins in lines of text, c(bottom, left, top, right)
    , oma=c(0, 0, 0, 0)
    # omd: A vector of the form c(x1, x2, y1, y2) giving the region inside outer margins
    #   in NDC (= normalized device coordinates), i.e., as a fraction (in [0, 1])
    #   of the device region.
    , omd=c(0, 1, 0, 1)
    # oma: outer margins in inches, c(bottom, left, top, right)
    , omi=c(0, 0, 0, 0)
    # pch: integer specifying a symbol or single character to be used as default in plotting points
    #   see http://127.0.0.1:44245/help/library/graphics/help/points
    #   0 = empty square
    #   1 = empty circle
    #   2 = empty triangle
    #   3 = +
    #   4 = x
    #   5 = diamond
    #   6 = empty down triangle
    #   15 = solid square
    #   16 = solid circle
    #   17 = solid triangle
    #   18 = solid diamond
    #   19 = solid larger circle
    #   20 = solid bullet
    #   21 = filled circle
    #   22 = filled square
    #   23 = filled diamond
    #   24 = filled triangle
    #   25 = filled down triangle
    , pch=1
    # pin: current plot dimensions in inches c(width, height)
    , pin=c(5.789070, 3.857674)
    # plt: A vector of the form c(x1, x2, y1, y2) giving the coordinates of the plot region
    #   as fractions of the current figure region.
    , plt=c(0.1166584, 0.9402481, 0.1790204, 0.8560816)
    # point size of text, but not symbols
    , ps=12
    # pty: plot region type: "s"=square, "m"=maximum plotting region
    , pty="m"
    # smo: unimplemented
    , smo=1
    # srt: string rotation in degrees
    , srt=0
    # tck: length of tick marks as fraction of the smaller of the width or heigh of plotting region
    #   if tck >= 0.5 it's interpreted as a fraction of the relevant side
    #   tck=1 = draw gridlines
    #   default = NA = tck=-0.5
    , tck=NA
    # tcl: length of tick marks as a fractoion of height of line of text
    , tcl=-0.5
    # usr: vector of c(x1 ,x2, y1, y2) giving extremes of user coordinates of plotting region
    , usr=c(0, 1, 0, 1)
    # xaxp: A vector of the form c(x1, x2, n) giving the coordinates of the extreme tick marks and
    #   the number of intervals between tick-marks when par("xlog") is false.
    , xaxp=c(0, 1, 5)
    # xaxs: style of axis interval calculation
    #   Style "r" (regular) first extends the data range by 4 percent at each end
    #     and then finds an axis with pretty labels that fits within the extended range.
    #   Style "i" (internal) just finds an axis with pretty labels that fits within the
    #     original data range.
    , xaxs="r"
    # xaxt: x axis type. "s" is standard, "n" suppresses plotting of the axis
    , xaxt="s"
    # xpd: F - plotting clipped to plot region, T - plotting clipped to figure region,
    #   NA - plotting clipped to device region
    , xpd=FALSE
    # xlog: if T, use logarithmic scale
    , xlog=FALSE
    # yaxp: see xaxp, above
    , yaxp=c(0, 1, 5)
    # yaxs: see xaxs, above
    , yaxs="r"
    # yaxt: see yaxt, above
    , yaxt="s"
    # positions text in margins. Device-specific, but 0.2 for R
    , ylbias=0.2
    # ylog: see xlog, above
    , ylog=FALSE
  )
  par(op)
}
