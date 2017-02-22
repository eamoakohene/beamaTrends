\name{beamaTrends}
\alias{beamaTrends}
\title{Describe growth trends of timeseries object}
\usage{
   beamaTrends::td$new(  x, x_name = NULL, x_start=NULL, x_frq = NULL )$get_growth_desc()
}
\arguments{
    \item{x}{ timeseries object, dataframe, vector or db \cr
             \code{timeseries} -  object only needs to specify x \cr
             \code{dataframe}  - should have columns \code{yr} , \code{mth}, and \code{value} \cr
             \code{vector} - should specify both \code{x_start} in the form \code{c(year, month)} and also \code{x_frq} \cr
             \code{db} - for internal use only
           }

    \item{x_name}{ name of the timeseries object x}

    \item{x_start}{ the start period of the series}

    \item{x_frq}{frequency of the series}
}

\description{

}
\examples{

#dataframe
set.seed( 100 )
abc <- beamaTrends::td$new(
      x = data.frame(
        yr = rep( 2010:2015 , each = 12 ),
        mth = rep( 1:12, times=6 ),
        value = rnorm( 72, mean = 10, sd = 1)
      ),
      x_name = 'random'
  )

abc$get_mtm()$desc
abc$get_successive( is_yoy = FALSE )
abc$get_yoy()$desc
abc$get_successive( is_yoy = TRUE)
abc$get_growth_desc()

#timeseries object
set.seed( 100 )
my_ts = stats::ts(rnorm( 72, mean = 10, sd = 1), start = c(2010,1), frequency = 12)
beamaTrends::td$new( my_ts, x_name = 'random' )$get_growth_desc()


#vector
set.seed( 100 )
my_vector = c( rnorm( 72, mean = 10, sd = 1) )
beamaTrends::td$new(
  my_vector, x_name = 'random',
  x_start = c(2010,1), x_frq = 12
)$get_growth_desc()

### ouput ###
#Between November 2015 and December 2015 random fell by -14.5\%.
#Random fell by -2.5\% in the year to December 2015, up from -8.2\%
#in November 2015. It is the 3rd successive year on year decline.
}
