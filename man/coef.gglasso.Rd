% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tools.R
\name{coef.gglasso}
\alias{coef.gglasso}
\title{get coefficients or make coefficient predictions from an "gglasso" object.}
\usage{
\method{coef}{gglasso}(object, s = NULL, ...)
}
\arguments{
\item{object}{fitted \code{\link{gglasso}} model object.}

\item{s}{value(s) of the penalty parameter \code{lambda} at which
predictions are required. Default is the entire sequence used to create the
model.}

\item{\dots}{not used. Other arguments to predict.}
}
\value{
The coefficients at the requested values for \code{lambda}.
}
\description{
Computes the coefficients at the requested values for \code{lambda} from a
fitted \code{\link{gglasso}} object.
}
\details{
\code{s} is the new vector at which predictions are requested. If \code{s}
is not in the lambda sequence used for fitting the model, the \code{coef}
function will use linear interpolation to make predictions. The new values
are interpolated using a fraction of coefficients from both left and right
\code{lambda} indices.
}
\examples{

# load gglasso library
library(gglasso)

# load data set
data(colon)

# define group index
group <- rep(1:20,each=5)

# fit group lasso
m1 <- gglasso(x=colon$x,y=colon$y,group=group,loss="logit")

# the coefficients at lambda = 0.01 and 0.02
coef(m1,s=c(0.01,0.02))

}
\references{
Yang, Y. and Zou, H. (2015), ``A Fast Unified Algorithm for
Computing Group-Lasso Penalized Learning Problems,'' \emph{Statistics and
Computing}. 25(6), 1129-1141.\cr BugReport:
\url{https://github.com/archer-yang-lab/gglasso}\cr
}
\seealso{
\code{\link{predict.gglasso}} method
}
\author{
Yi Yang and Hui Zou\cr Maintainer: Yi Yang <yi.yang6@mcgill.ca>
}
\keyword{models}
\keyword{regression}
