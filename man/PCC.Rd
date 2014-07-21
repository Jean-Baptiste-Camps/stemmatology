\name{PCC}
\alias{PCC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
    Global shell for all the PCC functions, both exploratory and stemma-building. Successively executes PCC.Exploratory and PCC.Stemma, while asking user for input when necessary.
}
\usage{
PCC(x, omissionsAsReadings = FALSE, limit = 0, recoverNAs = TRUE, pauseAtPlot = FALSE, interactive = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{omissionsAsReadings}{
%%     ~~Describe \code{omissionsAsReadings} here~~
}
  \item{limit}{
%%     ~~Describe \code{limit} here~~
}
  \item{recoverNAs}{
%%     ~~Describe \code{recoverNAs} here~~
}
  \item{pauseAtPlot}{
%%     ~~Describe \code{pauseAtPlot} here~~
}
  \item{interactive}{
%%     ~~Describe \code{interactive} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x, omissionsAsReadings = FALSE, limit = 0, recoverNAs = TRUE, 
    pauseAtPlot = FALSE, interactive = TRUE) 
{
    pccExploratory = PCC.Exploratory(x, omissionsAsReadings = omissionsAsReadings, 
        pauseAtPlot = pauseAtPlot, interactive = interactive)
    if (is.matrix(pccExploratory)) {
        output = pccStemma(pccExploratory)
    }
    else {
        if (!is.matrix(pccExploratory)) {
            if (class(pccExploratory) == "pccConflicts" | class(pccExploratory) == 
                "pccElimination" | class(pccExploratory) == "pccContam") {
                output = PCC.Stemma(pccExploratory$database, 
                  limit = limit, recoverNAs = recoverNAs)
            }
            else {
                if (class(pccExploratory) == "pccEquipollentDatabases") {
                  output = as.list(NULL)
                  for (i in 1:length(pccExploratory)) {
                    pccStemma = PCC.Stemma(pccExploratory[[i]])
                    legend("topright", paste("Alternative stemma", 
                      i, "out of", length(pccExploratory)))
                    if (i < length(pccExploratory)) {
                      readline("Press enter to proceed to next alternative stemma")
                    }
                    output[[i]] = pccStemma
                  }
                }
                else {
                  stop("Input is unknown.")
                }
            }
        }
    }
    return(output)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
