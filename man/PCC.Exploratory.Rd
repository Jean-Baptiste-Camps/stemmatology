\name{PCC.Exploratory}
\alias{PCC.Exploratory}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PCC.Exploratory(x, omissionsAsReadings = FALSE, pauseAtPlot = FALSE, interactive = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{omissionsAsReadings}{
%%     ~~Describe \code{omissionsAsReadings} here~~
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
function (x, omissionsAsReadings = FALSE, pauseAtPlot = FALSE, 
    interactive = TRUE) 
{
    tableVariantes = x
    pccConflicts = PCC.conflicts(tableVariantes, omissionsAsReadings = omissionsAsReadings)
    answered = FALSE
    while (answered == FALSE) {
        answerOne = readline("Do you want to proceed to the analysis of the network conflictuality ? Y/N \n ")
        if (answerOne != "N" && answerOne != "Y") {
            print("Please enter Y (yes) or N (no).")
        }
        if (answerOne == "N") {
            return(pccConflicts)
        }
        if (answerOne == "Y") {
            answered = TRUE
        }
    }
    answered = FALSE
    while (answered == FALSE) {
        pccElimination = PCC.elimination(pccConflicts)
        writeLines("Are you satisfied with this configuration and do you want to\n Proceed to actual elimination of over-conflicting variant locations [P],\n  Try again with different value [T],\n or Quit [Q] ? \n ")
        reiterateQuestion = TRUE
        while (reiterateQuestion == TRUE) {
            answerOne = readline("(P/T/Q)")
            if (answerOne != "P" && answerOne != "T" && answerOne != 
                "Q") {
                print("Please enter P (Proceed), T (Try again) or Q (Quit).")
            }
            if (answerOne == "T") {
                reiterateQuestion = FALSE
            }
            if (answerOne == "Q") {
                return(pccElimination)
            }
            if (answerOne == "P") {
                reiterateQuestion = FALSE
                answered = TRUE
            }
        }
    }
    pccDoElimination = PCC.doElimination(pccElimination)
    pccConflicts = PCC.conflicts(pccDoElimination, omissionsAsReadings = omissionsAsReadings)
    if (sum(pccConflicts$conflictsTotal[, 1]) == 0) {
        print("There is no longer any conflict in the database. Function will stop.")
        return(pccConflicts)
    }
    message = paste("There is still ", nrow(pccConflicts$edgelist), 
        "conflicts in the database. If this number is high it might indicate that : \n 1. The conflictuality index chosen was too low. \n 2. There are concurring structures in the tradition (due for instance to contamination).")
    writeLines(message)
    answered = FALSE
    while (answered == FALSE) {
        answerOne = readline("Do you want to proceed to contamination detection methods ? Y/N \n ")
        if (answerOne != "N" && answerOne != "Y") {
            print("Please enter Y (yes) or N (no).")
        }
        if (answerOne == "N") {
            return(pccConflicts)
        }
        if (answerOne == "Y") {
            answered = TRUE
        }
    }
    pccContam = PCC.contam(pccConflicts, pauseAtPlot = pauseAtPlot, 
        omissionsAsReadings = omissionsAsReadings)
    print(pccContam$conflictsDifferences)
    answered = FALSE
    while (answered == FALSE) {
        answerOne = readline("Do you want to define alternate configurations for stemma building ? Y/N \n ")
        if (answerOne != "N" && answerOne != "Y") {
            print("Please enter Y (yes) or N (no).")
        }
        if (answerOne == "N") {
            return(pccContam)
        }
        if (answerOne == "Y") {
            answered = TRUE
        }
    }
    pccEquipollent = PCC.equipollent(pccConflicts)
    return(pccEquipollent)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
