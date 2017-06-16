\name{PCC}
\alias{PCC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PCC (Poole-Camps-Cafiero) stemmatological method}
\description{%
    Global shell for all the PCC functions, both exploratory and stemma-building. This command successively executes PCC.Exploratory and PCC.Stemma, while asking user for input when necessary.
}
\usage{
PCC(x, omissionsAsReadings = FALSE, alternateReadings = FALSE, limit = 0, recoverNAs = TRUE, pauseAtPlot = FALSE, interactive = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  if \code{alternateReadings = FALSE} (default), a numeric matrix, with witnesses in columns, variant locations in rows, and readings coded by a number;
  if \code{alternateReadings = TRUE}, a character matrix, with with witnesses in columns, variant locations in rows, and, in each cell, one or several readings, coded by numbers and separated by a comma (e.g. '1,2,3', if the witness has three different readings). 
}
  \item{omissionsAsReadings}{
     logical; if \code{TRUE}, omissions are treated as variant readings (and taken into account in determining conflicts between variant locations or in computing severe disagreements between witnesses); if \code{FALSE}, they are treated only as omissions.  Default: \code{FALSE}.
}
  \item{alternateReadings}{
  logical; if \code{TRUE}, a witness can have multiples (> 1) variants for a single variant location (contaminated manuscripts, editio variorum, …), encoded as comma-separated values; if \code{FALSE}, only one reading per witness is allowed. Default: \code{FALSE}.
  }
  \item{limit}{The maximum number of severe disagreements expected for witnesses to be in the same group. Default: \code{0}.
  %%%TODO(JBC): Implémenter cette option !!!
}
  \item{recoverNAs}{
  logical; if \code{TRUE}, when an actual witness or reconstructed subarchetype is identified to the reconstructed model of a group, every NA it has is recovered by taking the value of the reconstructed model; if \code{FALSE}, their NAs values are kept. Default: \code{FALSE}.
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
%%TODO JBC: donner plus d'éléments dès ici


The option \code{recoverNAs=TRUE} is a novelty not described in the original paper (Camps & Cafiero 2015). 

To understand the underlying principles behind all the functions from this method, particulary the distinction between severe and bening disagreement, the different status given to readings, omissions and \emph{lacunae}, the notion of conflict between variant locations or the way the stemma is builded, see the \emph{references} section. 
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Jean-Baptiste Camps et Florian Cafiero, « Genealogical variant locations and simplified stemma: a test case », in \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, dir. Tara Andrews & Caroline Macé, Turnhout, 2015, p. 69‑93 (Lectio, 1).

Eric Poole, « The Computer in Determining Stemmatic Relationships », \emph{Computers and the Humanities}, 8-4 (1974), p. 207‑16.

Eric Poole, « L’analyse stemmatique des textes documentaires », in \emph{La pratique des ordinateurs dans la critique des textes}, Paris, 1979, p. 151‑161
}
\author{
Jean-Baptiste Camps & Florian Cafiero
}
\note{
%%%
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
