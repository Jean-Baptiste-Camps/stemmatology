\encoding{utf8}
\name{PCC.contam}
\alias{PCC.contam}
\title{
PCC Exploratory Methods: Contamination Detection
}
\description{
Detect possible contaminations by assessing the role of each witness in conflicting information between variant locations.
}
\usage{
PCC.contam(x, omissionsAsReadings = FALSE, alternateReadings = FALSE, pauseAtPlot = FALSE)
}
\arguments{
  \item{x}{
  if \code{alternateReadings = FALSE} (default), a numeric matrix, with witnesses in columns, variant locations in rows, and readings coded by a number;
  if \code{alternateReadings = TRUE}, a character matrix, with witnesses in columns, variant locations in rows, and, in each cell, one or several readings, coded by numbers and separated by a comma (e.g. '1,2,3', if the witness has three different readings);
 or an object of class "pccConflicts" or "pccElimination". 
 }
 \item{omissionsAsReadings}{
     logical; if \code{TRUE}, omissions are treated as variant readings (and taken into account in determining conflicts between variant locations or in computing severe disagreements between witnesses).
Default: \code{FALSE}.}
  \item{alternateReadings}{
  logical; if \code{TRUE}, a witness can have multiple variants for a single variant location (contaminated manuscripts, editio variorum, …), encoded as comma-separated values.
  Default: \code{FALSE}.}
  \item{pauseAtPlot}{logical; if \code{TRUE}, the algorithms stops at each plot during the execution of \code{PCC.contam} (by setting graphical parameter \code{ask = TRUE}). Default: \code{FALSE}.}
}
\details{
To help assess the role of each witness in the conflicting information between variant locations, this function computes the number of conflicting variant locations when removing one of the witnesses, for each witness. If removing a witness makes the number of conflicting variant locations significantly drop, then contamination can be considered as plausible.
Be aware that this function will be most efficient for contaminations limited to a single manuscript.
}
\value{
  An object of class "pccContam", a list containing
  \item{totalByMs}{a numeric matrix, with, in rows, each variant locations, and,   in columns, the number of conflicts and centrality in the full database, followed by the difference in total conflicts and centrality caused by the removal of each witness.}
  \item{conflictsDifferences}{a one row numeric matrix, containing, for each
  witness, the total decrease in conflicts caused by its removal from the
  computations}
  \item{database}{the original database used for the calculations}
}
\references{
Camps, Jean-Baptiste. ‘Detecting Contaminations in Textual Traditions
Computer Assisted and Traditional Methods’.  Leeds, International Medieval Congress, 2013, unpublished paper, \url{https://www.academia.edu/3825633/Detecting_Contaminations_in_Textual_Traditions_Computer_Assisted_and_Traditional_Methods}.
}
\author{
Jean-Baptiste Camps & Florian Cafiero
}
\note{
Additional contamination detection methods will be implemented in the future.
}

\section{Warning }{The execution of this command can be time-consuming for large databases.}

\seealso{
\code{\link{PCC.Exploratory}}, \code{\link{PCC.equipollent}}.
}
\examples{
# load a data set
data("fournival")
\dontshow{
# subsetting for exec time
fournival = fournival[1:200,c(-5,-8)]
}
# identify conflicts on a subset
x = PCC.conflicts(fournival)
# identify problematic variant locations
x = PCC.overconflicting(x, ask = FALSE, threshold = 0.06)
# eliminate them
x = PCC.elimination(x)
# examinate the rest of the problematic cases, to detect
# plausible contaminations
PCC.contam(x)
}
\keyword{stemmatology}
