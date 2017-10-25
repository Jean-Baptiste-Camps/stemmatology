\encoding{utf8}
\name{PCC.contam}
\alias{PCC.contam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PCC Exploratory Methods: Contamination Detection
}
\description{
Supplementary function to detect possible contaminations. It computes the number of conflicting variant locations when removing one of the witnesses, for each witness. If removing a witness makes the number of conflicting variant locations significantly drop, then contamination can be considered as plausible.
}
\usage{
PCC.contam(x, omissionsAsReadings = FALSE, alternateReadings = FALSE, pauseAtPlot = FALSE)
}
\arguments{
  \item{x}{
  if \code{alternateReadings = FALSE} (default), a numeric matrix, with witnesses in columns, variant locations in rows, and readings coded by a number;
  if \code{alternateReadings = TRUE}, a character matrix, with witnesses in columns, variant locations in rows, and, in each cell, one or several readings, coded by numbers and separated by a comma (e.g. '1,2,3', if the witness has three different readings);
 or an object of class "pccConflicts" or "pccElimination". 
 %TODO: tester le comportement de 'alternateReadings' dans ce cas
 }
 \item{omissionsAsReadings}{
     logical; if \code{TRUE}, omissions are treated as variant readings (and taken into account in determining conflicts between variant locations or in computing severe disagreements between witnesses).%; if \code{FALSE}, they are treated only as omissions.  
     Default: \code{FALSE}.
}
  \item{alternateReadings}{
  logical; if \code{TRUE}, a witness can have multiple variants for a single variant location (contaminated manuscripts, editio variorum, …), encoded as comma-separated values. %; if \code{FALSE}, only one reading per witness is allowed. 
  Default: \code{FALSE}.
  }
  \item{pauseAtPlot}{logical; if \code{TRUE}, the algorithms stops at each plot during the execution of \code{PCC.contam} (by setting graphical parameter \code{ask = TRUE}). Default: \code{FALSE}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  An object of class "pccContam", a list containing
  \item{totalByMs}{a numeric matrix, with, in rows, each variant locations, and,   in columns, the total sum of conflicts and centrality index, followed by 
  the sum of conflicts, centrality index and difference to the total sum for the
  removal of each of the witnesses in turn
  } %TODO: faudrait-il simplifier ou modifier la structure?
  \item{conflictsDifferences}{a one row numeric matrix, containing, for each
  witness, the total decrease in conflicts caused by its removal from the
  computations}
  \item{database}{the original database used for the calculations}
}
\references{
Jean-Baptiste Camps et Florian Cafiero, « Genealogical variant locations and simplified stemma: a test case », in \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, dir. Tara Andrews & Caroline Macé, Turnhout, 2015, p. 69‑93 (Lectio, 1), \url{http://halshs.archives-ouvertes.fr/halshs-01435633/}.

Eric Poole, « L’analyse stemmatique des textes documentaires », in \emph{La pratique des ordinateurs dans la critique des textes}, Paris, 1979, p. 151‑161

Eric Poole, « The Computer in Determining Stemmatic Relationships », \emph{Computers and the Humanities}, 8-4 (1974), p. 207‑16.
}
\author{
Jean-Baptiste Camps & Florian Cafiero
}
\note{
%%  ~~further notes~~
}

\section{Warning }{The execution of this command can be time-consuming for large databases.}

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{contamination}
\keyword{detection}% __ONLY ONE__ keyword per line
