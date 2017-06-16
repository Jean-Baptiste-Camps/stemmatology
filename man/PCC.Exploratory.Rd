\name{PCC.Exploratory}
\alias{PCC.Exploratory}
\title{PCC Exploratory methods}
\description{
This is the global function for exploratory methods of the PCC family.  
It interactively makes use of the lower-level exploratory functions, to assess conflicts between variant locations, eliminate problematic configurations or identify likely contaminations. 
}
\usage{
PCC.Exploratory(x, omissionsAsReadings = FALSE, alternateReadings = FALSE, pauseAtPlot = FALSE, interactive = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  if \code{alternateReadings = FALSE} (default), a numeric matrix, with witnesses in columns, variant locations in rows, and readings coded by a number;
  if \code{alternateReadings = TRUE}, a character matrix, with witnesses in columns, variant locations in rows, and, in each cell, one or several readings, coded by numbers and separated by a comma (e.g. '1,2,3', if the witness has three different readings). 
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
  \item{interactive}{logical; if \code{TRUE} (recommended), the algorithm stops each time user input is needed; if \code{FALSE}, it tries to guess an appropriate value. Default: \code{TRUE}.
}
}
\details{
This function is meant to guide the user through the process of assessing and eliminating unreliable variant locations and/or identify competing genealogies (i.e. contamination), as described in Camps & Cafiero 2015. 

It starts by computing and ploting the network of conflicting variant locations (i.e. variant locations that presents contradictory genealogical information), by calling \code{\link{PCC.conflicts}}, and then interactively aids the user in determining overconflicting variant locations (with \code{\link{PCC.overconflicting}}), eliminating problematic variant locations (\code{\link{PCC.elimination}}), detecting contamination (\code{\link{PCC.contam}}) or creating new databases reflecting competing genealogies (\code{\link{PCC.equipollent}}).
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
Jean-Baptiste Camps (\email{jbcamps@hotmail.com}) & Florian Cafiero
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{PCC.conflicts}}, \code{\link{PCC.overconflicting}}, \code{\link{PCC.elimination}}, \code{\link{PCC.contam}}, \code{\link{PCC.equipollent}}.
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Stemmatology}