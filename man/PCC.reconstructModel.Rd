\encoding{utf8}
\name{PCC.reconstructModel}
\alias{PCC.reconstructModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PCC.reconstructModel
}
\description{
\code{PCC.reconstructModel} examines coherent clusters of witnesses (PCC.buildGroup), to either identify their model in the tradition, either suggest a reconstructed model for the group.  
}
\usage{
PCC.reconstructModel(x, omissionsAsReadings = FALSE, recoverNAs = TRUE, ask = TRUE, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{x}{
A PCCbuildGroup object.
}
  \item{omissionsAsReadings}{
     logical; if \code{TRUE}, omissions are treated as variant readings. They are taken into account in determining conflicts between variant locations or in computing severe disagreements between witnesses).
     Default: \code{FALSE}.
}
 \item{recoverNAs}{
  logical; if \code{TRUE}, when an actual witness or reconstructed subarchetype is identified to the reconstructed model of a group, every NA it has is recovered by taking the value of the reconstructed model; if \code{FALSE}, their NAs values are kept. 
  Default: \code{TRUE}.
}
 \item{ask}{
  logical; if FALSE, decisions will be made without asking the user for input.
  Default: TRUE
  }
  \item{verbose}{
  logical; if FALSE, the function will only return the results, without information
  on the operations.
  Default: FALSE
  }
}

\details{
This function takes \code{\link{PCC.buildGroup}} objects as input. It assesses the characteristics of the model of each group, and compares it to the existing witnesses. If a witness has the same characteristics as the computed model, it is identified as the model for the group. If no witness seems to be a good fit, the function adds a reconstructed model to the tradition.
}

\value{
The function returns a list containing 
  \item{fullDatabase}{The full database, with the new reconstructed models 
  and recovered NAs (if applicable).}
  \item{database}{The same with the \emph{descripti} removed.}
  \item{edgelist}{An edgelist expressing the relations between the witnesses of each group.}
  \item{models}{A list containing the database of readings for each model.}
  \item{modelsByGroup}{A matrix with, in columns the groups, and a single row containing the label of their model.}

}
\references{
Jean-Baptiste Camps et Florian Cafiero, « Genealogical variant locations and simplified stemma: a test case », in \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, dir. Tara Andrews & Caroline Macé, Turnhout, 2015, p. 69‑93 (Lectio, 1), \url{http://halshs.archives-ouvertes.fr/halshs-01435633/}.

Eric Poole, « L’analyse stemmatique des textes documentaires », in \emph{La pratique des ordinateurs dans la critique des textes}, Paris, 1979, p. 151‑161

Eric Poole, « The Computer in Determining Stemmatic Relationships », \emph{Computers and the Humanities}, 8-4 (1974), p. 207‑16.
}
\author{
Jean-Baptiste Camps (\email{jbcamps@hotmail.com}) & Florian Cafiero
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{

}

\seealso{
\code{\link{PCC.Stemma}}, \code{\link{PCC.disagreement}}, \code{\link{PCC.buildGroup}}.
}
