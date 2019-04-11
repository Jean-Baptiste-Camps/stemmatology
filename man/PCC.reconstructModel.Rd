\encoding{utf8}
\name{PCC.reconstructModel}
\alias{PCC.reconstructModel}
\title{PCC.reconstructModel: Reconstruct the Model of Groups of Witnesses}
\description{
\code{PCC.reconstructModel} examines coherent clusters of witnesses (\code{\link{PCC.buildGroup}}), to either identify their model in the tradition, either suggest a reconstructed model for the group.  
}
\usage{
PCC.reconstructModel(x, omissionsAsReadings = FALSE, recoverNAs = TRUE,
                     ask = TRUE, verbose = FALSE)
}
\arguments{
\item{x}{
%A pccBuildGroup object.
The output of \code{\link{PCC.buildGroup}}.
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
  \item{edgelist}{An edgelist expressing the relations between the witnesses of each group with, as a third column, the distances between witnesses.}
  \item{models}{A list containing the database of readings for each model at the time of their reconstruction (i.e., before they are compared to extant witnesses).}
  \item{modelsByGroup}{A matrix with, in columns the groups, and a single row containing the label of their model.}
}
\references{
Camps, Jean-Baptiste, and Florian Cafiero. ‘Stemmatology: An R Package for the Computer-Assisted Analysis of Textual Traditions’. \emph{Proceedings of the Second Workshop on Corpus-Based Research in the Humanities (CRH-2)}, edited by Andrew U. Frank et al., 2018, pp. 65–74, \url{https://halshs.archives-ouvertes.fr/hal-01695903v1}.

Camps, Jean-Baptiste, and Florian Cafiero. ‘Genealogical Variant Locations and Simplified Stemma: A Test Case’. \emph{Analysis of Ancient and Medieval Texts and Manuscripts: Digital Approaches}, edited by Tara Andrews and Caroline Macé, Brepols, 2015, pp. 69–93, \url{https://halshs.archives-ouvertes.fr/halshs-01435633}, DOI: \href{http://dx.doi.org/10.1484/M.LECTIO-EB.5.102565}{10.1484/M.LECTIO-EB.5.102565}.

Poole, Eric. ‘L’analyse stemmatique des textes documentaires’. \emph{La pratique des ordinateurs dans la critique des textes}, Paris, 1979, p. 151-161.

Poole, Eric, ‘The Computer in Determining Stemmatic Relationships’. \emph{Computers and the Humanities}, 8-4 (1974), p. 207-16.
}
\author{
Jean-Baptiste Camps (\email{jbcamps@hotmail.com}) & Florian Cafiero
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
#A fictional simple tradition
x = list(database = matrix(
    c(
      1,0,1,1,1,1,1,1,
      1,0,1,2,2,2,1,2,
      1,0,0,3,2,1,NA,3,
      2,0,1,4,NA,1,1,1,
      2,1,2,5,2,1,1,4
    ), nrow = 8, ncol = 5,
    dimnames = list(c("VL1","VL2","VL3","VL4","VL5","VL6","VL7","VL8"),
                    c("A","B","C","D","E"))), 
    groups = list(c("A", "B", "C"), c("D", "E")))
#And now, reconstruct the groups
PCC.reconstructModel(x)
}

\seealso{
\code{\link{PCC.Stemma}}, \code{\link{PCC.disagreement}}, \code{\link{PCC.buildGroup}}.
}
