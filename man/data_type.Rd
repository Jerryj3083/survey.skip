\name{data_type}
\alias{data_type}

\title{FInd the type of response
}
\description{
This function uses a specific response the the survey data and finds is response type.
}
\usage{
data_type(row, question, data)
}

\arguments{
  \item{row}{integer, row number of the interested response
}
  \item{question}{string, question name of the interested response
}
  \item{data}{survey_sp, the object obtained from the data_sm function
}
}

\value{
data_type returns a character string of the type.

}

\author{
Jerry Jiang
}
\keyword{classes}