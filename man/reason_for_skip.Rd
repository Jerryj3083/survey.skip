\name{reason_for_skip}
\alias{reason_for_skip}

\title{Find the reason a qusetion was skipped
}
\description{
This function uses a specific NA skipped response in the survey data, goes backwards into the skip logic and finds the reason that question was skipped by specifying the question and answer that caused the skip.
}
\usage{
reason_for_skip(row, question, data)
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
reason_for_skip returns an object of class Skip_reason consisting of three components

	\item{Question}{The qustion name of interest}
	\item{Reason}{The qustion that caused the skip}
	\item{Answer}{The answer to reason that caused the skip}	
}

\author{
Jerry Jiang
}
\keyword{list}