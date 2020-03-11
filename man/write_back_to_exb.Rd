% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_back_to_exb.R
\name{write_back_to_exb}
\alias{write_back_to_exb}
\title{Write_back_to Exb
writes back changes in annotations to an exb}
\usage{
write_back_to_exb(
  CsvFile,
  sep = ",",
  PathExb,
  PathNewFile = PathExb,
  suffix = "_new"
)
}
\arguments{
\item{CsvFile}{File or object with data.frame as created by read_exb_file or read_exb_dir}

\item{sep}{seperator in Csv file}

\item{PathExb}{Path of the ExbFile}

\item{PathNewFile}{Directory where the new file is saved}
}
\value{

}
\description{
Write_back_to Exb
writes back changes in annotations to an exb
}