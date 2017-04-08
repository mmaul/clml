<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Package: <code>clml.r-datasets</code></a></li>
</ul>
</div>
</div>



# Package: `clml.r-datasets`

-   Uses:
    common-lisp, drakma, clml.utility.data, clml.hjs.read-data
-   Used by:
    clml.data

## Description

## Description

Makes datasets included with the R language distribution available as clml datasets.
R datasets are obtained csv files on Vincent Centarel's github repository.
More information on these datasets can be found at <http://vincentarelbundock.github.com/Rdatasets>

Because type information is not included it may be necessary to provide a type specification
for the columns in the csv file.

    (ql:quickload :clml.data.r-datasets)
    (in-package :clml.data.r-datasets)
    (defparameter dd (get-r-dataset-directory))
    (inventory dd)
      Package                   Item                      Title                     
      ------------------------- ------------------------- ------------------------- 
      datasets                  AirPassengers             Monthly Airline Passenger Numbers 1949-1960 
      datasets                  BJsales                   Sales Data with Leading Indicator 
      datasets                  BOD                       Biochemical Oxygen Demand 
    (dataset-documentation  dd  "datasets" "BOD")
      R: Biochemical Oxygen Demand
      BODR Documentation
      Biochemical Oxygen Demand
        Description
          The BOD data frame has 6 rows and 2 columns giving the
          biochemical oxygen demand versus time in an evaluation of water
          quality.
          ...
    
    (get-dataset dd "datasets" "BOD")
      #<UNSPECIALIZED-DATASET >
      DIMENSIONS:  | Time | demand
      TYPES:      UNKNOWN | UNKNOWN | UNKNOWN
      NUMBER OF DIMENSIONS: 3
      DATA POINTS: 6 POINTS

## Other uses

This package can also be used as a tool for sharing or distributing bundles of datasets.
To do this a csv file which provides the directory of data sets must be made availabe
via a URL. The csv file MUST comply to the following format:
A header with following collumns
-   Package : package
-   Item    : dataset name
-   Title   : Brief Description of dataset
-   csv     : URL where dataset is available
-   doc     : URL with documentation describing the dataset

The the contents of the file pointed to by doc `doc` can be plaintext of HTML.
If it is HTML the HTML tags will be stripped and what ever whitespace formatting will
be used. This field can be empty however the `inventory` method will be un available if it is

## External Symbols

### External Functions

---

#### Inherited Function: `dataset-documentation`

##### Syntax

    (dataset-documentation dataset-directory package name &key stream (stream t))

##### Description

Outputs documention for the R dataset to the specified stream if no stream supplied defaults to console
-return: <unspecialized-dataset> 
-arguments:
  -package: <string>  package
  -name: <string> dataset name
  -stream: <string> <key> <optional> specify output stream for documentation 

---

#### Inherited Function: `get-dataset`

##### Syntax

    (get-dataset dataset-directory package name &key csv-type-spec (csv-header-p t)
                 (missing-value-check t))

##### Description

Returns the dataset specified by the `package` and `name`
-return: <unspecialized-dataset> 
-arguments:
  -package: <string>  package
  -name: <string> dataset name
  -csv-type-spec: <list> column type specifier list
  -csv-header-p: <bool default t> if true first line of CSV is header
  -missing-values-check <boolean default t> Check for presence of missing values

Returns unspecialized dataset containing contents of CSV specified by the package and name.
Because type information is not included in the CSV nor in the dataset directory it will
probably in most cases be necessary to specify `csv-type-spec`. If `csv-type-spec` is not
set the columns wil be read as strings. You could of course manipulate the dataset later.
It is in most cases better to specify the `csv-type-spec`. The `csv-type~spec` should be a
list containing one of the follwong symbols:
  keyword symbol pathname integer double-float single-float float 
  number t nil string
The values t or nil in the spec will cause the column to be interpeted as a string.

Missing values are defined by a column value of NA or the empty string. Missing values will
cause the value to be represented by the keyword :na in the dataset. For R datasets it is
not necessary to set `csv-header-p` `missing-values-check`.

Example:

    ; grab the data and see what the types should be
    CL-USER> (head-points (get-dataset dd "datasets" "BOD"))
    #(#("1" "1" "8.3") #("2" "2" "10.3") #("3" "3" "19") #("4" "4" "16")
      #("5" "5" "15.6") #("6" "7" "19.8"))
    ; Looks like '(integer integer double-float) will do
    CL-USER> (head-points (get-dataset dd "datasets" "BOD" :csv-type-spec '(integer integer double-float)))
    #(#(1 1 8.3) #(2 2 10.3) #(3 3 19.0) #(4 4 16.0) #(5 5 15.6))

---

#### Inherited Function: `get-r-dataset-directory`

##### Syntax

    (get-r-dataset-directory &optional (url))

##### Description

-returns: <dataset-directory> object containg directory of available R datasets
-   arguments:
    -url: <string> Optional URL containing the location of the R dataset directory. Only needed if a custom directory is needed.

---

#### Inherited Function: `inventory`

##### Syntax

    (inventory dataset-directory &key stream (stream t))

##### Description

Outputs R packages, datasets and description available datasets in inventory
-return: nil
-arguments:
  -package: <dataset-directory>  datasets
  -stream: <string> <key> <optional> specify output stream for documentation
