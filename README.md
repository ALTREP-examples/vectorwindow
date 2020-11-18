# vectorwindow
An Example ALTREP package which implements vectors as windows/views on other vectors without duplication.

A simple ALTREP package developed for presentation to the Bioconductor Developer Forum on Nov 19, 2020.

This package impements (currently only for REALSXP vectors) the concept of constructing a view into a 
contiguous range of elements within the data of another existing vector (e.g., elements 100-199 of a 
vector with 1M elements).
