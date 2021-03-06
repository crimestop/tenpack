#!/bin/bash

mkdir doc_files
while read line; do
	if [ -d "source/$line" -a $line != "tnsp" -a $line != "symtnsp" ]; then	
		rsync -a --exclude='main.f90' source/${line}/source/ doc_files
	fi
done < components.dat
ford docs/ford.md
echo '.col-md-8{width:100%;} .col-sm-3{display:none;} footer{font-size:14px;} footer .col-xs-12{width:40%;} footer .col-xs-6{width:30%;} .well{display:none;} .jumbotron{display:none;} body{font-size:170%;} .navbar-brand{font-size:26px;}'>>./docs/document/css/local.css
echo '@media (min-width: 992px){.hidden-lg {display: none !important;}}@media (min-width: 992px){.visible-lg {display: block !important;}}'>>./docs/document/css/bootstrap.min.css
rm -r doc_files
