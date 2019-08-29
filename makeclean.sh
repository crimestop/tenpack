#!/bin/bash
rm lib/* 
rm include/* 
while read line; do
	rm source/${line}/*.a
	rm source/${line}/*.o
	rm source/${line}/*.mod
done < components.dat
