#!/bin/bash
rm lib/* 
rm include/* 
while read line; do
	rm source/${line}/v*/*.a
	rm source/${line}/v*/*.o
	rm source/${line}/v*/*.mod
done < components.dat
