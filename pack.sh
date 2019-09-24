#!/bin/bash
mkdir pack
cp make.sh pack
cp pack.sh pack
cp components.dat pack
cp make.inc pack
cp makeclean.sh pack
mkdir pack/output
mkdir pack/lib
mkdir pack/include
while read line; do
	mkdir pack/${line}
	cp -r source/${line} pack/${line}/
done < components.dat
rm  pack.zip
zip -r pack.zip pack
rm -r pack
