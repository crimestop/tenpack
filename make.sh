#!/bin/bash
version=v4.9
echo Compling Pack $version
while read line; do
	echo
	echo Compling $line
	ary=($line)
	cd source/${ary[0]}/${ary[1]}
		make
		cp *.mod ../../../include
		mv *.a ../../../lib
	cd ../../..
done < versions/${version}.dat
rm -r output/lib_${version}
cp -r lib output/lib_${version}
rm -r output/include_${version}
cp -r include output/include_${version}
