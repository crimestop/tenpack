#!/bin/bash
version=v4.7
echo Compling Pack $version
while read line; do
	echo
	echo Compling $line
	pwd
	echo "source/$line"
	if [ -d "source/$line" ]; then	
		cd source/${line}
		make
		cp *.mod ../../include
		mv *.a ../../lib
		cd ../..
	else
		echo $line is not contained in version $version
	fi
done < components.dat
rm -r output/lib_${version}
cp -r lib output/lib_${version}
rm -r output/include_${version}
cp -r include output/include_${version}
