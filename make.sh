#!/bin/bash
version=v5.1
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
while read line; do
	if [ -d "source/$line" ]; then	
		cp source/${line}/*.o lib
	fi
done < kernel.dat
ar rc lib/libkernel.a lib/*.o 
rm lib/*.o
rm -r output/TNSG_${version}
mkdir output/TNSG_${version}
cp -r lib output/TNSG_${version}/lib
cp -r include output/TNSG_${version}/include
