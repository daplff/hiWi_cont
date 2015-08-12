/*
 * netcdf_reader.cpp
 *
 *  Created on: 12 Aug 2015
 *      Author: erik
 */

#include <cstdio>
#include <string>
#include <iostream>
#include <netcdf.h>
#include <cstdlib>
#include <vector>

void write_vtkHeader( FILE *fp, int no_of_points)
{
	if( fp == NULL )
	{
		char szBuff[80];
		sprintf( szBuff, "Null pointer in write_vtkHeader" );
		std::cerr<<( szBuff );
		return;
	}

	fprintf(fp,"# vtk DataFile Version 2.0\n");
	fprintf(fp,"generated from a netcfd file (written by Erik Wannerberg) \n");
	fprintf(fp,"ASCII\n");
	fprintf(fp,"\n");
	fprintf(fp,"DATASET UNSTRUCTURED_GRID\n");
	fprintf(fp,"POINTS %i float\n", no_of_points );
	fprintf(fp,"\n");
}

void write_vtkPointcoords(FILE *fp, std::vector<double> xCoords, std::vector<double> yCoords, int no_of_points)
{
	for (int i=0; i<no_of_points; ++i)
	{
		fprintf(fp,"%g\t%g\t0\n", xCoords[i],yCoords[i]);

	}
	fprintf(fp,"\n");
}

void write_vtkScalars(FILE *fp, char * dataName, std::vector<double> scalarVec, int no_of_points)
{

	fprintf(fp,"SCALARS %s float\n",dataName);
	fprintf(fp,"LOOKUP_TABLE default\n");

	for (int i=0; i<no_of_points; ++i)
	{
		fprintf(fp,"%g\n", scalarVec[i]);

	}
	fprintf(fp,"\n");

}

bool nc_do(int netcdf_fn_output)
{
	if(netcdf_fn_output != NC_NOERR)
	{
		std::cerr << " Error string: " << nc_strerror(netcdf_fn_output) << std::endl;
		exit(1);
		return false;
	}
	return true;

}

int main(int argc, char * argv[])
{
	if (argc<7)
	{
		std::cout<<"Usage: <command> filename_in filename_out_base xdimvarname ydimvarname timedimname particledimname\n Try again!\n";
		return 1;
	}

	std::string inputFileName(argv[1]);
	std::string outputFileNameBase(argv[2]);
	std::string xDimVarName(argv[3]);
	std::string yDimVarName(argv[4]);
	std::string timeDimName(argv[5]);
	std::string particleDimName(argv[6]);


	int ncFileId;
	int nDims,nVars,nAttr;
	int xDimVarId, yDimVarId, timeDimId, timeCoordVarId, particleDimId;

	nc_do(nc_open(inputFileName.c_str(), NC_NOWRITE, &ncFileId));

	nc_do(nc_inq(ncFileId,&nDims,&nVars,&nAttr,NULL));

	//get dimlengths
	std::vector<size_t> dimLengths(nDims);
	for (int i = 0; i<nDims; ++i)
	{
		nc_do(nc_inq_dimlen(ncFileId,i,dimLengths.data()+i));
	}

	//get x&y&time ids
	nc_do(nc_inq_varid(ncFileId,xDimVarName.c_str(),&xDimVarId));
	nc_do(nc_inq_varid(ncFileId,yDimVarName.c_str(),&yDimVarId));
	nc_do(nc_inq_varid(ncFileId,timeDimName.c_str(),&timeCoordVarId));
	nc_do(nc_inq_dimid(ncFileId,timeDimName.c_str(),&timeDimId));
	nc_do(nc_inq_dimid(ncFileId,particleDimName.c_str(),&particleDimId));


	//for every time write out all xy pos
	size_t timestep = 0;
	for (timestep = 0; timestep<dimLengths[timeDimId]; ++timestep)
	{
		char timestepStringAddition [4];
		sprintf(timestepStringAddition,"%04u",static_cast<unsigned int>(timestep));
		std::string outputFileName = (outputFileNameBase+timestepStringAddition) + ".vtk";
		FILE* filePointer = fopen(outputFileName.c_str(), "w");
		if( filePointer == NULL )
		{
			std::cerr<< "Failed to open"<< outputFileName << std::endl;
			return 1;
		}

		write_vtkHeader(filePointer,dimLengths[particleDimId]);
		std::vector<double> xCoords(dimLengths[particleDimId]), yCoords(dimLengths[particleDimId]);

		//assume for now that time is the first dimension, particle no second for all variables

		size_t startIndices[2] = {timestep,0};
		size_t readLengths[2] = {1,dimLengths[particleDimId]};

		nc_do(nc_get_vara_double(ncFileId,xDimVarId,startIndices,readLengths,xCoords.data()));
		nc_do(nc_get_vara_double(ncFileId,yDimVarId,startIndices,readLengths,yCoords.data()));

		write_vtkPointcoords(filePointer,xCoords,yCoords,dimLengths[particleDimId]);


		std::vector<double> tempVar(dimLengths[particleDimId]);
		if(nVars>3)
		{
			fprintf(filePointer, "\n");
			fprintf(filePointer, "POINT_DATA %i \n", static_cast<int>(dimLengths[particleDimId]) );

			for (int var_index = 0; var_index < nVars; ++var_index)
			{
				if(var_index == xDimVarId || var_index == yDimVarId || var_index == timeCoordVarId)
					continue;


				char variableName[NC_MAX_NAME + 1];

				nc_do(nc_inq_varname(ncFileId,var_index,variableName));
				nc_do(nc_get_vara_double(ncFileId,var_index,startIndices,readLengths,tempVar.data()));

				write_vtkScalars(filePointer,variableName,tempVar,dimLengths[particleDimId]);
			}
		}

		fclose(filePointer);
	}

	return 0;
}
