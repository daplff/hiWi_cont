#!/usr/bin/python

from Scientific.IO import NetCDF as nf

import vtk
from vtk.util import numpy_support
import sys

####################################################################
# read from dataset
if len(sys.argv)<2:
	print("Usage: ", sys.argv[0], "<NetCDF-filename> \n")
	exit(1)
filename = sys.argv[1]
read_file = nf.NetCDFFile(filename, 'r')
#print read_file.dimensions

particleNo = read_file.dimensions["particleNo"]
timeDimension = read_file.dimensions["time"]

xDimension = read_file.variables["xpos"]
yDimension = read_file.variables["ypos"]

allvars = read_file.variables.keys()

####################################################################
 
# 1. create vtkUnstructuredGrid
totalGrid = vtk.vtkUnstructuredGrid()
gridPts = vtk.vtkPoints()
gridPts.SetNumberOfPoints(particleNo)



# 2. load values for grid
for partId in range(0,particleNo):
	#first variable index: time
	#i'll change z to the depth i think	
	gridPts.SetPoint(partId, xDimension[0,partId],yDimension[0,partId], 0) 

	
totalGrid.SetPoints(gridPts)

# 3. create variables in VTK form and add to dataset directly
#variablesList = []
for variable in allvars:
	tempVariable = read_file.variables[variable]
#	tempVar2 = tempVariable[:]
#	print (variable, len(tempVar2), tempVar2[0,:])
#	if len(tempVariable[:])==particleNo*timeDimension:
	if tempVariable.shape==(timeDimension,particleNo):
		#print('awww yiesssss')
#		temp = vtk.vtkFloatArray()
#		temp.SetNumberOfComponents(1)
#		temp.SetNumberOfTuples(particleNo)
		#make deep copy using vtk util - will give garbage if contents are garbage collected otherwise!!!
		temp = numpy_support.numpy_to_vtk(tempVariable[0,:],deep=1, array_type = vtk.VTK_FLOAT)
		#print(temp.GetDataSize())
		temp.SetName(variable)
		totalGrid.GetPointData().AddArray(temp)

# 4. write unstructured vtk grid from appendFilter
writer = vtk.vtkUnstructuredGridWriter()
writer.SetFileTypeToBinary()
writer.SetInput(totalGrid)
writer.SetFileName(filename+".vtk")
writer.Write()


# close data set
read_file.close()

