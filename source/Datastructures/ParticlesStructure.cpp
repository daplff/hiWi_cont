#include "ParticlesStructure.h"
#include <boost/property_tree/ptree.hpp>

template<typename DataType>
void extractArrayFromParentNode (const boost::property_tree::ptree& arrayParent, std::vector<DataType>& inVector)
{
	for(auto it = arrayParent.begin(); it != arrayParent.end(); it++)
	{
		inVector.push_back( (it->second).get_value <DataType> () );
	}
}


ParticlesStructure::ParticlesStructure()
{	
	
}

ParticlesStructure::ParticlesStructure(const boost::property_tree::ptree& input)
{
	extractArrayFromParentNode(input.get_child("xposArray"),xposArray);
	extractArrayFromParentNode(input.get_child("yposArray"),yposArray);
}

void ParticlesStructure::resizeArrays(size_t newSize)
{
	xposArray.resize(newSize);
	yposArray.resize(newSize);
	xvelArray.resize(newSize);
	yvelArray.resize(newSize);
	surfElevationArray.resize(newSize);
	areaArray.resize(newSize);
	smoothlenArray.resize(newSize);
	flagArray.resize(newSize);
}

ParticlesStructure::~ParticlesStructure()
{
}

